open Core_kernel
open Common
open Event
open Family

type timing_t =
| Defer
| Close [@@deriving sexp]

module ClassGifts = struct
  module B = Branch
  module D = Description
  module F = Family
  module H = Holder
  module P = Person
  module E = Event

  let frac n d : Q.t = Q.((of_int n) / (of_int d))

  let sole h : ('a * Q.t) list = [(h, Q.one)]
  let holder_of_p p = H.Person p
  let holderize ps = List.map ps ~f:holder_of_p
  let holderize_shares ps = List.map ps ~f:(fun (p,s) -> (holder_of_p p, s) )
  let equal_shares hs =  List.map hs ~f:(fun h -> (h, frac 1 (List.length hs)))
  let split_among ps = ps |> holderize |> equal_shares
  let scale q l = List.Assoc.map l ~f:(fun s -> s * q)

  let by_primogeniture p h =
    match F.firstborn_living_descendant p h with
      | Some p1 -> sole p1
      | None -> []

  let per_capita p h =
    let rec build_list = List.concat_map ~f:( fun p ->
      if F.is_alive_after p h then [p]
      else F.children_of p h |> build_list ) in
    build_list [p] |> equal_shares

  let per_stirpes p h =
    let rec build_list = List.concat_map ~f:( fun (p,s) -> 
      if F.is_alive_after p h then [(p,s)]
      else F.children_of p h |> equal_shares |> Shares.scale s |> build_list) in
    build_list (sole p)

  let by_representation p h  =
    let rec build_list = function 
      | [] -> []
      | ps -> 
        let (living,dead) = List.partition_tf ps ~f:(fun (p,_) -> F.is_alive_after p h ) in
        let next_generation = List.concat_map dead ~f:(fun (p,_) -> F.children_of p h) |> equal_shares in
        living @ (Shares.scale (Shares.total dead) next_generation |> build_list) in
    build_list (sole p)
  
  let descendants p (div:branch) h: holder shares = 
  ( match div with
    | Primogeniture    -> by_primogeniture p h
    | PerCapita        -> per_capita p h
    | PerStirpes       -> per_stirpes p h
    | ByRepresentation -> by_representation p h )
  |> holderize_shares

  let intestate_heirs (p:person) h: holder shares  =
    match F.widow_of p h with 
    | Unmarried -> 
      begin 
          match descendants p !Parameters.intestacy_branch_type h with 
          | [] -> if !Parameters.imply_unspecified_heirs 
               then sole ((H.Other (Heirs p)))
               else sole ((H.Other (Escheat)))
          | ps -> ps
      end
    | Unknown -> sole ((H.Other (Widow p)))
    | Known s -> sole ((H.Person s))

  let filter_by_restriction r h = function 
  | (H.Person p,_) -> 
    (match r with 
      | D.Restriction.Unrestricted -> true
      | D.Restriction.Graduate i -> List.mem h (E.Graduates(p,i)) ~equal:Poly.(=)
    )
  | _ -> true (* Restrictions never knock out unknown persons *)

  let filter_by_always r h = function
  | (H.Person p,_) -> 
    (match r with
       | D.Restriction.Unrestricted -> true
       | D.Restriction.Graduate i -> List.mem h (E.Graduates(p,i)) ~equal:Poly.(=)
    )
  | _ -> true (* Restrictions never knock out unknown persons *)

  (* Who is in the class _right now_? Used when the class is permanently closed *)
  let rec eval (d:description) h: holder shares =
    match d with
    | Individuals ps  -> split_among ps
    | Spouse p -> 
      ( match F.spouse_of p h with
        | Unmarried -> []
        | Known p   -> sole (H.Person p)
        | Unknown   -> sole (H.Other (Spouse p)) )
    | Widow p -> 
      ( match F.widow_of p h with
        | Unmarried -> []
        | Known p   -> sole (H.Person p)
        | Unknown   -> sole (H.Other (Widow p)) )
    | Descendants (p,div) -> descendants p div h
    | Heirs p         -> if F.is_alive_after p h then [] else intestate_heirs p h
    | Children p      -> split_among (F.children_of p h)
    | Grandchildren p -> split_among (F.grandchildren_of p h)
    | Restriction(r,d1) 
      -> List.filter (eval d1 h) ~f:(filter_by_restriction r h)

    (* Members who _cannot_ be removed from the class *)
    let rec always (d:description) h: holder shares = 
      match d with 
      | Individuals ps  -> split_among ps
      | Spouse _ -> [] (* Could *always* change *)
      | Widow p -> 
        ( match F.widow_of p h with
          | Unmarried -> []
          | Known p   -> sole (H.Person p)
          | Unknown   -> sole (H.Other (Widow p)) )
      | Descendants (p,div) -> if F.is_alive_after p h then [] else descendants p div h
      | Heirs p         -> if F.is_alive_after p h then [] else intestate_heirs p h
      | Children p      -> split_among (F.children_of p h)
      | Grandchildren p -> split_among (F.grandchildren_of p h)
      | Restriction (r,d1) 
        -> List.filter (always d1 h) ~f:(filter_by_always r h)
      
  let rec track_base (d:description) h = match d with 
  | Restriction (_,d1) -> track_base d1 h
  | _ -> always d h

 (* Who has enough of an interest to break out individually and track subconveyances? *)
  (* Sets the share of unvested members to 0 -- i.e. they satisfy g but not r *)
  let track d h = List.map (track_base d h) 
    ~f:(fun (p,s) -> if List.Assoc.mem (always d h) p ~equal:Poly.(=) then (p,s) else (p,Q.zero))

  (* true if the class has closed on its own *)
  let rec closed (d:description) h : bool = 
     match d with
    | Individuals _    -> true
    | Spouse p        -> F.is_dead_after p h
    | Widow p         -> F.is_dead_after p h
    | Descendants (p,div) -> Poly.(descendants p div h = [])
    | Heirs p         -> F.is_dead_after p h
    | Children p      -> F.is_dead_after p h
    | Grandchildren p 
      -> (F.is_dead_after p h
          && not (List.fold (F.children_of p h) ~init:false 
                 ~f:(fun acc x -> acc || F.is_alive_after x h)))
    | Restriction(_,d') ->  closed d' h &&  Poly.(always d h = track d h) (*JG: This is wrong *)

  (* Is the class of vested members currently empty? *)
  let empty d h = Poly.(always d h = [])

  let rec possibly_fail_ascertain (d:description) h = match d with
  | Spouse _ | Widow _ -> true
  | Heirs _ -> true
  (* | _, PerStirpes -> true
  | _, ByRepresentation -> true *)
  | Children p -> Poly.((F.children_of p h) = [])
  | Grandchildren p -> Poly.((F.grandchildren_of p h) = [])
  | Restriction (r,d1) 
     -> if not (empty d h) then false 
        else ( match r with 
              | D.Restriction.Graduate _ -> true (* If no one _has_ graduated, it's possible no one will *)
              | _ -> possibly_fail_ascertain d1 h )
  | _  -> false

  let rec possibly_remote_ascertain (d:description) h = match d with
  | Grandchildren p when F.is_alive_after p h -> true
  | Restriction(Graduate _, _) -> false (* JG: wrong *)
  | Restriction(_,d1) -> possibly_remote_ascertain d1 h
  | _ -> false

end