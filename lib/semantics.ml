open Core_kernel
open Common
open Event
open Predicate
open Term
open Classgift
open Classify
open Interest
open Condition
open Parser

type wills = UnnamedTerm.t Wills.t

module type SEMANTICS = sig

  module U : TERM
  module N : TERM

  type t [@@deriving sexp]
  type term =  U.t [@@deriving sexp]
  type named_term =  N.t [@@deriving sexp]

  val init : t
  val unlift : t -> term
  val owns :  t -> person -> t
  val occurs :  t -> event -> t
  val conveys : t -> person -> term -> t
  val makes_will : t -> person -> term -> t

  val translate : Parser.combination ->  person -> term

  val name : term -> named_term
  val unname : named_term -> term
end

module Derivatives = struct

  module A = Annotate.Annotation
  module C = UnnamedCondition
  module CG = ClassGifts
  module D = Description
  module P = Predicate
  module F = Family
  module N = Term.NamedTerm
  module U = Term.UnnamedTerm
 
 
  module CGI = struct
    let cgi_apply f (i:Unnamed.t) = f (Unnamed.grantee i) (Unnamed.history i)
    let eval = cgi_apply CG.eval
    let closed = cgi_apply CG.closed
    let track = cgi_apply CG.track
  end
  
  type term =  U.t [@@deriving sexp]
  type named_term =  N.t [@@deriving sexp]
  type unannoted_term = U.t [@@deriving sexp]
  type t = term * history * term will_list [@@deriving sexp]

  let (=) = Poly.(=)

  let apply_named f t = t |> A.name |> f |> A.unname
      
  (* If p is dead, determines who receives their property *)
  let inheritance (p:person) (_:history) (wills:wills):term =
      match Wills.will_of p wills with
      | Some will -> will 
      | None      -> Bottom

  let update_class_shares (i,ts) hs: (holder * term) list =
    List.map hs 
    ~f:(fun (h,_) -> match List.Assoc.find ts  ~equal:Poly.(=) h with 
        | Some t -> (h,t)
        | None -> (h, U.Atom (Unnamed.transfer_to i h)) ) 

  let holder_shares_to_term_shares i (hs: holder shares) hts:term shares =
    List.map hs ~f:(fun (h,s) -> match List.Assoc.find hts ~equal:Poly.(=) h with 
    | Some t -> (t,s)
    | None -> (U.Atom {i with grantee = Individuals (Holder.persons h); owner = h}, s) )

  let divide (i,hts): term = 
    let ts = holder_shares_to_term_shares i (CGI.eval i) hts in
    Shared(Common,ts)
           
  let ascertain_possessory ((i,ts):U.class_node):term =  
    if not (CGI.eval i = []) then divide (i,ts)
    (* JG: limit to cases of true remainders *)
    else if CGI.closed i || !Parameters.destructibility_of_contingent_remainders
        then Bottom 
    else  
      let reversion () = U.Atom (Unnamed.self i.grantor) in
      let cond  = C.of_predicate (P.Not(P.Atomic(P.A.Known i.grantee ))) Precedent in
      Seq(While(cond, reversion()), Class (i,ts))

  let ascertain_nonpossessory (((i,_) as c):U.class_node):term = 
    if CGI.closed i then divide c
    else Class (i, update_class_shares c (CGI.track i) )
 
  (* Simplify an expression based on the state of the conditions *)
  let rec delta : term -> term = function
    | Bottom  -> Bottom
    | Atom i  -> Atom i 
    | Class c -> ascertain_possessory c
    | Seq(t1,t2) ->
      if delta t1 = Bottom then delta t2 else Seq(delta t1, t2)
    | Shared(ct,ts) -> 
      ( match U.Shared(ct,Shares.map ts ~g:delta) with
        | Shared (_, []) -> Bottom
        | Shared (_, [(t1,_)]) -> t1
        | t'              -> t' ) 
    | If(c,t1,t2) ->
      if (C.eval c) then delta t1 else delta t2
    | While(c,t1) when c.source = Precedent 
       -> delta (While({c with source = ImpliedReversion},t1)) (*For naming / noDCR*)
    | While(c,Atom i) when c.source = Executory && Unnamed.is_reversionary i 
       -> delta (While({c with source = ImpliedReversion},Atom i)) (*For naming *)
    | While(c,t1) ->
      if (not (C.eval c)) || delta t1 = Bottom then Bottom
      else While (C.delta c, delta t1)
    | Tail(i,ps,t1) ->  Tail (i, ps, delta t1)

    (* Eliminates the interests of any joint/tail tenants who have died *)
    (* It presupposes that deaths are not simultaneous *)
    let term_death (t:term) :term =
      let joint_death = function 
      | (U.Atom {owner = Person p; history;_},_) when F.is_dead_after p history -> false
      | _ -> true  in 
      let rec tail_death (i,ps,t1):term = match ps with 
      | []       -> U.Bottom
      | p::ps_tl when F.is_dead_after p (Unnamed.history i)
        -> ( match  F.firstborn_living_descendant p (Unnamed.history i) with
             | Some p1 
               -> let atom = U.Atom (Unnamed.transfer_to i (Person p1)) in 
                  U.Tail(i, (p1::ps), atom)
             | None    -> tail_death (i,ps_tl,t1) )
      | _ -> U.Tail(i,ps,t1)
    in
      U.traverse (function 
      | Shared(Joint,ts) -> Shared(Joint, List.filter ts ~f:joint_death)
      | Shared(Entireties,ts) -> Shared(Entireties, List.filter ts ~f:joint_death)
      | Tail (i,ps,t1) -> tail_death (i,ps,t1)
      | x -> x ) t

     let rec flatten = List.concat_map ~f:(function 
      | (U.Shared(Common,ts),q) -> flatten ts |> Shares.scale q
      | (t,q) -> [(t,q)] )
    
    (* This step collapses multiple equal terms into a common one *)
    let combine ts = 
      let same_owner t1 t2 = Poly.(=) (U.possessors t1) (U.possessors t2) in 
      let combine' acc (t,num) = 
       match List.Assoc.find acc ~equal:same_owner t with
       | None -> List.Assoc.add acc ~equal:same_owner t num
       | Some num' -> List.Assoc.add (List.Assoc.remove acc ~equal:same_owner t) ~equal:same_owner t 
         (Q.(+) num num')
    in List.fold ts ~init:[] ~f:combine'
    
  (* This step checks whether there is a single owner of all interests *) 
  (* If so, it merges them. If there are NO interests, it replaces them with Bottom *)
  let merge :term -> term = function
  | Shared (_, [(t1,_)]) -> t1
  | Shared (_, []) -> Bottom
  | Shared (ct, ts)              -> Shared(ct, Shares.normalize ts)
  | x -> x

  let ascertain = U.traverse (function 
     | Class c -> ascertain_nonpossessory c 
     | x -> x)

  (* Prunes impossible branches *)
  let prune (t:term) : term =
    let prune_unreachable = apply_named (N.traverse (fun x -> match x with 
      | Atom i | Class (i,_) -> if i.reachable then x else Bottom 
      | _ -> x)) in
    let rec prune': term -> term = function 
      | Bottom -> Bottom
      | Atom i -> Atom i
      | Class c -> Class c
      | Seq(t1,t2) -> 
        if      prune' t1 = Bottom then prune' t2
        else if prune' t2 = Bottom then prune' t1
        else Seq(prune' t1, prune' t2)
      | Shared(Common,ts) -> Shared (Common, 
        (Shares.map ts ~g:prune') |> flatten |> combine) |> merge 
      | Shared(Joint,ts)      -> Shared(Joint,ts)        |> merge
      | Shared(Entireties,ts) -> Shared(Entireties,ts)   |> merge
      | If(c,t1,t2) ->
        if      (C.always c) then prune' t1
        else if (C.never c) then prune' t2
        else if prune' t1 = Bottom && prune' t2 = Bottom then Bottom
        else If(C.simplify c, prune' t1, prune' t2)
      | While(c,t1) ->
        if      C.always c then prune' t1
        else if C.never c then Bottom
        else if  prune' t1 = Bottom then Bottom
        else While(C.simplify c,prune' t1)
      | Tail(i,ps,t1) -> Tail(i, ps, prune' t1)
    in
    t  |> ascertain |> prune' |> prune_unreachable |> prune'
    
    (* Merger can be in one of four modes: 
       (1) Searching for initial LE
       (2) Found: found LE, looking for matching VR
       (3) Done: found LE and matching VR
       (4) NoMerger possible *)
    type merger_search_t =
    | Searching 
    | Found of person
    | Done
    | NoMerger

    let merger (t:term):term =
      let merger_lite (t:term):term = U.traverse (function 
      | Seq(While(_, Atom i1), Atom i2) when i1.owner = i2.owner -> Atom i2
      | x -> x) t in

      let rec merger' (t:named_term) (mode:merger_search_t): named_term * merger_search_t = 
      match t, mode with
      | Bottom,_           -> Bottom, mode
      | Atom {nature = Possessory; 
              quantum = Duration.ForLife p1;
              owner = Person p2; _}, Searching when p1 = p2 
        -> Bottom, Found p1
      | Atom i, Found p  
      | Class (i,_), Found p ->
           (match i.nature, Vesting.vested i.vesting with
            (*Found a matching vested remainder, switch out of search mode*)
            | Remainder, true 
            | Reversion, true when i.owner = Person p -> t, Done
            (* Found a non-matching vested remainder, so no merger at all *)
            | Remainder, true 
            | Reversion, true when not (i.owner = Person p) -> t, NoMerger
            (*Found a contingent remainder, delete it, stay in searcch mode *)
            | Remainder, false  -> Bottom, Found p
            (*Anything else: stay in search mode *)
            | _, _  -> t, Found p ) 
      | Atom _, _  -> t, mode
      | Class _, _ -> t, mode
      | Shared (ct,ts), _ -> Shared (ct,ts), mode  (*JG: This is wrong *)
      | Seq(t1,t2),_        -> 
          let t1', mode1 = merger' t1 mode in 
          let t2', mode2 = merger' t2 mode1 in 
          Seq(t1',t2'), mode2
      | If(c,t1,t2), _       ->           
          let t1',_ = merger' t1 mode in 
          let t2',_ = merger' t2 mode in 
           If(c,t1',t2'), mode        (* Mode cannot change inside an if *)
      | While(c,t1),_   ->           
          let t1',mode1 = merger' t1 mode in
          While(c,t1'),mode1
      | Tail(i,ps,t1),_ -> Tail(i,ps,t1),mode (* JG: merger for fee tail *) in
    let do_merger t = match merger' t Searching with | t', Done -> t' | _ ->  t
    in apply_named do_merger t |> merger_lite

  let rap (t:term)  = 
  if !Parameters.rule_against_perpetuities then 
    apply_named (N.traverse (function 
    | Atom i  | Class (i,_) | Tail (i,_,_)  
      when i.remote = RRemoteCondition && not (Named.is_reversionary i) 
      -> Bottom 
    | While(({source = Executory;_},RRemoteCondition),t1) -> t1
    | x -> x) ) t
  else t 
       
  type conveyance_timing =
  | InterVivos
  | PostMortem [@@deriving sexp]

  (* Applies any doctrines neeeded to modify t at the moment of insertion into the title tree *)
  let insert (grantor:person) (t:term) (old_i:named_interest) (timing:conveyance_timing):term =
    let module H = Holder in
    let h = old_i.history in
    let intestate_heirs_term =
      let i = Unnamed.replace_history (Unnamed.create grantor (D.Heirs grantor)) h in
      divide (i, []) in

    let check_unities (t:term):term = 
      let are_equal l = List.for_all l ~f:(fun (_,s) -> s = Q.one ) in
      let are_spouses h1 h2 h = match h1,h2 with 
      | H.Person p1, H.Person p2 when F.spouse_of p1 h = F.Known p2 -> true
      | Person p1,         Other (Spouse p2)
      | Other (Spouse p1), Person p2         when p1 = p2 -> true 
      | _ -> false in 
      t |> U.traverse (function  
      (* Entireties tenancies are only for spouses *)
      | Shared (Entireties, ([(Atom {owner = h1;_},_);(Atom {owner = h2;_},_)] as ts))
        when (are_spouses h1 h2 h) && are_equal ts
        -> Shared(Entireties, ts)
      (* Joint and entireties tenancies require equal shares *)
      | Shared(Entireties, ts)
      | Shared(Joint, ts) when are_equal ts
          -> Shared(Joint, ts)
      (* Tenancy in common is the fallback *)
      | Shared(_, ts) -> Shared(Common,ts)
      | x -> x) in

    let dwtrsc = apply_named (N.traverse  (function
    | Class (i,_) when !Parameters.doctrine_of_worthier_title
                       && timing = InterVivos 
                       && (i.nature = Remainder || i.nature = ExecutoryInterest)
                       && i.grantee = D.Heirs grantor
      -> Atom {i with grantee = Individuals [grantor]; owner = Person grantor}
    | Class (({nature = Remainder; owner = Description (Heirs p);_} as i),_) 
             when !Parameters.rule_in_shelleys_case 
             && List.exists (N.leftmost (A.name t))
            ~f:(fun il -> il.quantum = ForLife p && il.owner = Person p)
      -> Atom {i with grantee = Individuals [p]; owner = Person p}
    | x -> x ) ) in

    (* Grant to deceased taker *)
    let lapse t = t |> U.traverse (function 
    | Atom {owner = Person p; _} when F.is_dead_after p h -> 
        (* Explicit inter vivos grant to dead person simply fails *)
        if timing = InterVivos then Bottom
        (* Lapsed gift by decedent passes to residuary estate of grantor *)
        else intestate_heirs_term
    | x -> x)
    in

    (* Reversionary status propagates to leftmost interest *)
    (* JG: This is wrong when the interest being replaced is possessory *)
    let propagate_reversionary  = apply_named (N.traverse (function
      | Atom i when i.nature = Possessory  && not (old_i.nature = Possessory)
        -> Atom  {i with grantor = old_i.grantor; grantee = old_i.grantee}
      | x -> x) ) in

    let reversion =  if F.is_alive_after grantor h then U.owns grantor 
                     else intestate_heirs_term
    in
    U.Seq(t,reversion) |> U.replace_history h |> ascertain |> check_unities
    |> dwtrsc |> lapse |> propagate_reversionary |> rap

  (* Substitutes to_t into from_t wherever Atom p appears *)
  let subst from_t p to_t timing =
    let keep = function 
    | (N.Atom i,_) when i.owner = Person p -> false 
    | _ -> true   in
    let not_keep x = not (keep x) in
    let replace i = A.name (insert p to_t i timing) in

    let rec subst':named_term -> named_term = function
    | Bottom -> N.Bottom
    | Atom i when i.owner = Person p -> replace i
    | Atom i -> Atom i
    | Class (i,hts) -> 
      Class (i, List.map hts ~f:(fun (p,t1) -> ((p,subst' t1))) )
    (* Severance, then conveyance *)
    | Shared(Joint,ts) ->  
      ( match List.find ts ~f:not_keep with
        | Some (Atom i,s)
          -> Shared(Common, 
                    [(replace i, s); 
                     (Shared(Joint, List.filter ts ~f:keep), Q.(one - s)) ] )
        | _ -> Shared(Joint,ts)
      )
    (* Cannot be separately conveyed. JG: deal with joint conveyances? *)
    | Shared(Entireties,ts) -> Shared(Entireties, ts) 
    | Shared(Common,ts) -> Shared(Common,List.map ts ~f:(fun (t1,s) -> (subst' t1, s)))
    | If(c,t1,t2) -> If(c, subst' t1, subst' t2)
    | While(c,t1) -> While(c, subst' t1)
    | Seq(t1,t2) -> Seq(subst' t1, subst' t2)
    (* Disentailment by inter vivos conveyance of fee simple by holder of tail *)
    | Tail(i1,ps,Atom i2) when !Parameters.disentail_by_conveyance 
                             && i2.owner = Person p 
                             && timing = InterVivos
        -> ( match to_t with 
            | Atom _ | Shared _ | Class _ -> replace i2 
            | _ ->  Tail (i1, ps, subst' (Atom i2)) )
    | Tail (i1, ps, t1) -> Tail (i1, ps, subst' t1) 
    in apply_named subst' from_t

  let update t = 
    t |> delta |> merger |> prune 

  (** Implement the SEMANTICS interface *)
  (* Convert a term to a thunk *)
  let init = 
    (U.owns (Person.of_string !Parameters.initial_owner), [], [])

  (* Extract the current term from the thunk *)
  let unlift ((t,_,_):t) : term = t

  let owns (_,h,w) p =
    (U.owns p |> update, h,w)

  let conveys (from_t,h,w) p to_t =
    (subst from_t p to_t InterVivos |> update |> update, h,w)

  (* Step the given thunk using the next (given) event *)
   let occurs ((t,h,w):t) (e:event) : t =
    let t1 =  (t |> U.add_event e |> term_death |> update) in
    let t2 = match e with
    | Dies p -> 
      (subst t1 p (inheritance p (e::h) w) PostMortem)
    | _ -> t1 in 
    (update t2, e::h, w)

  let makes_will (t,h,wills) p will  =
    (t, h, Wills.add_will p will wills)
 
  let rec translate (c:Parser.combination) (grantor:person) :  term =
    
    let module E = Event in
    let module H = Holder in 
    let module P = PredicateType in
    let open UnnamedTerm in
    let module I = Unnamed in
    let module C = UnnamedCondition in

    let reversion () = Atom (I.self grantor) in
    match c with

  | Nothing -> Bottom

  | Single g ->
    let atom p = Atom (I.create_vested grantor (D.Individuals [p]) (Person p)) in
    let atoms ct ps = 
       let ct' = ( match ct, !Parameters.ambiguous_tenancy with
                 | InCommon, _      -> Cotenancy.Common
                 | Jointly, _       -> Cotenancy.Joint
                 | ByEntireties,_   -> Cotenancy.Entireties
                 | UnspecifiedCotenancy, Parameters.AmbiguousTenancy.InCommon -> Cotenancy.Common
                 | UnspecifiedCotenancy, Parameters.AmbiguousTenancy.Joint    -> Cotenancy.Joint)
      in Shared(ct', List.map ps ~f:(fun p -> (atom p,Q.one))) in
    let make_while d g = While(C.of_predicate (Duration.to_predicate d) (Natural d), g) in
    ( match g with 
      | (Individuals [p],_,InTail) 
        -> Tail(I.create_vested grantor (D.Individuals [p]) (Person p), [p], atom p)
      | (Individuals [p],_, ForLife) -> make_while (Duration.ForLife p) (atom p)
      | (Individuals [p],_, UnspecifiedDuration) ->
        (match !Parameters.ambiguous_interest with
        | Parameters.AmbiguousInterest.FeeSimple -> atom p
        | Parameters.AmbiguousInterest.LifeEstate -> make_while (Duration.ForLife p) (atom p)
        ) 
      | (Individuals ps, ct, ForLifePerson p1) -> make_while (Duration.ForLife p1) (atoms ct ps)
      | (Individuals ps, ct, ForYears n) -> make_while (Duration.ForYears n) (atoms ct ps)
      | (Individuals ps, ct, _) -> atoms ct ps
      | (d,_,_) -> Class(I.create grantor d, []) )
        
  | CondPredWait (cl1, cl2, pr) when not !Parameters.destructibility_of_contingent_remainders ->
    let after = If (C.of_predicate pr If, translate cl1 grantor,  translate cl2 grantor) in
    let cond = C.of_predicate (P.Not(P.Or( pr, P.Never pr))) Precedent in
    Seq(While(cond,reversion ()), after)

  | CondPredWait (cl1, cl2, pr)
  | CondPred (cl1,cl2,pr) ->
      If (C.of_predicate pr If, translate cl1 grantor,  translate cl2 grantor)

  | After (cl1, pr) ->
    let cond = C.of_predicate (P.Not pr) Precedent in
    Seq(While(cond, reversion ()), translate cl1 grantor)

  | While (cl,pr) -> 
    While (C.of_predicate pr Added, translate cl grantor)

  | Then (c,c') -> 
    Seq(translate c grantor, translate c' grantor)

  | But (c1,pr,c2) ->
    let cond = C.of_predicate (P.Not pr) Executory in
    let before = Seq ( (translate c1 grantor), reversion ()) in
    let after = translate c2 grantor in
    Seq( While (cond, before), after)

  | Reentry(c,pr) ->
    let cond = C.of_predicate (P.Not (P.And ((P.Atomic (P.A.Reentry grantor)), (P.Past (P.And (pr, (P.Past (P.Occurs (E.Start))))))))) Subsequent in
    let before = Seq ((translate c grantor), reversion ()) in
    Seq( While(cond,before), reversion ())

  (* Naming *)
  let name = A.name
  let unname = A.unname

end
