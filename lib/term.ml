open Core_kernel
open Common
open Interest
open Event
open Condition

module Json = Yojson

module type TERM = sig
  type interest [@@deriving sexp]
  type condition [@@deriving sexp]

  type t =
    | Bottom
    | Atom  of interest
    | Class of class_node
    | Shared of cotenancy * t shares
    | Seq   of t * t
    | While of  condition * t
    | If    of condition * t * t
    | Tail of interest * person list * t [@@deriving sexp]
    and class_node =  interest * (holder * t) list [@@deriving sexp]

  val owns : person -> t
  val to_json : t -> Json.t

  val to_abstract : t -> string
  val possessors : t -> holder list
  val leftmost : t -> interest list
  val persons_of_term : t -> person list
  val traverse : (t -> t) -> t -> t
  val add_event : event -> t -> t
  val replace_history : history -> t -> t
  (* val of_combination : Parser.combination -> person -> t *)
end

module MakeTerm (I:INTEREST) (C:CONDITION): TERM 
with type interest = I.t and type condition = C.t = struct
  type interest = I.t [@@deriving sexp]
  type condition = C.t [@@deriving sexp]

  module D = Description

  type t =
    | Bottom
    | Atom  of interest
    | Class of class_node
    | Shared of cotenancy * t shares
    | Seq   of t * t
    | While of  condition * t
    | If    of condition * t * t 
    | Tail of interest * person list * t [@@deriving sexp]
  and class_node = interest * (holder * t) list [@@deriving sexp]


  let rec to_json t  : Json.t = match t with
    | Bottom    -> `String "bottom"
    | Atom (i) -> `Assoc [ ("atom", `String (I.to_string i ))]
    | Class (i,ts) -> `Assoc [("class", 
                             `Assoc [ ("description", `String (Description.to_string (I.grantee i)))
                                    ; ("interests", if Poly.(ts = []) then `List ([`Assoc [("atom",`String (I.to_string i))]])
                                                    else `List (List.map ts 
                                                          ~f:(fun t' -> to_json (snd t')) ))])]
    | Shared (Common, l) -> `Assoc [ ("common", `List (List.map l ~f:(fun t' -> to_json (fst t'))))]
    | Shared (Joint, l) -> `Assoc [ ("joint", `List (List.map l ~f:(fun t' -> to_json (fst t'))))]
    | Shared (Entireties, l) -> `Assoc [ ("entireties", `List (List.map l ~f:(fun t' -> to_json (fst t'))))]
    | Seq(t1,t2) -> `Assoc [ ("seq", `List [to_json t1; to_json t2])]
    | While (c,Atom i) 
      when !Parameters.hide_implied_reversions 
      && I.is_reversionary i && Poly.(C.source c = Precedent) -> `Null
    | While (c,t1) ->  `Assoc [ ("while",
                              `Assoc [ ("condition", `String (C.to_json c))
                                      ; ("term", to_json t1)] )]  
    | If(c,t1,t2) -> `Assoc [ ("if",
                             `Assoc [ ("condition", `String (C.to_json c))
                                    ; ("term1", to_json t1)
                                    ; ("term2", to_json t2)] )]
    | Tail(i,_,t1)  -> `Assoc ["tail", 
                          `Assoc [("issue", `String (Holder.to_string (I.owner i)));
                           ("term", to_json t1)]]

  let rec to_abstract t : string = match t with
    | Bottom   -> "Nothing"
    | Atom (i) -> sprintf "Atom (%s)" (I.to_abstract i)
    | Class (i,_) -> sprintf "class [%s]" (I.to_string i) (*JG: Wrong*)
    | Shared (Common, ts) -> sprintf "Common [%s]" (String.concat ~sep:" ; " (List.map ts ~f:(fun t' -> to_abstract (fst t'))))
    | Shared (Joint, ts) -> sprintf "Joint [%s]" (String.concat ~sep:" ; " (List.map ts ~f:(fun t' -> to_abstract (fst t'))))
    | Shared (Entireties, ts) -> sprintf "Entirety [%s]" (String.concat ~sep:" ; " (List.map ts ~f:(fun t' -> to_abstract (fst t'))))
    | Seq(t1, t2) -> sprintf "(%s ; %s)" (to_abstract t1) (to_abstract t2)
    | While(c,t) -> sprintf "While (%s, %s)" (C.to_string c)
                          (to_abstract t)
    | If(c,t1,t2) -> sprintf "If (%s, %s, %s)" (C.to_string c) (to_abstract t1) (to_abstract t2)
    | Tail(i,_,t1) -> sprintf "Tail (%s) (%s)" (Holder.to_string (I.owner i)) (to_abstract t1)

  let owns p = Atom (I.self p)

  (* Return the leftmost interests of a term *)
  let rec leftmost t : interest list = match t with
    | Bottom     -> []
    | Atom (i) -> [i]
    | Class (i,hts) -> [i] @ (List.concat_map hts 
                                ~f:(fun (_,t') -> leftmost t'))
    | Shared (_,ts) -> List.concat_map ts ~f:(fun t' -> leftmost (fst t'))
    | Seq(t1, _) -> leftmost t1
    | If(_,t1,_) -> leftmost t1
    | While(_,t1) -> leftmost t1
    | Tail(_,_,t1) -> leftmost t1

  (* Return the possessors of a term *)
  let possessors t : holder list =
    List.map (leftmost t) ~f:(fun i -> I.owner i)

  let rec persons_of_term t : person list= match t with
    | Bottom    -> []
    | Atom (i) -> Holder.persons (I.owner i)
    | Class (i,hts) -> Holder.persons (I.owner i) @ 
                         List.concat_map hts ~f:(fun (_,t') -> persons_of_term t')
    | Shared (_,l) -> List.concat_map l ~f:(fun t' -> persons_of_term (fst t'))
    | Seq(t1, t2) -> (persons_of_term t1) @ (persons_of_term t2)
    | If(_,t1, t2) -> (persons_of_term t1) @ (persons_of_term t2)
    | While(_,t1) -> persons_of_term t1
    | Tail(i,_,t1) -> Holder.persons (I.owner i) @ (persons_of_term t1) (* JG: Wrong *)

  let rec traverse (f:t->t) (t:t):t = 
    let t' = match t with 
    | Bottom -> Bottom
    | Atom i -> Atom i
    | Class(i,ts) -> Class(i, List.map ts 
                        ~f:(fun (p,t') -> (p, traverse f t') ))
    | Shared (ct,l) -> Shared (ct, List.map l ~f:(fun t' -> (traverse f (fst t'), snd t')))
    | If (c,t1,t2) -> If (c, traverse f t1, traverse f t2)
    | While (c,t1) -> While (c, traverse f t1)
    | Seq(t1,t2) -> Seq(traverse f t1, traverse f t2)
    | Tail(i,ps, t1) -> Tail(i,ps,traverse f t1)
  in f t'

  let add_event (e:event) (t:t): t =  
    traverse (function
    | Atom i -> Atom (I.add_event i e)
    | Class(i,ts) -> Class(I.add_event i e,ts)
    | If(c,t1,t2) -> If(C.add_event c e, t1, t2 )
    | While(c,t1) -> While(C.add_event c e, t1 )
    | Tail(i,ps,t1) -> Tail(I.add_event i e, ps, t1)
    | x -> x ) t

  let replace_history  (h:history) (t:t) : t =  
    traverse (function
    | Atom i -> Atom (I.replace_history i h)
    | Class(i,ts) -> Class(I.replace_history i h,ts)
    | If(c,t1,t2) -> If(C.replace_history c h, t1, t2)
    | While(c,t1) -> While (C.replace_history c h, t1)
    | Tail(i,ps,t1) -> Tail (I.replace_history i h, ps, t1)
    | x -> x ) t

end
  

module UnnamedTerm = MakeTerm(Unnamed)(UnnamedCondition)
module NamedTerm = MakeTerm(Named)(NamedCondition)

type term = UnnamedTerm.t [@@deriving sexp]

