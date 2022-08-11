open Common
open Event
open Interest
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
    | While of condition * t
    | If    of condition * t * t
    | Tail of interest * person list * t [@@deriving sexp]
  and class_node =  interest * (holder * t) list [@@deriving sexp]


  val owns : person ->  t
  val to_json : t -> Json.t
  val to_abstract : t -> string
  val possessors : t -> holder list
  val leftmost : t -> interest list
  val persons_of_term : t -> person list
  val traverse : (t -> t) -> t -> t
  val add_event : event -> t -> t
  val replace_history : history -> t -> t

end

module UnnamedTerm : TERM with type interest = Unnamed.t and type condition = UnnamedCondition.t
module NamedTerm : TERM with type interest = Named.t and type condition = NamedCondition.t

type term = UnnamedTerm.t [@@deriving sexp]
