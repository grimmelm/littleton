open Event
open Predicate
open Classify

module type CONDITION = sig
  type t [@@deriving sexp]

  (* val equal : event -> event -> bool *)
  val add_event : t -> event -> t
  val replace_history : t -> history -> t
  val delta : t -> t
  val simplify :  t -> t
  val eval :  t -> bool
  val always : t -> bool
  val never : t -> bool
  val must_wait : t -> bool
  val possibly_remote : t -> bool
  val of_predicate : predicate -> source -> t
  val to_predicate : t -> predicate 
  val source : t -> source
  val to_json : t -> string
  val to_string : t -> string

end

type unnamed_condition =  {
  predicate : PredicateType.t;
  source : source;
  ready : bool;
  history : history;  } [@@deriving sexp]
type named_condition = unnamed_condition * remoteness [@@deriving sexp]

module UnnamedCondition : CONDITION with type t = unnamed_condition [@@deriving sexp]
module NamedCondition : CONDITION with type t = named_condition [@@deriving sexp]