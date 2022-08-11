open Common
open Event

(* AM: mostly autogenerated, then trimmed to hide the graph *)

(** A module that represents family interactions and semantics. Should ideally
 ** be a functor on Events and Persons, but this will do for now. *)

(* The Events that this model of families can respond to. Should ideally be a
   functor argument. *)
module E = Event
module P = Person

(** *** Useful types *** **)
type survivor = Neither | FirstP | SecondP [@@deriving sexp]
type spouse = Unmarried | Unknown | Known of P.t [@@deriving sexp]

(** *** Find relationships of the person after the given history *** **)
val survivor : P.t -> P.t -> E.t list -> survivor

val widow_of : P.t -> E.t list -> spouse
val spouse_of : P.t -> E.t list -> spouse
val grandchildren_of : P.t -> E.t list -> P.t list
val parents_of : P.t -> E.t list -> P.t list
val children_of : P.t -> E.t list -> P.t list

val firstborn_living_descendant : P.t -> E.t list -> P.t option

(** *** Useful predicates after a history *** **)
val is_alive_after : P.t -> E.t list -> bool
val is_dead_after : P.t -> E.t list -> bool
val has_issue : P.t -> E.t list -> bool

val years_passed : E.t list -> int
