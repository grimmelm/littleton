open Core_kernel
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

(* AM: functor on Event, and/or Family? *)
module UnnamedCondition : CONDITION with type t =  unnamed_condition = struct
  module P = PredicateType
  module E = Event
  module F = Family

  type t = unnamed_condition [@@deriving sexp]
  (* { p : P.t;
    source : source;
    ready : bool;
    history : history;
  } [@@deriving sexp] *)
  (* type t =  P.t * source * bool * history  *)

  (* exception UnsupportedCondition of P.t   *)
  (* let equal = E.(=) *)

  let add_event c e = {c with history = e::c.history}
  let replace_history c h' = {c with history = h'}
  let delta c = if c.ready then c else {c with ready = true; history = E.Start::c.history}
  let simplify c = {c with predicate = P.simplify c.predicate c.history}
  let eval c = P.eval c.predicate c.history
  let always c = P.always c.predicate c.history
  let never c = P.never c.predicate c.history
  let must_wait c = P.must_wait c.predicate
  let possibly_remote c = P.possibly_remote c.predicate c.history
  let of_predicate predicate source = {predicate; source; ready = false; history = []}
  let to_predicate c = c.predicate
  let source c = c.source

  (* JG: This is a hack that is a warning of a deeper design issue *)
  let to_json c = match c.predicate with
    | P.Not(P.Atomic(P.A.Years n)) -> P.to_json (P.Atomic (P.A.Years (n - F.years_passed c.history)))
    | _ -> P.to_json c.predicate

  (* JG: This is a hack that is a warning of a deeper design issue *)
  let to_string c = match c.predicate with
    | P.Not(P.Atomic(P.A.Years n)) -> P.to_string (P.Atomic (P.A.Years (n - F.years_passed c.history)))
    | _ -> P.to_string c.predicate
end



type named_condition = unnamed_condition * remoteness [@@deriving sexp]

(* AM: functor on Event, and/or Family? *)
module NamedCondition : CONDITION with type t =  named_condition = struct
  module U = UnnamedCondition
  module P = PredicateType
  module E = Event
  module F = Family

  type t =  named_condition [@@deriving sexp]
  
  let add_event (c,r) e = (U.add_event c e, r)
  let replace_history (c,r) e = (U.replace_history c e,r)
  let delta(c,r) = (U.delta c, r)
  let simplify (c,r) = (U.simplify c,r)
  let eval (c,_) = U.eval c
  let always (c,_) = U.always c
  let never (c,_) = U.never c
  let must_wait(c,_) = U.must_wait c
  let possibly_remote (c,_) = U.possibly_remote c
  let of_predicate p s = (U.of_predicate p s, RNone)
  let to_predicate (c,_) = U.to_predicate c
  let source (c,_) = U.source c
  let to_json (c,_) = U.to_string c
  let to_string (c,_) = U.to_string c

end
