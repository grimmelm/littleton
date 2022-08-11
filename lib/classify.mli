open Common
open Event
open Predicate

module Nature : sig
  type t =
    | Possessory
    | Remainder
    | Reversion
    | PossibilityOfReverter
    | PowerOfTermination
    | ExecutoryInterest
    | Unreachable  [@@deriving sexp]

  val to_string: t -> string
end

module Vesting : sig

    type t =
    | Unreachable    
    | Contingent
    | VestedSubject
    | VestedOpen
    | Vested
    | Possessory      [@@deriving sexp]

  val to_string: t -> string
  val to_full_string: t -> string
  val min: t -> t -> t
  val max: t -> t -> t
  val vested: t -> bool

end

module Duration : sig

  type t =
    | Absolute
    | InTail   of person
    | ForLife  of person
    | ForYears of interval  [@@deriving sexp]

  val to_string: t -> string
  val to_quantum : t -> string
  val to_predicate : t -> PredicateType.t

  val min: t -> t -> t
  val events: t -> ExEventSet.t
end

type duration = Duration.t [@@deriving sexp]

module FullDuration : sig
  type t =
    { quantum : duration
    ; determinable : bool
    ; subsequent : bool
    ; executory : bool
    ; with_executory: bool    } [@@deriving sexp]

  val make: duration -> t
  val has_limit : t -> bool
  val to_string: t -> string
  val to_full_string: t -> holder -> string
end

module Source : sig

  type t =
    | Natural of Duration.t 
    | Added
    | Subsequent 
    | Precedent 
    | Executory 
    | ImpliedReversion 
    | If [@@deriving sexp]

  val to_string: t -> string
  val to_duration : t -> FullDuration.t -> bool -> FullDuration.t

end
type source = Source.t [@@deriving sexp]

module Divest : sig 
  type t = 
    | NoDivest
    | Springing
    | Shifting 
    | Termination [@@deriving compare, sexp]
val to_string: t -> string
end
type divest_t = Divest.t [@@deriving compare, sexp]

type remoteness =
  | RNone
  | RCondition
  | RRemote
  | RRemoteCondition
  | RNever [@@deriving sexp]


type share_t = Q.t  [@@deriving sexp]