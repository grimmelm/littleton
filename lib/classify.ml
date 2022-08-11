open Core_kernel
open Common
open Event
open Predicate

module Nature = struct
  type t =
    | Possessory
    | Remainder
    | Reversion
    | PossibilityOfReverter
    | PowerOfTermination
    | ExecutoryInterest
    | Unreachable  [@@deriving sexp]

  let to_string = function
    | Possessory -> "possessory estate"
    | Remainder -> "remainder"
    | Reversion -> "a reversion"
    | PossibilityOfReverter -> "possibility of reverter"
    | PowerOfTermination -> "power of termination"
    | ExecutoryInterest -> "executory interest"
    | Unreachable -> "unreachable interest"
end

module Vesting = struct
  type t =
  | Unreachable    
  | Contingent
  | VestedSubject
  | VestedOpen
  | Vested
  | Possessory      [@@deriving sexp]

  let to_string = function
    | Vested -> "vested"
    | VestedOpen -> "vested (subject to open)"
    | VestedSubject -> "vested (subject to divestment)"
    | Contingent -> "contingent"
    | Possessory -> "possessory"
    | Unreachable -> "unreachable"

  let to_full_string = function
    | Vested -> "indefeasibly vested"
    | VestedOpen -> "vested subject to open"
    | VestedSubject -> "vested subject to complete divestment"
    | Contingent -> "subject to condition precedent"
    | Possessory -> "possessory"
    | Unreachable -> "unreachable"

  let min = Poly.min 
  let max = Poly.max  
  let vested v = Poly.(v >= VestedSubject)

end

module Duration = struct
  module P = PredicateType
  module E = Event

  type t =
    | Absolute
    | InTail   of person
    | ForLife  of person
    | ForYears of interval  [@@deriving sexp]

  let to_string = function
    | Absolute   -> "absolute"
    | InTail p   -> "failure of issue of " ^  (Person.to_string p)
    | ForLife p  -> "death of " ^  (Person.to_string p)
    | ForYears i -> sprintf "for %i years" i

  let to_quantum = function
    | Absolute   -> "fee simple"
    | InTail _   -> "fee tail"
    | ForLife _  -> "life estate"
    | ForYears _ -> "term of years"

  let to_predicate = PredicateType.( function
      | Absolute   -> True
      | InTail p   ->  P.Atomic(P.A.HasIssue p)
      | ForLife p  -> P.Atomic(P.A.IsAlive p)
      | ForYears n -> P.Not(P.Atomic(P.A.Years n)) )

  let min d1 d2 = match d1,d2 with
    | ForYears n,_
    | _, ForYears n
      -> ForYears n
    | ForLife p, _
    | _, ForLife p
      -> ForLife p
    | InTail p, _
    | _, InTail p
      -> InTail p
    | _,_
      -> Absolute

  let events d = match d with
    | Absolute -> ExEventSet.empty
    | InTail _ -> ExEventSet.empty
    | ForLife p -> ExEventSet.singleton ( Event.Dies p )
    | ForYears n -> ExEventSet.singleton ( Event.YearsPass n )

end

type duration = Duration.t [@@deriving sexp]

module FullDuration = struct
  type t =
    { quantum : duration
    ; determinable : bool
    ; subsequent : bool
    ; executory : bool
    ; with_executory: bool    } [@@deriving sexp]

  let make duration =
    { quantum = duration ; determinable = false ;
      subsequent = false ; executory = false;
      with_executory = false    }

  let has_limit fd =
    fd.determinable || fd.subsequent || fd.executory || fd.with_executory

  let to_string fd =
    let buffer = Buffer.create 100 in
    let add = Buffer.add_string buffer in
    add (Duration.to_quantum fd.quantum);
    if fd.determinable then add " determinable";
    if fd.executory then add " subject to executory limitation";
    if fd.with_executory then add " with executory limitation";
    if fd.subsequent then add " subject to condition subsequent";
    Buffer.contents buffer

  let to_full_string fd owner =
    let buffer = Buffer.create 100 in
    let add = Buffer.add_string buffer in
    add (Duration.to_quantum fd.quantum);
    (match fd.quantum with
     | Duration.ForLife p when not Poly.(Holder.Person p = owner)
       -> add " for the life of "; add  (Person.to_string p)
     | _ -> ());
    if fd.determinable then add " determinable";
    if fd.executory then add " subject to executory limitation";
    if fd.with_executory then add " with executory limitation";
    if fd.subsequent then add " subject to condition subsequent";
    Buffer.contents buffer

end

module Source = struct

  type t =
    | Natural of Duration.t 
    | Added
    | Subsequent 
    | Precedent 
    | Executory
    | ImpliedReversion
    | If
    [@@deriving sexp]

  let to_string s = match s with
    | Natural _ -> "Natural Duration"
    | Added -> "Added Limitation"
    | Subsequent -> "Condition Subsequent"
    | Precedent -> "Condition Precedent"
    | Executory -> "Executory Limitation"
    | ImpliedReversion -> "Executory Limitation on Implied Reversion"
    | If -> "Condition Precedent / if"

  let to_duration source duration after_reversionary =
    let open FullDuration in
    match source with
    | Natural d  ->
      {duration with quantum = Duration.min d duration.quantum}
    | Added      ->
      if after_reversionary
      then {duration with determinable = true}
      else {duration with with_executory = true }
    | Precedent  -> {duration with executory = true}
    | Subsequent -> {duration with subsequent = true}
    | Executory  -> {duration with executory = true}
    | ImpliedReversion  -> {duration with executory = true}
    | If -> duration
end

type source = Source.t [@@deriving sexp]

module Divest = struct 
  type t = 
    | NoDivest
    | Springing
    | Shifting 
    | Termination [@@deriving compare, sexp]
  
  let to_string = function
    | NoDivest -> "shifting" (* This is the case for executory interests following fee simple determinables *)
    | Springing -> "springing"
    | Shifting -> "shifting"
    | Termination -> "terminating" (* For a right of entry *)
  end
type divest_t = Divest.t [@@deriving compare, sexp]

type remoteness =
  | RNone
  | RCondition
  | RRemote
  | RRemoteCondition
  | RNever [@@deriving sexp]


type share_t = Q.t  [@@deriving sexp]