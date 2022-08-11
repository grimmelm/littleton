open Common
open Event
open Classify

module type INTEREST = sig
  type t [@@deriving sexp]

  val self : person -> t
  val owner : t -> holder
  val grantor : t -> person
  val grantee : t -> description
  val history : t -> history
  val create : person -> description -> t
  val transfer_to : t -> holder -> t
  val create_vested : person -> description -> holder -> t
  val add_event : t -> event -> t
  val replace_history : t -> history -> t
  val is_reversionary : t -> bool
  val to_string : t -> string
  val to_abstract : t -> string
end

type unnamed_interest = 
  { grantor: person
  ; grantee: description
  ; owner: holder
  ; history : history} [@@deriving sexp]

type named_interest  =
  { grantor   : person
  ; grantee   : description
  ; owner     : holder 
  ; history   : history
  ; reachable : bool
  ; quantum   : Duration.t
  ; immediate : bool
  ; divest    : divest_t
  ; afterfsd  : bool
  ; wait      : bool
  ; scp       : bool
  ; nature    : Nature.t
  ; duration  : FullDuration.t
  ; vesting   : Vesting.t 
  ; share     : share_t
  ; remote    : remoteness} [@@deriving sexp]

module Unnamed : INTEREST with type t = unnamed_interest [@@deriving sexp]
module Named : INTEREST with type t = named_interest [@@deriving sexp]