open Common
open Event
open Predicate

module P = PredicateType

exception ParseError of string

type symbol = (string, bytes list) MParser.t

type parser_duration =
  | Absolute
  | ForLife
  | ForLifePerson of person
  | InTail
  | ForYears of int
  | UnspecifiedDuration [@@deriving sexp]

type parser_cotenancy =
  | InCommon
  | Jointly
  | ByEntireties
  | UnspecifiedCotenancy  [@@deriving sexp]

type grant = description * parser_cotenancy * parser_duration [@@deriving sexp]

type combination =
  | Nothing
  | Single  of grant
  | CondPred       of  combination *  combination * P.t
  | CondPredWait   of  combination *  combination * P.t
  | After      of combination  * P.t
  | While   of combination *  P.t
  | Then    of combination * combination
  | But     of combination *  P.t  * combination
  | Reentry of combination *  P.t  [@@deriving sexp]

type 'a stmt =
  | Owns of person
  | Conveys of person * 'a
  | Wills of person * 'a
  | Occurs of event
  | Set of Parameters.t [@@deriving sexp]

type t = combination stmt list [@@deriving sexp]

val parse : string -> (t,string) result
val parse' : string -> t MParser.result
