open Term
open Common
open Event

module type SEMANTICS = sig

  module U : TERM
  module N : TERM

  type t [@@deriving sexp]
  type term =  U.t [@@deriving sexp]
  type named_term = N.t [@@deriving sexp]

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

module Derivatives : SEMANTICS
