open Semantics

module Json = Yojson

module type PROGRAM = sig

  (** An abstract type representing individual statements in the program *)
  type statement [@@deriving sexp]

  (** A program is a list of statements of abstract type *)
  type t = statement list [@@deriving sexp]

  (** A step is a single result after a single statement has been executed *)
  type step [@@deriving sexp]

  module Point : sig
    type t = (statement * step)
    val statement : t -> statement
    val step : t -> step
    val to_json : t -> Json.t
  end

  (** A point is tuple of a program statement and step reached upon executing it *)
  type point = Point.t [@@deriving sexp]

  (** The trace of a program is a list of points *)
  type trace = point list

  (** Programs can fail to execute correctly, resulting in an error *)
  type error

  (** The result of executing a program is either the successful trace, or an error *)
  type result = (trace, error) Core_kernel.Result.t

  val of_string : string -> (t, error) Core_kernel.Result.t
  val run : t -> result
  val export: result -> Json.t

  val inspect : result -> string
  val abstract : result -> string
  val error_msg : error -> string
  val testable : point -> Core_kernel.Sexp.t

  (* This is used for testing. Really shouldn't be exposed. *)
  val parse' : string -> Parser.t MParser.result
end

module MakeProgram : functor (S:SEMANTICS) -> PROGRAM
module DerivativeProgram : PROGRAM
