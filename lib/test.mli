open Core_kernel

type test = { citation : string option
            ; caption  :  string option
            ; comment : string option
            ; expected : Sexp.t option
            ; program  :  string} [@@deriving sexp]

type suite = { title : string
             ; reference : string option
             ; tests : test list } [@@deriving sexp]

type tester = string -> test -> unit -> unit

type mode = Parsing | Expected

exception InvalidTestFile of string

(** Build a test suite from an input source *)
val from_filename : string -> suite
val from_toml : Toml.Types.table -> suite

(** Some useful test functions *)
val parsing : tester
val expected : tester

(** Apply functions to the test programs in a suite *)
val test : suite -> f:tester -> unit
val map  : suite -> f:(string -> 'a) -> (string * 'a) list
val iter : suite -> f:(string -> unit) -> unit
val pick : suite -> int -> f:(string -> 'a) -> 'a option
