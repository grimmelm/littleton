open Core_kernel
module P = Program.DerivativeProgram

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

(** Utility functions for working with toml *)
let program t = Util.TOML.(
    let (>>|) = Option.(>>|) in
    let citation = get_string' "citation" t in
    let caption  = get_string' "caption" t in
    let comment  = get_string' "comment" t in
    let expected = get_nonempty_string' "expected" t >>| Sexp.of_string in
    let program  = get_string' "program" t |>
        function | Some p -> p
                 | None -> raise (InvalidTestFile "No program in test") in
    { citation; caption; comment; expected; program })

(** Build a test suite from an input source *)
let from_toml toml =
  let open Util.TOML in
  let title = get_string "title" toml in
  let reference = get_string' "reference" toml in
  let examples = get_table_array' "examples" toml in
  let ts = get_table_array' "tests" toml in
  let tests = (match examples, ts with
      | None, Some pgms
      | Some pgms, None -> List.map ~f:program pgms
      | _ -> [program toml] ) in
  { title; reference; tests }

let from_filename (filename:string) : suite =
  match Util.TOML.from_filename' filename with
    | `Ok t -> from_toml t
    | `Error(s,l) ->
      let msg = sprintf "Error at line %d: %s\n%!" l.Toml.Parser.line s in
      raise (InvalidTestFile msg)

(** Some useful test functions *)

(* Check for successful parsing *)
let parsing name test () =
  let open Alcotest in
  let result = match (P.parse' test.program) with
    | MParser.Success _ -> true
    | MParser.Failed(_,_) -> false in
  (check bool) name true result

(* Compare the expected result with the computed result *)
let expected name test () =
  let open Alcotest in
  let sexp = testable Sexp.pp Sexp.equal in
  let expect = Option.value test.expected ~default:(Sexp.Atom "") in
  let result = Result.(P.of_string test.program >>=
                       P.run) in
  match result with
  | Ok trace ->
    let computed = (List.last_exn trace) |> P.testable in
    (check sexp) name expect computed
  | Error _ ->
    Alcotest.fail "There was an error"

(** Apply functions to the test programs in a suite *)
let test (s:suite) ~f =
  let open Alcotest in
  let test_set = List.mapi s.tests ~f:(fun i test ->
      let name = match test.caption, test.comment with
        | Some n, _
        | None, Some n -> n
        | None, None -> sprintf "test %d" i in
      (name, `Quick, f name test)) in
  let argv = Array.of_list ["--verbose"] in
  run s.title ~argv [("Suite", test_set)]

let map (s:suite) ~f =
  List.map s.tests ~f:(fun test ->
      test.program, f test.program )

let iter (s:suite) ~f =
  List.iter s.tests ~f:(fun test -> f test.program)

let pick (s:suite) (i:int) ~f =
  match List.nth s.tests i with
  | Some t -> Some (f t.program)
  | None -> None
