open Core

module Test = Conveyances.Test
module Util = Conveyances.Util
module TOML = Util.TOML
module Ex = Conveyances.Example
module Parser = Conveyances.Parser
module Common = Conveyances.Common
module Semantics = Conveyances.Semantics
module Program = Conveyances.Program

let file = Filename.arg_type

let test_mode =
  Command.Arg_type.create
    (fun string -> match string with
       | "expected" -> Test.Expected
       | "parsing" -> Test.Parsing
       | _ -> eprintf "%s is not a valid test mode\n%!" string; exit 1)

let common = Command.Let_syntax.(
    let%map_open
      debug = flag "-d" no_arg ~doc:" Debug mode" and
    verbose = flag "-v" no_arg ~doc:" Verbose output"
    in ( debug, verbose ))

let run =
  Command.basic
    ~summary: "Run an example from a file"
    Command.Let_syntax.(
      let%map_open
        filename = anon ( "filename" %: file )
      and index = anon ( maybe ("index" %: int) )
      and paper = flag "--paper" no_arg ~doc:" show terms in a paper-friendly form"
      and json = flag "--json" no_arg ~doc:" show terms in JSON" in
      fun () ->
        let open Program.DerivativeProgram in
        let f s =
            let result = Result.(of_string s >>= run) in
            ( match result with
            | Ok _ ->
              if paper then result |> abstract |> print_endline
              else if json then result |> export |> Yojson.to_string |> print_endline
              else result |> inspect |> print_endline
            | Error m -> print_endline (error_msg m) ) in
        let suite = Test.from_filename filename in
        match index with
        | None -> Test.iter ~f suite
        | Some i -> (match Test.pick suite i ~f with
            | None -> printf "File %s has no test at index %d\n" filename i
            | Some _ -> ()))

let test =
  Command.basic
    ~summary:"Run the test cases from the given file"
    Command.Let_syntax.(
      let%map_open
        mode = flag "-m"
          (optional_with_default Test.Expected test_mode)
          ~doc:"Testing mode"
      and filename = anon ("filename" %: file) in
      fun () ->
        let open Test in
        let suite = from_filename filename in
        let f = match mode with
          | Parsing -> parsing
          | Expected -> expected in
        test ~f suite)

let server =
  Command.basic
    ~summary:"Start the HTTP server on the given port"
    Command.Let_syntax.(
      let%map_open
        port = anon ("port" %: int) in
      fun () ->
        ignore (Server.start port : unit);
        never_returns (Async.Scheduler.go ()))

let jsonify =
  let to_json toml filename =
    match Ex.of_toml toml with
    | None -> Printf.eprintf "TOML in file %s is does not have required entries"
                filename
    | Some e ->
      let example = Ex.to_json e in
      let out_file = String.(^) (Filename.chop_extension filename) ".json" in
      let out_channel = Out_channel.create out_file in
      Yojson.Safe.pretty_to_channel out_channel example ;
      Out_channel.close out_channel in
  Command.basic
    ~summary:"Convert a TOML example file to JSON for website deployment"
    Command.Let_syntax.(
      let%map_open
        filename = anon ( "filename" %: file ) in
      fun () ->
        let msg = sprintf "Could not read TOML file: %s\n" filename in
        match Core.Sys.is_file filename with
        | `Yes -> ( match TOML.from_filename' filename with
            | `Error(_,_) -> Printf.eprintf "%s" msg
            | `Ok toml -> to_json toml filename )
        | _ -> Printf.eprintf "%s" msg
    )

let main =
  Command.group
    ~summary:"Run the given subcommand."
    [ ("run", run )
    ; ("jsonify", jsonify)
    ; ("server" , server)
    ; ("test"   , test) ]

let () =
  Command.run ~version:"0.3" main
