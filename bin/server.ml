open Core
open Async
open Conveyances

module S = Cohttp_async.Server
module R = Cohttp_async.Request
module Json = Yojson
module Body = Cohttp_async.Body
module TOML = Util.TOML
(* module M = Interface.Make(Model.Tree) *)

(* Extract the path, split on slashes, and remove empty strings caused by
   repeated slashes *)
let split (req : R.t) : string list =
  List.filter ~f:(fun str -> not (String.is_empty str))
    (String.split_on_chars ~on:['/'; '?']
       (Uri.path (R.uri req)))

let graph_id = ref 0

let static parts =
  let open Core.Sys in
  let path = Filename.of_parts parts in
  match is_file path with
  | `Yes ->
    printf "Serving %s\n%!" path;
    let headers =
      if Filename.check_suffix path ".html" then
        Cohttp.Header.init_with "Content-Type" "text/html"
      else if Filename.check_suffix path ".json" then
        Cohttp.Header.init_with "Content-Type" "text/json"
      else if Filename.check_suffix path ".png" then
        Cohttp.Header.init_with "Content-Type" "image/png"
      else Cohttp.Header.init () in
    S.respond_with_file ~headers path
  | `No | `Unknown ->
    printf "Cannot find %s\n%!" path;
    S.respond `Not_found

let get_toml root name =
  let name = name ^ ".toml" in
  let path = Filename.of_parts [root; name] in
  match Core.Sys.is_file path with
  | `Yes -> ( match TOML.from_filename' path with
      | `Error(_,_) -> `Error (S.respond `Not_found)
      | `Ok toml -> `Ok toml )
  | _ -> `Error (S.respond `Not_found)

let examples root name =
  match get_toml root name with
  | `Error e -> e
  | `Ok toml ->
    if TOML.has "tests" toml then
      let open Json in
      let tests = TOML.get_table_array "tests" toml in
      let examples = List.mapi tests ~f:(fun index test ->
          let desc = match TOML.get_string' "description" test with
            | Some d -> d
            | None -> Int.to_string index in
          let program = TOML.get_string "program" test in
          `Assoc [ ( "id", `Int index)
                 ; ( "desc", `String desc)
                 ; ( "program", `String program) ] ) in
      let response = `String (to_string (`List examples)) in
      let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
      S.respond ~headers ~body:response `OK
    else
      S.respond `Not_found

let example root name id =
  match get_toml root name with
  | `Error e -> e
  | `Ok toml ->
    if TOML.has "tests" toml then
      let open Json in
      let tests = TOML.get_table_array "tests" toml in
      let example = List.nth_exn tests id in
      let text = TOML.get_string "program" example in
      let body = `String (to_string (`String text)) in
      let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
      S.respond ~headers ~body `OK
    else
      S.respond `Not_found

let chapters root =
  let dir = Core.Unix.opendir root in
  let rec info acc =
    match Core.Unix.readdir_opt dir with
    | None -> acc
    | Some s -> info (s::acc) in
  let entries = info [] in
  let chapters = List.foldi entries ~init:[] ~f:(fun i acc name ->
      let name,_ = Filename.split_extension name in
      match get_toml root name with
      | `Error _ -> acc
      | `Ok toml ->
        let id = TOML.get_default_int i "chapter" toml in
        let desc = TOML.get_default_string (sprintf "Chapter %d" id)
            "title" toml in
        (`Assoc [ ("id", `Int id); ("desc", `String desc); ("name", `String name)] )::acc) in
  let json = Json.to_string (`List chapters) in
  let body = `String json in
  let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
  S.respond ~headers ~body `OK


let dot string =
  let fname = "graphs/graph.dot" in
  let oname = sprintf "graphs/graph_%d.png" !graph_id in
  ignore( Core.Unix.mkdir_p "graphs" : unit);
  graph_id := !graph_id + 1;
  let outc = Out_channel.create fname in
  Out_channel.output_string outc string;
  Out_channel.close outc;
  let cmd = sprintf "dot -Tpng %s > %s" fname oname in
  ignore( Core.Sys.command_exn cmd : unit) ;
  oname

let interpret program =
  let open Program.DerivativeProgram in
  let result = Result.(of_string program >>= run) in
  let body = `String (Json.to_string (export result)) in
  let ok = match result with
    | Ok _ -> `OK
    | Error _ -> `Bad_request in
  let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
  S.respond ~headers ~body ok

let handle_request config
    ~(body : Body.t)
    (_ : Socket.Address.Inet.t)
    (request : R.t) : S.response Deferred.t =
  let root = TOML.get_string "examples" config in
  match request.R.meth, split request with
  | `POST, (["interpreter.html"] | ["interpreter.html"; _]) ->
    Body.to_string body >>= ( fun p -> interpret p )
  | `GET, ["chapters"] -> chapters root
  | `GET, ([] | [""] ) -> static ["index.html"]
  | `GET, [string] -> static [string]
  | `GET, parts -> static parts
  | _ ->
    S.respond `Not_found

let start ?(config="config.toml") (port : int) : unit =
  match TOML.from_filename' config with
  | `Ok toml ->
    if TOML.has "server" toml then
      let server_config = TOML.get_table "server" toml in
      let listener = (Async_unix.Tcp.Where_to_listen.of_port port) in
      let handler = handle_request server_config in
      ignore (S.create ~on_handler_error:`Raise listener handler 
              : (Socket.Address.Inet.t, 'a) S.t Deferred.t)
    else
      failwith "Configuration file has no server section"
  | `Error(s,_) ->
    failwith (sprintf "Could not read from configuration file %s: %s\n%!"
                config s)
