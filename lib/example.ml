open Core_kernel

type example =
  {
    desc: string option [@key "description"]
  ; result: string option
  ; program: string } [@@deriving yojson]

type t =
  | Example of example
  | Examples of { title : string ; examples: example list } [@@deriving yojson]

let to_json = to_yojson
let of_json = of_yojson

let to_toml_ex e =
  let eg = [(Toml.Min.key "program", Toml.Types.TString e.program)] in
  let eg' = match e.desc with
    | Some d -> ( Toml.Min.key "description", Toml.Types.TString d )::eg
    | None -> eg in
  let eg'' = match e.result with
    | Some r -> ( Toml.Min.key "result", Toml.Types.TString r ):: eg'
    | None -> eg' in
  Toml.Min.of_key_values eg''

let to_toml t = match t with
  | Example e -> to_toml_ex e
  | Examples e ->
    let es = Toml.Types.NodeTable ( List.map e.examples ~f:to_toml_ex ) in
    Toml.Min.of_key_values [ (Toml.Min.key "title", Toml.Types.TString e.title)
                       ; (Toml.Min.key "examples", Toml.Types.TArray es)]

let of_toml_ex toml =
  let open Util.TOML in
  let desc = get_string' "desc" toml in
  let result = get_string' "result" toml in
  match get_string' "program" toml with
  | Some program -> Some { desc; result; program }
  | None -> None

let of_toml toml = match Util.TOML.get_string' "title" toml with
  | Some title ->
    let ts = Util.TOML.get_table_array "examples" toml in
    let es = List.map ts ~f:of_toml_ex in
    let es' = List.fold_until es ~init:[]
        ~f:(fun egs ex -> match ex with
            | Some e -> Continue (e::egs)
            | None -> Stop egs )
        ~finish:(fun x -> x) in
    let examples = List.rev es' in
    Some (Examples { title ; examples })
  | None -> ( match (of_toml_ex toml) with
    | Some e -> Some (Example e)
    | None -> None  )
