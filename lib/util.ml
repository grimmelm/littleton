open Core_kernel

module TOML = struct
  let from_filename f =
    match Toml.Parser.from_filename f with
    | `Ok t -> t
    | `Error(s,_) ->
      failwith (sprintf "Could not read file %s: %s\n%!" f s)

  let from_filename' f = Toml.Parser.from_filename f

  let force opt =
    match opt with
    | Some value  -> value
    | None        -> failwith "No value"

  let has k toml_table =
    Toml.Lenses.(match get toml_table (key k) with
        | Some _ -> true
        | None -> false )

  let get_string k toml_table =
    Toml.Lenses.(get toml_table (key k |-- string)) |> force

  let get_string' k toml_table =
    Toml.Lenses.(get toml_table (key k |-- string))

  let get_nonempty_string' k toml_table =
    match get_string' k toml_table with
    | None -> None
    | Some s -> if String.equal (String.strip s) "" then None else Some s

  let get_default_string def k toml_table =
    match get_string' k toml_table with
    | Some s -> s
    | None -> def

  let get_int k toml_table =
    Toml.Lenses.(get toml_table (key k |-- int)) |> force

  let get_int' k toml_table =
    Toml.Lenses.(get toml_table (key k |-- int))

  let get_default_int def k toml_table =
    match get_int' k toml_table with
    | Some i -> i
    | None -> def

  let get_float k toml_table =
    Toml.Lenses.(get toml_table (key k |-- float)) |> force

  let get_bool k toml_table =
    Toml.Lenses.(get toml_table (key k |-- bool)) |> force

  let get_bool_array k toml_table =
    Toml.Lenses.(get toml_table (key k |-- array |-- bools)) |> force

  let get_table k toml_table =
    Toml.Lenses.(get toml_table (key k |-- table)) |> force

  let get_table_array k toml_table =
    Toml.Lenses.(get toml_table (key k |-- array |-- tables)) |> force

  let get_table_array' k toml_table =
    Toml.Lenses.(get toml_table (key k |-- array |-- tables))

  let iter_over k toml_table ~f =
    match get_table_array' k toml_table with
    | Some value -> List.iter value ~f
    | None -> ()
end
