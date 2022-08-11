open Js_of_ocaml
open Conveyances
open Program.DerivativeProgram

let interpret str =
  Core_kernel.Result.(of_string str >>= run |> export)

let _ = Js.export "littleton"
    (object%js

      method interpret str : string =
        str |> Js.to_string |> interpret |> Yojson.to_string

      method parameters () : string =
        let open Core_kernel in
        let params = Parameters.Interface.parameters in
        let json_params = `List (List.map ~f:(Parameters.Interface.param_to_yojson) params) in
        Yojson.Safe.to_string json_params

      method setParameter p v: unit =
        let param = Js.to_string p in
        let value = Js.to_string v in

        let parameter = Parameters.Interface.of_abbrev param value in
        Parameters.set parameter

      method getParameter p =
        let param = Js.to_string p in
        Parameters.Interface.get param

    end)
