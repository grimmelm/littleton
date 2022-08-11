open Core_kernel
open Event
open Semantics
open Parser

module Json = Yojson

module type PROGRAM = sig

  type statement [@@deriving sexp]
  type t = statement list [@@deriving sexp]
  type step [@@deriving sexp]

  module Point : sig
    type t = (statement * step)
    val statement : t -> statement
    val step : t -> step
    val to_json : t -> Json.t
  end

  type point = Point.t [@@deriving sexp]
  type trace = point list
  type error
  type result = (trace, error) Core_kernel.Result.t

  val of_string : string -> (t, error) Core_kernel.Result.t
  val run : t -> result
  val export: result -> Json.t

  val inspect : result -> string
  val abstract : result -> string
  val error_msg : error -> string
  val testable : point -> Sexp.t

  val parse' : string -> Parser.t MParser.result
end

module MakeProgram (S:SEMANTICS) = struct

  type thunk = S.t [@@deriving sexp]
  type term = S.term [@@deriving sexp]

  type statement = term stmt [@@deriving sexp]

  type t = statement list [@@deriving sexp]

  module Point = struct

    (** A step is a single result after a single statement has been executed *)
    type step = term [@@deriving sexp]

    (** A point is tuple of a program statement and step reached upon executing it *)
    type t = (statement * term) [@@deriving sexp ]

    let statement (s,_) = s
    let step (_, t) = t

    let sexp_of_t (s, t) =
      let s' = sexp_of_statement s in
      let t' = S.name t |> S.sexp_of_named_term in
      Sexp.List [s'; t']

    let t_of_sexp sexp = match sexp with
      | Sexp.List [s;t] ->
        let s' = statement_of_sexp s in
        let t' = S.named_term_of_sexp t |> S.unname in
        (s',t')
      | _ ->
        let msg = sprintf "Cannot parse S-expresion: %s\n" (Sexp.to_string sexp) in
        raise (Parser.ParseError msg)

    let of_stmt s = match s with
      | Owns p -> sprintf "%s owns the property" (Common.Person.to_string p)
      | Conveys(p,_) -> sprintf "Conveyance by %s" (Common.Person.to_string p)
      | Wills(p,_) -> sprintf "Will by %s" (Common.Person.to_string p)
      | Occurs e -> Event.to_string e
      | Set (p) -> sprintf "Set %s" (Parameters.to_string p)

    let to_testable (_,t) = S.U.sexp_of_t t

    let to_json (s,t) : Json.t =
      `Assoc [ ("statement", `String (of_stmt s))
             ; ("step", S.name t |> S.N.to_json )]

    let to_abstract (s, t) =
      sprintf "Statement:\t%s\nTerm:\t%s\n"
        (of_stmt s) (S.U.to_abstract t)

    (* let possibilities (s, _) : Json.t = match s with
     *   | Owns _ -> `List[ `Assoc ["event",`String "Conveys"; "count", `Int 2]]
     *   | Conveys _ -> `List [`Assoc ["event", `String "Conveys"; "count", `Int 2]]
     *   | Wills _ -> `List [`Assoc ["event", `String "Wills"; "count", `Int 2]]
     *   | Occurs e ->  Event.possibilties e
     *   | Set _ -> `Null *)

    (* let person_of_point (s, _) = match s with
     *   | Owns p -> [`String p]
     *   | Conveys (p, t') -> `String p :: (List.map ~f: (fun a -> `String a)
     *                                       (S.U.persons_of_term t'))
     *   | Wills   (p, t') -> `String p :: (List.map ~f: (fun a -> `String a)
     *                                        (S.U.persons_of_term t'))
     *   | Occurs e ->  (e |> Event.get_person
     *                   |> List.map ~f: (fun x -> `String x)) 
     *   | Set _ -> [] *)
  end

  (** A step is a single result after a single statement has been executed *)
  type step = Point.step [@@deriving sexp]

  (** A point is tuple of a program statement and step reached upon executing it *)
  type point = Point.t [@@deriving sexp]

  (** The trace of a program is a list of points *)
  type trace = point list

  (** Programs can fail to execute correctly, resulting in an error *)
  type error = string

  (** The result of executing a program is either the successful trace, or an
  error *)
  type result = (trace, error) Core_kernel.Result.t

  let inspect (r:result) = match r with
      | Ok t ->
        let points = List.map t
            ~f:(fun p -> Point.to_testable p |> Sexp.to_string_hum) in
        String.concat ~sep:"\n" points
      | Error e -> e

  let abstract (r:result) = match r with
    | Ok t ->
      String.concat ~sep:"\n" (List.map t ~f:Point.to_abstract)
    | Error e -> e

  (* let get_future = function
   *   | [] -> `Null
   *   | h :: _ -> Point.possibilities h *)

  let export (r:result) = match r with
    | Ok t ->
      let points = List.map t ~f:(Point.to_json) in
      `Assoc [ ("success", `List points)]
    | Error e -> `Assoc [ ("error", `String e) ]
  (* let people = List.fold t ~init:[]
     *       ~f: (fun acc x -> (Point.person_of_point x) @ acc) in
     * let future = t |> List.rev |> get_future in
     * let uniq_people = (List.dedup_and_sort ~compare:Poly.compare people) in
     * `Assoc [ ("result", `List json); ("people", `List uniq_people);
     *          "events", future] *)

  let error_msg e = e
  let testable (p:point) = Point.to_testable p

  let to_point (s:statement) (t:thunk) : point =
    (s, S.unlift t)

  let transform (t:thunk) (s:statement) : thunk = match s with
    | Occurs e -> S.occurs t e
    | Conveys(p,term) -> S.conveys t p term
    | Wills(p,term) -> S.makes_will t p term
    | Owns p -> S.owns t p
    | Set(p) -> Parameters.set p; t

  let run (pgm:t) : result =
    let run' thunk cs =
      let trace, _ = List.fold cs ~init:([],thunk)
          ~f:(fun (trace,thunk) stmt ->
              let thunk' = transform thunk stmt in
              let point = to_point stmt thunk' in
              (point::trace, thunk')) in
      List.rev trace in
    Ok (run' S.init pgm)

  let to_program (p:Parser.t) : t =
    List.map p ~f:(fun stmt -> match stmt with
        | Owns p   -> Owns p
        | Occurs e -> Occurs e
        | Conveys(p,c) -> Conveys (p, S.translate c p)
        | Wills(p,c) -> Wills (p, S.translate c p)
        | Set(p) -> Set(p))


  let of_string string = Result.( parse string >>| to_program )

  let parse' = parse'
end

module DerivativeProgram = MakeProgram(Derivatives)
