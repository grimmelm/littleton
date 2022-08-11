open Core_kernel
open Common

module Interval = struct
  type t = int [@@deriving sexp]

  let to_string i = sprintf "%i years" i
end
type interval = Interval.t  [@@deriving sexp]

module Event = struct

  type t =
    | Empty
    | Dies of person
    | ConveysAway of person
    | Marry of person * person option
    | Divorce of person * person option
    | Child of person * person
    | Graduates of person * string
    | BecomesState of string
    | Consumes of person * string
    | IsUsedAs of string
    | FailureOfIssue of person
    | YearsPass of int
    | Reenters of person
    | Start
    | Generic of string [@@deriving sexp]

  let possibly_remote = function
    | Empty -> false
    | Dies _ -> false
    | ConveysAway _ -> true
    | Marry (_,_) -> false
    | Divorce (_,_) -> false
    | Child (_,_) -> false
    | Graduates (_,_) -> false
    | BecomesState _ -> true
    | Consumes (_,_) -> false
    | IsUsedAs (_) -> true
    | FailureOfIssue _ -> true
    | YearsPass y -> y  > 21
    | Reenters _ -> true
    | Start -> false
    | Generic _ -> true

  let to_string e = 
    let module P =  Person in
    match e with
    | Empty -> sprintf "empty event"
    | Dies p -> sprintf "%s dies" (P.to_string p)
    | ConveysAway p -> sprintf "%s conveys" (P.to_string p)
    | Marry (p1, None) -> sprintf "%s marries" (P.to_string p1)
    | Marry (p1, Some p2) -> sprintf "%s and %s marry" (P.to_string p1) (P.to_string p2)
    | Divorce (p1, None) -> sprintf "%s divorces" (P.to_string p1)
    | Divorce (p1, Some p2) -> sprintf "%s and %s divorce" (P.to_string p1) (P.to_string p2)
    | Child (p1,p2) -> sprintf "%s has child %s" (P.to_string p1) (P.to_string p2)
    | Graduates (p,i) -> sprintf "%s graduates %s" (P.to_string p) i
    | BecomesState (s) -> sprintf "%s becomes a state" s
    | Consumes (p,s) -> sprintf "%s consumes %s" (P.to_string p) s
    | IsUsedAs (s) -> sprintf "the property is used as %s" s
    | FailureOfIssue p -> sprintf "failure of issue of %s" (P.to_string p)
    | YearsPass n -> sprintf "%d years pass" n
    | Reenters p -> sprintf "%s reenters" (P.to_string p)
    | Start -> "becomes possessory"
    | Generic s -> s

  let (=) e1 e2 = match e1, e2 with
    | Marry(a,Some b), Marry(c,Some d) when Poly.(a = d && b = c)  -> true
    | Divorce(a, Some b), Divorce(c, Some d) when Poly.(a = d && b = c) -> true
    | _,_ -> Poly.(e1 = e2)

  (* let convert_to_JSON_String s = `String s   *)

  (** Association list of possible events and number of people needed *)
  let all_possibilities = [
    "dies", 1;
    "conveys", 2;
    "marries", 1;
    "divorces", 1;
  ]

  (** [json_of_list lst] is a json representing (lst : (string * int) list) *)
  let json_of_list lst =
    List.map lst
      ~f: (fun (a, b) -> `Assoc (["event", `String a; "count", `Int b]))

  (** [possibility_of_string s] is a json representing all possible actions
      except for [s] *)
  let possibility_of_string s =
    `List (s |> List.Assoc.remove all_possibilities ~equal:Poly.(=)
           |> json_of_list)

  let possibilities = function
    | Empty ->  `List []
    | Dies _ -> possibility_of_string "dies"
    | ConveysAway _ ->  possibility_of_string "conveys"
    | Marry _ -> possibility_of_string "marries"
    | Divorce _ -> possibility_of_string "divorces"
    | Child _ -> `List []
    | Graduates _ -> `List []
    | BecomesState _ -> `List []
    | Consumes _ -> `List []
    | IsUsedAs _ ->  `List []
    | FailureOfIssue _ -> `List []
    | YearsPass _ ->  `List []
    | Reenters _ ->  `List []
    | Start -> `List []
    | Generic _ -> `List []

  let get_person = function
    | Empty -> []
    | Dies p -> [p]
    | ConveysAway p -> [p]
    | Marry (p1, None) -> [p1]
    | Marry (p1, Some p2) -> [p1; p2]
    | Divorce (p1, None) -> [p1]
    | Divorce (p1, Some p2) -> [p1; p2]
    | Child (p1,p2) -> [p1; p2]
    | Graduates (p,_) -> [p]
    | BecomesState (_) -> []
    | Consumes (p,_) -> [p]
    | IsUsedAs (_) -> []
    | FailureOfIssue p -> [p]
    | YearsPass _ -> []
    | Reenters p -> [p]
    | Start -> []
    | Generic _ -> []

end
type event = Event.t [@@deriving sexp]
type history = event list [@@deriving sexp]

module EventSet = struct
  module E = Event

  module M = Set.Make(struct
      type t = E.t [@@deriving sexp]
      let compare = Stdlib.compare
    end)

    include M

  let to_string ?(sep="\n") es =
    let strings = M.fold es ~init:[] ~f:(fun acc e -> (E.to_string e)::acc) in
    String.concat ~sep (List.rev strings)

  let rec permutations es =
    if M.is_empty es then [ [] ]
    else
      M.fold es ~init:[] ~f:(fun acc e ->
          let es' = M.remove es e in
          let ps = permutations es' in
          List.fold ps ~init:acc ~f:(fun acc p -> (e::p)::acc))
end
module ExEventSet = EventSet

type 'a will_list = (person * 'a) list [@@deriving sexp]

module Wills = struct
  type 'a t = 'a will_list [@@deriving sexp]

  let add_will p will wills =
     List.Assoc.add wills ~equal:Poly.(=) p will
  let will_of p wills  =
    List.Assoc.find wills ~equal:Poly.(=) p  
end
