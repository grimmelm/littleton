open Core_kernel

(** This is an implementation of real numbers required for representing
 ** fractional ownerships. *)
module Q = struct
  type t = int * int [@@deriving sexp]
  let zero = (0,1)
  let one = (1,1)
  let of_int i = (i,1)
  let to_int_exn (a,b) = a / b
  let num (a,_) = a
  let den (_,b) = b
  let rec gcd a b = if b = 0 then a else gcd b (a mod b)
  let reduce (a,b) = let g = gcd a b in (a / g, b / g)
  let (+) (a,b) (c,d) = reduce (a * d + b * c, b * d)
  let (-) (a,b) (c,d) = reduce (a * d - b * c, b * d)
  let times (a,b) (c,d) = reduce (a * c, b * d)
  let ( / ) (a,b) (c,d) = reduce (a * d, b * c)
  let equal (a,b) (c,d) = a = c && b = d
  let ( * ) (a,b) (c,d) = times (a,b) (c,d)
  let ( = ) (a,b) (c,d) = equal (a,b) (c,d)
end

module Property = struct
  type t = string [@@deriving sexp]
  let to_string p = p
end
type property = Property.t [@@deriving sexp]

module Person = struct

  type t = Named of string | Anonymous [@@deriving sexp, hash ]

  let is_anon (p:t) : bool =
    match p with
    | Named _ -> false
    | Anonymous -> true

  let equal p1 p2 =
    match p1, p2 with
    | Named n1, Named n2 -> String.equal n1 n2
    | _, _ -> false

  let compare p1 p2 = match p1, p2 with
    | Anonymous, Anonymous -> 0
    | Named s1, Named s2 -> String.compare s1 s2
    | Named _, Anonymous -> 1
    | Anonymous, Named _ -> -1

  let to_string (p:t): string =
    match p with
    | Named n -> n
    | Anonymous -> "Anonymous"

  let of_string (s:string):t = Named s

end
type person = Person.t [@@deriving sexp ]

module OtherHolder = struct
  module P = Person

  type t =
    | Heirs of person
    | Spouse of person
    | Widow of person
    | Escheat [@@deriving sexp ]

  let persons = function
    | Heirs p -> [p]
    | Spouse p -> [p]
    | Widow p -> [p]
    | Escheat -> []

  let plural = function
    | Heirs _ -> true
    | Spouse _ -> false
    | Widow _ -> false
    | Escheat -> false

  let to_string = function
    | Heirs p -> (P.to_string p) ^ "'s (unknown) heirs"
    | Spouse p -> (P.to_string p) ^ "'s (unknown) spouse"
    | Widow p -> (P.to_string p) ^ "'s (unknown) surviving spouse"
    | Escheat -> "The state (via escheat)"

end
type other_holder = OtherHolder.t [@@deriving sexp ]

module Shares = struct
  open Q
  type 'a t = 'a * Q.t [@@deriving sexp ]
  let holder t = fst t
  let share t = snd t
  let map l ~g = List.map l ~f:(fun (t,n) -> (g t,n))
  let total l = List.fold (snd (List.unzip l)) ~init:zero ~f:(+)
  let scale q l = List.Assoc.map l ~f:(fun s -> q * s )
  let normalize l = scale (one / (total l)) l
end
type 'a shares = 'a Shares.t list [@@deriving sexp ]

module Cotenancy = struct
  type t =
    | Common
    | Joint
    | Entireties [@@deriving sexp ]

  let to_string = function
    | Common -> "in common"
    | Joint -> "jointly"
    | Entireties -> "by the entireties"  [@@deriving sexp ]
end
type cotenancy = Cotenancy.t [@@deriving sexp ]

module Branch = struct
  type t =
    | Primogeniture
    | PerCapita
    | PerStirpes
    | ByRepresentation [@@deriving sexp, yojson ]

  let to_string = function
    | Primogeniture -> "by primogeniture"
    | PerCapita -> "per capita"
    | PerStirpes -> "per stirpes"
    | ByRepresentation -> "by representation"

  let of_string s = match (String.lowercase s) with
    | "by primogeniture" -> Primogeniture
    | "per capita" -> PerCapita
    | "per stirpes" -> PerStirpes
    | "by representation" -> ByRepresentation
    | _ ->  raise (Invalid_argument ("Invalid value " ^ s ^ " for intestancy branch type "))

  let to_abbrev = function
    | Primogeniture -> "p"
    | PerCapita -> "pc"
    | PerStirpes -> "ps"
    | ByRepresentation -> "br"

  let of_abbrev s = match (String.lowercase s) with
    | "p" -> Primogeniture
    | "pc" -> PerCapita
    | "ps" -> PerStirpes
    | "br" -> ByRepresentation
    | _ ->  raise (Invalid_argument ("Invalid value " ^ s ^ " for intestancy branch type "))
end
type branch = Branch.t [@@deriving sexp, yojson ]

module Description = struct
  module P = Person

  module Restriction  = struct
    type t =
      | Unrestricted
      | Graduate of string [@@deriving sexp ]

    let to_string r plural = match r with
      | Unrestricted -> ""
      | Graduate i -> (if plural then "who  graduate" else "if they graduate") ^ " " ^ i
  end
  type restriction = Restriction.t [@@deriving sexp ]

  type t =
    | Individuals of person list
    | Spouse of person
    | Widow of person
    | Descendants of person * branch
    | Heirs of person
    | Children of person
    | Grandchildren of person
    | Restriction of restriction * t [@@deriving sexp]

  let rec plural = function
    | Individuals ps -> List.length ps > 1
    | Spouse _ | Widow _ -> false
    | Descendants _ | Heirs _ | Children _ | Grandchildren _ -> true
    | Restriction (_,d) -> plural d

  let rec to_string = function
    | Individuals ps -> String.concat ~sep:" and " (List.map ps ~f:(fun p -> P.to_string p))
    | Spouse p -> P.to_string p ^ "'s spouse"
    | Widow p ->  P.to_string p ^ "'s surviving spouse"
    | Descendants (p,b)-> P.to_string p ^ "'s descendants " ^ Branch.to_string b
    | Heirs p  -> P.to_string p ^ "'s heirs"
    | Children p -> P.to_string p ^ "'s children"
    | Grandchildren p -> P.to_string p ^ "'s grandchildren"
    | Restriction (r,d) -> to_string d ^ " " ^ Restriction.to_string r (plural d)

  let rec persons = function
    | Spouse p | Widow p -> [p]
    (* This is minimal but okay for now *)
    | Descendants (p,_) | Heirs p | Children p | Grandchildren p -> [p]
    | Individuals ps -> ps
    | Restriction (_,d) -> persons d

  let rec root_of_title = function
    | Heirs _ -> None
    | Individuals [p] -> Some p
    | Restriction (_,d) -> root_of_title d
    | _ -> None

  module R = Restriction

end
type description = Description.t [@@deriving sexp ]


module Holder = struct
  module P = Person
  module O = OtherHolder
  module D = Description

  type t =
    | Person of person
    | Other of other_holder
    | Description of description [@@deriving sexp ]

  let to_string = function
    | Person p -> P.to_string p
    | Other o -> O.to_string o
    | Description d -> D.to_string d

  let persons = function
    | Person p -> [p]
    | Other p -> O.persons p
    | Description d -> D.persons d

  let plural = function
    | Person _ -> false
    | Other p -> O.plural p
    | Description d -> D.plural d

  let ascertained = function
    | Person _ | Other _ -> true
    | Description _ -> false

  let root_of_title = function
    | Person p -> Some p
    | Other _ -> None
    | Description d -> D.root_of_title d

end
type holder = Holder.t [@@deriving sexp ]
