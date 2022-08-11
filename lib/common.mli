
(* AM: mostly autogenerated mli *)

module Q :
  sig
    type t = int * int [@@deriving sexp]

    val zero : int * int
    val one : int * int
    val of_int : 'a -> 'a * int
    val to_int_exn : int * int -> int
    val num : 'a * 'b -> 'a
    val den : 'a * 'b -> 'b
    val gcd : int -> int -> int
    val reduce : int * int -> int * int
    val ( + ) : int * int -> int * int -> int * int
    val ( - ) : int * int -> int * int -> int * int
    val times : int * int -> int * int -> int * int
    val ( / ) : int * int -> int * int -> int * int
    val equal : int * int -> int * int -> bool
    val ( * ) : int * int -> int * int -> int * int
    val ( = ) : int * int -> int * int -> bool
  end
module Branch :
  sig
    type t = Primogeniture | PerCapita | PerStirpes | ByRepresentation [@@deriving sexp, yojson]

    val to_string : t -> string
    val of_string : string -> t
    val to_abbrev: t -> string
    val of_abbrev: string -> t
  end
type branch = Branch.t [@@deriving sexp, yojson]

module Property :
  sig
    type t = string [@@deriving sexp]
    val to_string : 'a -> 'a
  end
type property = Property.t [@@deriving sexp]

module Person :
  sig
    type t = Named of string | Anonymous [@@deriving sexp, hash]
    val is_anon : t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val of_string : string -> t
  end
type person = Person.t [@@deriving sexp]

module OtherHolder :
  sig
    module P = Person
    type t = Heirs of person | Spouse of person | Widow of person | Escheat  [@@deriving sexp]

    val persons : t -> person list
    val plural : t -> bool
    val to_string : t -> property
  end
type other_holder = OtherHolder.t [@@deriving sexp]

module Shares :
  sig
    type 'a t = 'a * Q.t  [@@deriving sexp]

    val holder : 'a * 'b -> 'a
    val share : 'a * 'b -> 'b
    val map : ('a * 'b) list -> g:('a -> 'c) -> ('c * 'b) list
    val total : ('a * (int * int)) list -> int * int
    val scale :
      int * int ->
      ('a, int * int) Base.List.Assoc.t -> ('a, int * int) Base.List.Assoc.t
    val normalize :
      ('a, int * int) Base.List.Assoc.t -> ('a, int * int) Base.List.Assoc.t
  end
type 'a shares = 'a Shares.t list [@@deriving sexp]

module Cotenancy :
  sig
    type t = Common | Joint | Entireties [@@deriving sexp]
    val to_string : t -> property
  end
type cotenancy = Cotenancy.t [@@deriving sexp]

module Description :
  sig
    module P = Person
    module Restriction :
      sig
        type t = Unrestricted | Graduate of property  [@@deriving sexp]
        val to_string : t -> bool -> property
      end
    type restriction = Restriction.t [@@deriving sexp]

    type t =
        Individuals of person list
      | Spouse of person
      | Widow of person
      | Descendants of person * branch
      | Heirs of person
      | Children of person
      | Grandchildren of person
      | Restriction of restriction * t [@@deriving sexp]

    val plural : t -> bool
    val to_string : t -> property
    val persons : t -> person list
    val root_of_title : t -> person option
    module R = Restriction
  end
type description = Description.t [@@deriving sexp]
module Holder :
  sig
    module P = Person
    module O = OtherHolder
    module D = Description
    type t =
        Person of person
      | Other of other_holder
      | Description of description [@@deriving sexp]
    val to_string : t -> property
    val persons : t -> person list
    val plural : t -> bool
    val ascertained : t -> bool
    val root_of_title : t -> person option
  end
type holder = Holder.t [@@deriving sexp]
