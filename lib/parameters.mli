module Verbosity : sig

  type t =
    | Verbose
    | Brief
    | Superbrief [@@deriving sexp, yojson ]

  val to_string : t -> string
  val of_string : string -> t

  val to_abbrev: t -> string
  val of_abbrev : string -> t

end
type verbosity = Verbosity.t [@@deriving sexp, yojson ]

module AmbiguousInterest : sig
  type t =
    | FeeSimple
    | LifeEstate [@@deriving sexp, yojson ]

  val to_string : t -> string
  val of_string : string -> t

  val to_abbrev: t -> string
  val of_abbrev : string -> t
end

module AmbiguousTenancy : sig

  type t =
    | InCommon
    | Joint [@@deriving sexp, yojson ]

  val to_string : t -> string
  val of_string : string -> t

  val to_abbrev: t -> string
  val of_abbrev : string -> t
end

(* Keep this in alphabetical order for easier updates *)
type t =
  | AmbiguousInterest of AmbiguousInterest.t
  | AmbiguousTenancy of AmbiguousTenancy.t
  | DefaultDistributionRule of Common.branch
  | DestructibilityOfContintentRemainders of bool
  | DisentailByConveyance of bool
  | DoctrineOfWorthierTitle of bool
  | HideImpliedReversions of bool
  | ImplyUnspecifiedHeirs of bool
  | InitialOwner of string
  | IntestacyBranchType of Common.branch
  | RuleAgainstPerpetuities of bool
  | RuleInShelleysCase of bool
  | Verbosity of verbosity [@@deriving sexp, yojson ]

val to_string : t -> string
val set : t -> unit

(* Keep this in alphabetical order for easier updates *)
val ambiguous_interest : AmbiguousInterest.t ref
val ambiguous_tenancy : AmbiguousTenancy.t ref
val default_distribution_rule : Common.branch ref
val destructibility_of_contingent_remainders : bool ref
val disentail_by_conveyance : bool ref
val doctrine_of_worthier_title : bool ref
val hide_implied_reversions : bool ref
val imply_unspecified_heirs : bool ref
val initial_owner : string ref
val intestacy_branch_type : Common.branch ref
val rule_against_perpetuities : bool ref
val rule_in_shelleys_case : bool ref
val verbosity : verbosity ref


module Interface : sig

  type options = Boolean | OneOf of (string * string) list | String [@@deriving yojson]
  type current = Yes | No | Value of string [@@deriving yojson]

  type param = { name: string
               ; desc : string
               ; abbrev : string
               ; options : options
               ; current : current
               } [@@deriving yojson]

  val parameters : param list

  val of_abbrev : string -> string -> t

  val get : string -> t
end
