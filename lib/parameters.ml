open Core_kernel
open Common

exception InvalidParameterValue of string
exception InvalidParameter of string

module Verbosity = struct
  type t =
    | Verbose
    | Brief
    | Superbrief [@@deriving sexp, yojson ]

  let to_string = function
    | Verbose -> "full descriptions including vesting"
    | Brief -> "brief descriptions without vesting"
    | Superbrief -> "owner only"

  let of_string s = match (String.lowercase s) with
    | "verbose" -> Verbose
    | "brief" -> Brief
    | "superbrief" -> Superbrief
    | _ ->  raise (InvalidParameterValue ("Invalid value " ^ s ^ " for verbosity"))

  let to_abbrev = function
    | Verbose -> "v"
    | Brief -> "b"
    | Superbrief -> "sb"


  let of_abbrev s = match (String.lowercase s) with
    | "v" -> Verbose
    | "b" -> Brief
    | "sb" -> Superbrief
    | _ ->  raise (InvalidParameterValue ("Invalid value " ^ s ^ " for verbosity"))

end
type verbosity = Verbosity.t [@@deriving sexp, yojson ]

module AmbiguousInterest = struct

  type t =
    | FeeSimple
    | LifeEstate [@@deriving sexp, yojson ]

  let to_string  t = match t with
    | FeeSimple -> "Fee Simple"
    | LifeEstate -> "Life Estate"

  let of_string s = match (String.lowercase s) with
    | "fee simple" -> FeeSimple
    | "life estate" -> LifeEstate
    | _ -> raise (InvalidParameterValue ("Invalid value " ^ s ^ " for ambiguous interest"))

  let to_abbrev t = match t with
    | FeeSimple -> "fs"
    | LifeEstate -> "le"

  let of_abbrev s = match (String.lowercase s) with
    | "fs" -> FeeSimple
    | "le" -> LifeEstate
    | _ -> raise (InvalidParameterValue ("Invalid value " ^ s ^ " for ambiguous interest"))

end

module AmbiguousTenancy = struct

  type t =
    | InCommon
    | Joint [@@deriving sexp, yojson ]

  let to_string  t = match t with
    | InCommon -> "tenancy in common"
    | Joint -> "joint tenancy"

  let of_string s = match (String.lowercase s) with
    | "common" -> InCommon
    | "joint" -> Joint
    | _ -> raise (InvalidParameterValue ("Invalid value " ^ s ^ " for ambiguous tenancy"))

  let to_abbrev t = match t with
    | InCommon -> "ic"
    | Joint -> "j"

  let of_abbrev s = match (String.lowercase s) with
    | "ic" -> InCommon
    | "j" -> Joint
    | _ -> raise (InvalidParameterValue ("Invalid value " ^ s ^ " for ambiguous tenancy"))

end

let initial_owner = ref "O"
let ambiguous_interest = ref AmbiguousInterest.FeeSimple
let ambiguous_tenancy = ref AmbiguousTenancy.InCommon
let default_distribution_rule = ref Branch.ByRepresentation
let destructibility_of_contingent_remainders = ref true
let doctrine_of_worthier_title = ref true
let rule_in_shelleys_case = ref true
let rule_against_perpetuities = ref true
let disentail_by_conveyance = ref true
let imply_unspecified_heirs = ref false
let intestacy_branch_type = ref Branch.ByRepresentation
let hide_implied_reversions = ref true
let verbosity = ref Verbosity.Verbose

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
  | Verbosity of verbosity [@@deriving sexp, yojson]

let in_force b = " is" ^ (if b then "" else " not") ^ " in force"
let can b = if b then " can" else " cannot"

let to_string p = match p with
  | InitialOwner p
    -> "initial owner is " ^ p
  | AmbiguousInterest i
    -> "abmiguous language creates a " ^ (AmbiguousInterest.to_string i)
  | AmbiguousTenancy t
    -> "ambiguous langauge creates a " ^ (AmbiguousTenancy.to_string t)
  | DefaultDistributionRule b
  -> "default distribution is " ^ Branch.to_string b
  | DestructibilityOfContintentRemainders b
    -> "destructibility of contintent remainders" ^ in_force b
  | DisentailByConveyance b
    -> "a tenant in tail" ^ can b ^ "bar the entail by conveying a fee simple"
  | DoctrineOfWorthierTitle b
    -> "the doctrine of worthier title" ^ in_force b
  | HideImpliedReversions b -> "defeasible non-possessory implied reversions are" ^
       if b then " hidden" else " displayed"
  | ImplyUnspecifiedHeirs b
    -> if b then "generic 'heirs' assumed when no other heir can be found"
       else "property escheats when no heir can be found"
  | IntestacyBranchType b
    -> "property descends in intestacy " ^ Branch.to_string b
  | RuleAgainstPerpetuities b
    -> "the rule against perpetuities" ^ in_force b
  | RuleInShelleysCase b ->
    "the rule in Shelley's case" ^ in_force b
  | Verbosity v
    -> "descriptions of interests are " ^ Verbosity.to_string v

let set (parameter:t): unit =
  match parameter with
  | AmbiguousInterest i -> ambiguous_interest := i
  | AmbiguousTenancy t -> ambiguous_tenancy := t
  | DefaultDistributionRule v -> default_distribution_rule := v
  | DestructibilityOfContintentRemainders v -> destructibility_of_contingent_remainders := v
  | DisentailByConveyance v -> disentail_by_conveyance := v
  | DoctrineOfWorthierTitle v -> doctrine_of_worthier_title := v
  | HideImpliedReversions v -> hide_implied_reversions := v
  | ImplyUnspecifiedHeirs v -> imply_unspecified_heirs := v
  | InitialOwner p -> initial_owner := p
  | IntestacyBranchType v -> intestacy_branch_type := v
  | RuleAgainstPerpetuities v -> rule_against_perpetuities := v
  | RuleInShelleysCase v -> rule_in_shelleys_case := v
  | Verbosity v -> verbosity := v


module Interface = struct

  type options = Boolean | OneOf of (string * string) list | String  [@@deriving yojson]
  type current = Yes | No | Value of string [@@deriving yojson]

  type param = { name: string
               ; desc : string
               ; abbrev : string
               ; options : options
               ; current : current 
               } [@@deriving yojson]

  let to_current b = if b then Yes else No

  let parameters : param list = [
    {
      name = "Default Duration";
      desc =
        "The estate created by language that does not explicitly specify a fee simple or life estate";
      abbrev = "afs";
      options = OneOf [("fs", "Fee Simple"); ("le", "Life Estate")];
      current = Value (AmbiguousInterest.to_abbrev !ambiguous_interest)
    };
    {
      name = "Default Cotenancy";
      desc = "The cotenancy created by language that does not explicitly specify a tenancy in common or joint tenancy";
      abbrev = "atc";
      options = OneOf [("ic", "Common"); ("j", "Joint")];
      current = Value (AmbiguousTenancy.to_abbrev !ambiguous_tenancy)
    };
    {
      name = "Imply Heirs When Needed";
      desc = "Generic 'heirs' will be implied when no other heirs have been identified";
      abbrev = "iuh";
      options = Boolean;
      current = to_current !imply_unspecified_heirs
    };
    {
      name = "Default Distribution Rule";
      desc = "How property is divided among class members when no distribution rule is specified by the grantor";
      abbrev = "ddr";
      options = OneOf [("p", "Primogeniture") ; ("pc", "Per Capita") ;
                       ("ps", "Per Stirpes") ; ("br", "By Representation")];
      current = Value (Common.Branch.to_abbrev !default_distribution_rule)
    };
    {
      name = "Intestacy Distribution Rule";
      desc = "How property is divided among heirs in intestacy";
      abbrev = "ibt";
      options = OneOf [("p", "Primogeniture") ; ("pc", "Per Capita") ;
                       ("ps", "Per Stirpes") ; ("br", "By Representation")];
      current = Value (Common.Branch.to_abbrev !intestacy_branch_type)
    };
    {
      name = "Destructibility of Contingent Remainders" ;
      desc = "The doctrine of the destructibility of contingent remainders is in force";
      abbrev = "dcr";
      options = Boolean;
      current = to_current !destructibility_of_contingent_remainders
    };
    {
      name = "Doctrine of Worthier Title";
      desc = "The doctrine of worthier title is in force";
      abbrev = "dwt";
      options = Boolean;
      current = to_current !doctrine_of_worthier_title
    
    };
    {
      name = "Rule in Shelley's Case";
      desc = "The Rule in Shelleys Case is in force";
      abbrev = "rsc";
      options = Boolean;
      current = to_current !rule_in_shelleys_case
    };
    {
      name = "Rule Against Perpetuities";
      desc = "The rule against perpetuities is in force";
      abbrev = "rap";
      options = Boolean;
      current = to_current !rule_against_perpetuities
    };
    {
      name = "Disentailment by Conveyance" ;
      desc = "A tenant in tail can bar the entail by conveying a fee simple";
      abbrev = "dbc";
      options = Boolean;
      current = to_current !disentail_by_conveyance
    };
    {
      name = "Initial Owner";
      desc = "";
      abbrev = "io";
      options = String;
      current = Value !initial_owner
    };
    {
      name = "Hide Implied Reversions";
      desc = "Hide certain implied reversions unless they become possessory";
      abbrev = "hir";
      options = Boolean;
      current = to_current !hide_implied_reversions
    };
    {
      name = "Verbosity";
      desc = "Use complete, simple, or abbreviated descriptions of interests";
      abbrev = "verbosity";
      options = OneOf [("v", "Verbose" ); ("b", "Brief"); ("sb", "Superbrief")];
      current = Value (Verbosity.to_abbrev !verbosity)
    }
  ]

  let of_abbrev abbrev value =
    match abbrev with
    | "afs" -> AmbiguousInterest (AmbiguousInterest.of_abbrev value)
    | "atc" -> AmbiguousTenancy (AmbiguousTenancy.of_abbrev value)
    | "dcr" -> DestructibilityOfContintentRemainders (Bool.of_string value)
    | "ddr"  -> DefaultDistributionRule (Branch.of_abbrev value)
    | "dbc" -> DisentailByConveyance (Bool.of_string value)
    | "dwt" -> DoctrineOfWorthierTitle (Bool.of_string value)
    | "hir" -> HideImpliedReversions (Bool.of_string value)
    | "io" -> InitialOwner value
    | "ibt" -> IntestacyBranchType (Branch.of_abbrev value)
    | "rap" -> RuleAgainstPerpetuities (Bool.of_string value)
    | "rsc" -> RuleInShelleysCase (Bool.of_string value)
    | "iuh" -> ImplyUnspecifiedHeirs (Bool.of_string value)
    | "verbosity" -> Verbosity (Verbosity.of_abbrev value)
    | _ -> raise (InvalidParameter ("Unknown parameter: " ^ abbrev))


  let get abbrev =
    match abbrev with
    | "afs" -> AmbiguousInterest !ambiguous_interest
    | "atc" -> AmbiguousTenancy !ambiguous_tenancy
    | "dcr" -> DestructibilityOfContintentRemainders !destructibility_of_contingent_remainders
    | "dbc" -> DisentailByConveyance !disentail_by_conveyance
    | "ddr" -> DefaultDistributionRule !default_distribution_rule
    | "dwt" -> DoctrineOfWorthierTitle !doctrine_of_worthier_title
    | "hir" -> HideImpliedReversions !hide_implied_reversions
    | "io" -> InitialOwner !initial_owner
    | "ibt" -> IntestacyBranchType !intestacy_branch_type
    | "rap" -> RuleAgainstPerpetuities !rule_against_perpetuities
    | "rsc" -> RuleInShelleysCase !rule_in_shelleys_case
    | "iuh" -> ImplyUnspecifiedHeirs !imply_unspecified_heirs
    | "verbosity" -> Verbosity !verbosity
    | _ -> raise (InvalidParameter ("Unknown parameter: " ^ abbrev))

end
