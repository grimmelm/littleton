(** Monadic Parsers for the command line *)
open Core_kernel
open MParser
open Common
open Event
open Predicate

module H = Holder
module D = Description
module P = PredicateType

module Tokens = MParser_RE.Tokens

exception ParseError of string

type symbol = (string, bytes list) MParser.t
type parser_duration =
  | Absolute
  | ForLife
  | ForLifePerson of person
  | InTail
  | ForYears of int
  | UnspecifiedDuration [@@deriving sexp]

type parser_cotenancy =
  | InCommon
  | Jointly
  | ByEntireties
  | UnspecifiedCotenancy [@@deriving sexp]

type grant = description * parser_cotenancy * parser_duration [@@deriving sexp] 

type combination =
  | Nothing
  | Single  of grant
  | CondPred       of  combination *  combination * P.t
  | CondPredWait   of  combination *  combination * P.t
  | After      of combination  * P.t
  | While   of combination *  P.t
  | Then    of combination * combination
  | But     of combination *  P.t  * combination
  | Reentry of combination *  P.t  [@@deriving sexp]

type 'a stmt =
  | Owns of person
  | Conveys of person * 'a
  | Wills of person * 'a
  | Occurs of event
  | Set of Parameters.t [@@deriving sexp]

type t = combination stmt list [@@deriving sexp]

(* Shortcut functions *)
let symbol = Tokens.symbol
let dot = Tokens.dot
let semi = Tokens.semi
let comma = Tokens.comma
let integer = Tokens.integer

(* Common symbols *)
let to' : symbol = (attempt (symbol "to") <|> symbol "To")
let from' : symbol = symbol "from"
let if' : symbol = symbol "if"
let and' : symbol = symbol "and"
let but' : symbol = symbol "but"
let then' : symbol = symbol "then"
let else' : symbol = symbol "else"
let during' : symbol = symbol "during"
let before' : symbol = symbol "before"
let after' : symbol = symbol "after"
let however' : symbol = symbol "however"
let otherwise' : symbol = symbol "otherwise"
let the' :  symbol = symbol "the"
let thecap' :  symbol = symbol "The"
let a' :  symbol = symbol "a"
let for' : symbol = symbol "for"
let years' : symbol = symbol "years"
let pass' : symbol = symbol "pass"
let life' : symbol = symbol "life"
let of' : symbol = symbol "of"
let owns' : symbol = symbol "owns"
let who' : symbol = symbol "who"
let when' : symbol = symbol "when"
let until' : symbol = symbol "until"
(* let unless' : symbol = symbol "unless" *)
let while' : symbol = symbol "while"
let his' : symbol = symbol "his"
let her' : symbol = symbol "her"
let their' : symbol = symbol "their"
let heirs' : symbol = symbol "heirs"
let spouse' : symbol = symbol "spouse"
let wife' : symbol = symbol "wife"
let husband' : symbol = symbol "husband"
let widow' : symbol = symbol "widow"
let widower' : symbol = symbol "widower"
let surviving' : symbol = symbol "surviving"
let forever' : symbol = symbol "forever"
let body' : symbol = symbol "body"
let grantor' : symbol = symbol "grantor"
let may' : symbol = symbol "may"
let reenter' : symbol = symbol "reenter"
let conveys' : symbol = symbol "conveys"
let property' : symbol = symbol "property"
let land' : symbol = symbol "land"
let makes' : symbol = symbol "makes"
let will' : symbol = symbol "will"
let leaving' : symbol = symbol "leaving"
let dies' : symbol = symbol "dies"
let died' : symbol = symbol "died"
let dead' : symbol = symbol "dead"
let survives' : symbol = symbol "survives"
let survived' : symbol = symbol "survived"
let outlives' : symbol = symbol "outlives"
let outlived' : symbol = symbol "outlived"
let predeceases' : symbol = symbol "predeceases"
let predeceased' : symbol = symbol "predeceased"
let alive' : symbol = symbol "alive"
let get' :  symbol = symbol "get"
let gets' :  symbol = symbol "gets"
(* let gotten' :  symbol = symbol "gotten" *)
let does' :  symbol = symbol "does"
let do' :  symbol = symbol "do"
let marry' : symbol = symbol "marry"
let marries' : symbol = symbol "marries"
let married' : symbol = symbol "married"
let unmarried' : symbol = symbol "unmarried"
let divorce' : symbol = symbol "divorce"
let divorces' : symbol = symbol "divorces"
let divorced' : symbol = symbol "divorced"
let has' : symbol = symbol "has"
let have' : symbol = symbol "have"
(* let occurs' : symbol = symbol "occurs" *)
let reenters' : symbol = symbol "reenters"
let retakes' : symbol = symbol "retakes"
let is' : symbol = symbol "is"
let are' : symbol = symbol "are"
let been' : symbol = symbol "been"
let not' : symbol = symbol "not"
let ever' : symbol = symbol "ever"
let never' : symbol = symbol "never"
let no' : symbol = symbol "no"
let living' : symbol = symbol "living"
let child' : symbol = symbol "child"
let children' : symbol = symbol "children"
let grandchildren' : symbol = symbol "grandchildren"
let descendants' : symbol = symbol "descendants"
let issue' : symbol = symbol "issue"
let used'  : symbol = symbol "used"
let as'    : symbol = symbol "as"
let commercial' : symbol = symbol "commercial"
let purposes' : symbol = symbol "purposes"
let become'    : symbol = symbol "become"
let becomes'    : symbol = symbol "becomes"
let graduate'    : symbol = symbol "graduate"
let graduates'    : symbol = symbol "graduates"
let graduated'    : symbol = symbol "graduated"
let law' : symbol = symbol "law"
let school' : symbol = symbol "school"
let state' : symbol = symbol "state"
let consume'    : symbol = symbol "consume"
let consumes'    : symbol = symbol "consumes"
let consumed'    : symbol = symbol "consumed"
let eat'    : symbol = symbol "eat"
let eats'    : symbol = symbol "eats"
let eaten'    : symbol = symbol "eaten"
let drink'    : symbol = symbol "drink"
let drinks'    : symbol = symbol "drinks"
let drunk'    : symbol = symbol "drunk"
let lparen' : symbol = symbol "("
let rparen' : symbol = symbol ")"
let possessive' : symbol = symbol "poss"
let in' : symbol = symbol "in"
let common' : symbol = symbol "common"
let joint' : symbol = symbol "joint"
let jointly' : symbol = symbol "jointly"
let tenants' : symbol = symbol "tenants"
let by' : symbol = symbol "by"
let entireties' : symbol = symbol "entireties"
let per' : symbol = symbol "per"
let stirpes' : symbol = symbol "stirpes"
let capita' : symbol = symbol "capita"
let primogeniture' : symbol = symbol "primogeniture"
let representation' : symbol = symbol "representation"
let set' : symbol = symbol "set"
let afs' : symbol = symbol "afs"
let atc' : symbol = symbol "atc"
let dcr' : symbol = symbol "dcr"
let dwt' : symbol = symbol "dwt"
let rsc' : symbol = symbol "rsc"
let rap' : symbol = symbol "rap"
let dbc' : symbol = symbol "dbc"
let iuh' : symbol = symbol "iuh"
(* let ips' : symbol = symbol "ips" *)
let hir' : symbol = symbol "hir"
let verbosity' : symbol = symbol "verbosity"
let intestacy' : symbol = symbol "intestacy"
let true' : symbol = symbol "true"
let false' : symbol = symbol "false"
let verbose' : symbol = symbol "verbose"
let brief' : symbol = symbol "brief"
let superbrief' : symbol = symbol "superbrief"
let fee' : symbol = symbol "fee"
let simple' = symbol "simple"

let but'' = (but'<|>however') >> optional comma

let provided' = symbol "provided" >>=
  fun p -> optional (symbol "that") >> return p

let oncondition' = symbol "on" >> symbol "condition" >> symbol "that"

let solongas' = symbol "so" >> spaces >> symbol "long" >> spaces >> symbol "as"

let opt_comma : (unit , bytes list) MParser.t =
  (spaces >> (comma<|>semi) >> spaces) <|> spaces

let phrase : (string, bytes list) MParser.t =
  many_chars ( alphanum <|> blank <|> (any_of "'"))

let andthen : (unit, bytes list) MParser.t =
  opt_comma >> optional (then' <|>  (and' >> then'))
  >> optional (symbol "back") >> return ()

let orelse' = opt_comma >> (else' <|> otherwise')

let name : (string, bytes list) MParser.t =
  uppercase >>= fun head -> many_chars lowercase >>= fun rest ->
  spaces >> return (String.concat ~sep:"" [String.of_char head; rest])

(* JG: the apostrophe variant doesn't work *)
let name_possessive : (string, bytes list) MParser.t =
  uppercase >>= fun head -> many_chars lowercase >>= fun rest -> spaces >> 
    possessive' >> return (String.concat ~sep:"" [String.of_char head; rest])

let person : (person, bytes list) MParser.t =
  many1 name >>= fun l -> return (Person.of_string (String.concat ~sep:" " l))

(* JG: the apostrophe variant doesn't work *)
let person_possessive : (person, bytes list) MParser.t =
  many name >>= fun l -> name_possessive >>= fun rest ->
  let firstn = (String.concat ~sep:" " l) in 
  return (Person.of_string (String.concat ~sep:" " [firstn; rest]))

(* JG: This is hacky and fragile *)
let rec person_tail _ : (person list, bytes list) MParser.t = 
  attempt (and' >> person >>= fun p -> person_tail () >>= fun ps -> return (p::ps)) <|>
  return []

let person_set : (person list, bytes list) MParser.t =
  person >>= fun p1 -> person_tail () >>= fun ps -> return (p1::ps)

let oneword : (string, bytes list) MParser.t = 
  spaces >> many alphanum >>= fun p -> spaces >>
  return (String.of_char_list p)

let property : (unit, bytes list) MParser.t =
  attempt ((the' <|> thecap') >> (property' <|> land') >> return () ) <|>
  (oneword >>= fun _ -> return () )

let use : (string, bytes list) MParser.t =
  attempt (commercial' >> purposes' >>  return "commercial purposes") <|>
  attempt (a' >> oneword >>= fun u  -> return ("a " ^ u) ) <|>
  oneword

let institution : (string, bytes list) MParser.t =
    attempt (law' >> school' >>  return "law school") <|>
    oneword

 let rec attemptall phrases : (string, bytes list) MParser.t = match phrases with
    | [] -> zero
    | hd::tl -> attempt hd <|> attemptall tl

let pronoun : (string, bytes list) MParser.t
 =  attemptall [their' ; his' ; her' ; oneword]

let event_death : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p -> dies' >>
  return (E.Dies p)

let event_marriage : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p1 ->
  (attempt (( marries' <|> (gets' >> married' >> to'))  >>
  person >>= fun p2 ->
  return (E.Marry (p1,Some p2)) ) )
  <|>
  attempt ((and' >> person >>= fun p2 ->
  (marry' <|> (get' >> married') ) >>
  return (E.Marry (p1,Some p2)) ) )
  <|>
  ( (marries' <|>  (gets' >> married' )) >>
  return (E.Marry (p1,None)) )

let event_divorce : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p1 ->
  attempt (attemptall [ divorces' ;  gets' >> divorced' >> from' ] >>
  person >>= fun p2 ->
  return (E.Divorce (p1,Some p2)) )
  <|>
  attempt (and' >> person >>= fun p2 ->
  attemptall [divorce' ; get' >> divorced']  >>
  return (E.Divorce (p1,Some p2)) )
  <|>
  (attemptall [divorces' ; gets' >> divorced' ] >>
  return (E.Divorce (p1,None)) )

let event_child : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p1 ->
    attempt (  has' >> child' >> 
    person >>= fun p2 ->
    return (E.Child (p1, p2)) )

let event_reentry : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p ->
  attemptall [reenters' ; retakes' ; reenters' >> and' >> retakes'] >> optional property >>
  return (E.Reenters p)

let event_time : (event, bytes list) MParser.t =
  let module E = Event in
  Tokens.integer >>= fun n -> years' >> pass' >>
  return (E.YearsPass n)

let event_usedas:  (event, bytes list) MParser.t =
  let module E = Event in
  property >> is' >> used' >> attemptall [for' ; as'] >>
  use >>= fun u -> return (E.IsUsedAs u)

let event_graduates:  (event, bytes list) MParser.t =
  let module E = Event in
  attempt (person >>= fun p -> graduates' >> from' >> institution >>= fun i -> return (E.Graduates (p,i)))
  <|>
  attempt (person >>= fun p -> graduates' >> institution >>= fun i -> return (E.Graduates (p,i)))

let event_consumes:  (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p ->
  (consumes' <|> eats' <|> drinks') >>
  oneword >>= fun s ->
  return (E.Consumes (p,s))

let event_state: (event, bytes list) MParser.t  = 
  let module E = Event in
  oneword >>= fun s ->
  becomes' >> a' >> state' >>
  return (E.BecomesState (s))

let event_failureissue : (event, bytes list) MParser.t =
  let module E = Event in
  person >>= fun p ->
  attemptall [
    has' >> no' >> living' >> descendants';
    has' >> no' >> living' >> issue'] >>
  return (E.FailureOfIssue p)

let event_generic : (event, bytes list) MParser.t =
  let module E = Event in
  spaces >> Tokens.parens phrase >>= fun string ->
  return (E.Generic string)

let event : (event, bytes list) MParser.t =
  attempt event_death <|>
  attempt event_marriage <|>
  attempt event_divorce <|>
  attempt event_child <|>
  attempt event_reentry <|>
  attempt event_time <|>
  attempt event_usedas <|>
  attempt event_graduates <|>
  attempt event_consumes <|>
  attempt event_state <|>
  attempt event_failureissue <|>
  event_generic

let isdead_person : (person, bytes list) MParser.t =
  person >>= fun p ->
  attemptall [dies' ;  is' >> dead' ; has' >> died' ; is' >> not' >> alive'] >>
  return p

let isalive_person : (person, bytes list) MParser.t =
  person >>= fun p ->
  attemptall [is' >> alive' ; has' >> not' >> died' ; is' >> not' >> dead'] >>
  return p

(* let pospres pred = pred
 * let negpres pred = P.Not (pred)
 * let pospast pred = P.Past (pred)
 * let negpast pred = P.Not (P.Past (pred)) *)

(* Parses both singular and plural indiscriminately *)
let tenses (pluralpres, singpres, past) =
  attempt (singpres >> return (fun pred -> pred)) <|>
  attempt (does' >> not' >> pluralpres >> return (fun pred -> P.Not (pred))) <|>
  attempt (has' >> (optional ever') >> past >> return (fun pred -> P.Past (pred))) <|>
  attempt (has' >> (not' <|> never') >> past >> return (fun pred -> P.Not (P.Past (pred)))) <|>
  attempt (pluralpres >> return (fun pred -> pred)) <|>
  attempt (do' >> not' >> pluralpres >> return( fun pred -> P.Not (pred))) <|>
  attempt (have' >> (optional ever') >> past >> return (fun pred -> P.Past (pred))) <|>
  attempt (have' >> (not' <|> never') >> past >> return (fun pred -> P.Not (P.Past (pred))))

let tenses_passive verb = 
  attempt (is' >> verb >> return (fun pred -> pred)) <|>
  attempt (is' >> not' >> verb >> return (fun pred -> P.Not (pred))) <|>
  attempt (has' >> been' >> verb >> return (fun pred -> P.Past (pred))) <|>
  attempt (has' >> not' >> been' >> return (fun pred -> P.Not (P.Past (pred))))

 let pred_isdead : (P.t, bytes list) MParser.t =
  attempt (isdead_person >>= fun p -> return  (P.Not (P.Atomic (P.A.IsAlive p))) ) <|>
  (isalive_person >>= fun p -> return (P.Atomic (P.A.IsAlive p)) )

let pred_survives : (P.t, bytes list) MParser.t =
  person >>= fun p1 ->
    ( attemptall [survives' ; (optional has') >> survived' ;
    outlives' ; (optional has') >> outlived' ;
    dies' >> after' ; (optional has') >> died' >> after' ] >>
    person >>= fun p2 -> return (P.Atomic (P.A.Survives (p1,p2))) )
    <|>
   ( attemptall [predeceases' ; (optional has') >> predeceased' ;
    dies' >> before' ; (optional has') >> died' >> before' ] >>
    person >>= fun p2 -> return (P.Atomic (P.A.Survives (p2,p1))) )

let pred_marries : (P.t, bytes list) MParser.t =
  let module E = Event in
  attempt (person >>= fun p1 -> and' >> person >>= fun p2 ->
    attempt ( marry'                      >> return (P.Atomic (P.A.Marry         (p1,Some p2))) ) <|>
    attempt ( have' >> (optional ever') >> married'  >> return (P.Past (P.Atomic (P.A.Marry (p1,Some p2)))) ) <|>
    attempt ( are' >> married'            >> return (P.Atomic (P.A.IsMarried     (p1,Some p2))) ) <|>
    attempt ( do' >> not' >> marry'       >> return (P.Not (P.Atomic (P.A.Marry         (p1,Some p2)))) ) <|>
    attempt ( have' >> (not' <|> never')  >> married'   >> return (P.Not (P.Past (P.Atomic (P.A.Marry (p1,Some p2))))) ) <|>
    attempt ( are' >> not' >> married'    >> return (P.Not (P.Atomic (P.A.IsMarried     (p1,Some p2)))) ) )
  <|>
  attempt (person >>= fun p1 ->
    attempt ( marries'                    >> return (P.Atomic (P.A.Marry         (p1,None))) ) <|>
    attempt ( has' >> (optional ever') >> married'  >> return (P.Past (P.Atomic (P.A.Marry (p1,None)))) ) <|>
    attempt ( is' >> married'             >> return (P.Atomic (P.A.IsMarried     (p1,None))) ) <|>
    attempt ( does' >> not' >> marry'     >> return (P.Not (P.Atomic (P.A.Marry         (p1,None)))) ) <|>
    attempt ( has' >> (not' <|> never')  >> married'    >> return (P.Not (P.Past (P.Atomic (P.A.Marry (p1,None))))) ) <|>
    attempt ( attemptall [is' >> not' >> married' ; is' >> unmarried']  >> return (P.Not (P.Atomic (P.A.IsMarried     (p1,None)))) ) )

let pred_divorces : (P.t, bytes list) MParser.t =
  attempt (person >>= fun p1 -> and' >> person >>= fun p2 ->
     tenses (divorce', divorces', divorced') >>= fun t -> return (t (P.Atomic (P.A.Divorce (p1,Some p2)))))
     <|> 
    (person >>= fun p1 ->
      tenses (divorce', divorces', divorced') >>= fun t -> return (t (P.Atomic (P.A.Divorce (p1,None)))))

     (* attempt ( divorce'                                >> return (pospres pred)) <|>
      attempt ( have' >> (optional ever') >> divorced'  >> return (pospast pred)) <|>
      attempt ( do' >> not' >> divorce'                 >> return (negpres pred)) <|>
      attempt ( have' >> (not' <|> never') >> divorced' >> return (negpast pred)) ) *)

  (* (person >>= fun p1 ->
  let pred = P.Atomic (P.A.Divorce (p1,None)) in
      attempt ( divorces'                 >> return (pospres pred )) <|>
      attempt ( has' >> divorced'         >> return (pospast pred )) <|>
      attempt ( does' >> not' >> divorce' >> return (negpres pred )) <|> 
      attempt ( has' >> (not' <|> never') >> return (negpast pred )) ) *)

let pred_time : (P.t, bytes list) MParser.t =
  Tokens.integer >>= fun n -> years' >> pass' >>
  return (P.Atomic(P.A.Years n))

let pred_usedas :  (P.t, bytes list) MParser.t =
  let module E = Event in
  property >> tenses_passive used' >>= fun t -> 
  attemptall [for' ; as'] >> 
  use >>= fun u -> 
  return (t (P.Atomic (P.A.UsedAs u)))

let pred_graduates :  (P.t, bytes list) MParser.t =
  person >>= fun p -> 
  tenses (graduate', graduates', graduated') >>= fun t ->
  (attempt (from' >> institution >>= fun i -> return (t (P.Atomic (P.A.Graduate (p,i))))) <|>
  (institution >>= fun i -> return (t (P.Atomic (P.A.Graduate (p,i))))))
 
let pred_consumes :  (P.t, bytes list) MParser.t =
  person >>= fun p ->
  tenses (consume' <|> drink' <|> eat', consumes' <|> drinks' <|> eats', consumed' <|> drunk' <|> eaten') >>= fun t ->
  oneword >>= fun s ->
  return (t (P.Atomic (P.A.Consume (p,s))))

 let pred_statehood : (P.t, bytes list) MParser.t =
  oneword >>= fun s -> 
  tenses (become', becomes', become') >>= fun t ->
  a' >> state' >>
  return (t (P.Atomic (P.A.State (s))))

let pred_generic : (P.t, bytes list) MParser.t =
  let module E = Event in
  spaces >> Tokens.parens phrase >>= fun string ->
  return (P.Occurs (E.Generic string))

let pred : ( P.t , bytes list) MParser.t =
  attempt pred_isdead <|>
  attempt pred_survives <|>
  attempt pred_marries <|>
  attempt pred_divorces <|>
  attempt pred_time <|>
  attempt pred_usedas <|>
  attempt pred_graduates <|>
  attempt pred_consumes <|>
  attempt pred_statehood <|>
  pred_generic
let predicate = pred <?> "a supported condition or (a parenthesized condition)."

let spouse_term  =  spouse'<|> wife' <|> husband'

let parse_sharing : (parser_cotenancy, bytes list)  MParser.t =
  (attemptall [in' >> common' ; as' >> tenants' >> in' >> common'] >> return InCommon) <|>
  (attemptall [jointly' ; as' >> joint' >> tenants'] >> return Jointly) <|>
  (attemptall [as' >> spouse_term >> and' >> spouse_term ; as' >> tenants' >> by' >> the' >> entireties'] >> return ByEntireties) <|>
  return UnspecifiedCotenancy

let parse_duration : (parser_duration, bytes list) MParser.t =
  (* fee tail *) (* JG: the backtracking here isn't working correctly  *)
  attempt (and' >> the' >> heirs' >> of' >> pronoun >> body' >> return InTail) <|>
  (* fee simple *)
  attempt (forever' >> return Absolute) <|>
  attempt (and' >> pronoun >> heirs' >> return Absolute) <|>
  (* life estate *)
  attempt (for' >> life' >> return ForLife) <|>
  attempt (for' >> the' >> life' >> of' >> person >>= fun p -> return (ForLifePerson p)) <|>
  attempt (while' >> isalive_person >>= fun p -> return (ForLifePerson p)) <|>
  attempt (until' >> isdead_person >>= fun p -> return (ForLifePerson p)) <|>
  (* term of years *)
  attempt (for' >> integer >>= (fun n -> years' >> return (ForYears n))) <|>
  (* default *)
  return UnspecifiedDuration


let individuals : (description, bytes list) MParser.t =
  person_set >>= fun ps -> return (D.Individuals ps)

let spouse : (description, bytes list) MParser.t =
  attempt (the' >> spouse_term >> of' >> person >>= fun p -> return (D.Spouse p)) <|>
  attempt (person_possessive >>= fun p -> spouse_term >> return (D.Spouse p))

(* Using "widow" as shorthand for widow/widower/surviving spouse *)
let widow : (description, bytes list) MParser.t =
    attempt (the' >> attemptall [widow' ; widower' ; surviving' >> spouse_term] >> of' >> person >>= fun p -> 
      return (D.Widow p)) <|>
    attempt (person_possessive >>= fun p -> attemptall [widow' ; widower' ; surviving' >> spouse_term] >> return (D.Widow p))

let verbosity : (Parameters.verbosity, bytes list) MParser.t =
  (attempt verbose' >> return Parameters.Verbosity.Verbose) <|>
  (attempt brief' >> return Parameters.Verbosity.Brief) <|>
  (attempt superbrief' >> return Parameters.Verbosity.Superbrief) <|>
  return !Parameters.verbosity

let branch : (branch, bytes list) MParser.t =
  (attempt (per' >> stirpes') >> return Branch.PerStirpes) <|>
  (attempt (by' >> representation') >> return Branch.ByRepresentation) <|>
  (attempt (per' >> capita') >> return Branch.PerCapita) <|>
  (attempt ((optional by') >> primogeniture') >> return Branch.Primogeniture) <|>
  (* If none is specified, return the default *)
  return !Parameters.default_distribution_rule

let amb_interest : (Parameters.AmbiguousInterest.t, bytes list) MParser.t =
  (attempt (fee' >> simple') >> return Parameters.AmbiguousInterest.FeeSimple) <|>
  (attempt (for' >> life') >> return Parameters.AmbiguousInterest.LifeEstate) <|>
  (return !Parameters.ambiguous_interest)

let amb_tenancy : (Parameters.AmbiguousTenancy.t, bytes list) MParser.t =
  (attempt (in' >> common') >> return Parameters.AmbiguousTenancy.InCommon) <|>
  (attempt (joint') >> return Parameters.AmbiguousTenancy.InCommon) <|>
  return !Parameters.ambiguous_tenancy

let descendants : (description, bytes list) MParser.t =
  the' >> descendants' >> of' >> person >>= fun p ->
  branch >>= fun b -> return (D.Descendants(p,b))

let heirs : (description, bytes list) MParser.t =
  attempt (the' >> heirs' >> of' >> person >>= fun p -> return (D.Heirs p)) <|>
  attempt (person_possessive >>= fun p -> heirs' >> return (D.Heirs p))

let children : (description, bytes list) MParser.t =
    attempt (the' >> children' >> of' >> person >>= fun p -> return (D.Children p)) <|>
    attempt (person_possessive >>= fun p -> children' >> return (D.Children p))

let grandchildren : (description, bytes list) MParser.t =
  attempt (the' >> grandchildren' >> of' >> person >>= fun p -> return (D.Grandchildren p)) <|>
  attempt (person_possessive >>= fun p -> grandchildren' >> return (D.Grandchildren p))

let parse_group : (description, bytes list) MParser.t =
    (attempt individuals) <|>
    (attempt spouse) <|>
    (attempt widow) <|>
    (attempt descendants) <|>
    (attempt heirs) <|> 
    (attempt children) <|>
    (attempt grandchildren)

let restriction_graduates : (D.restriction, bytes list) MParser.t =
  who' >> graduate' >> (optional from') >> institution >>= fun i -> return (D.R.Graduate i)

let restriction_unrestricted : (D.restriction, bytes list) MParser.t =
  return D.R.Unrestricted

let parse_restriction : (D.restriction, bytes list) MParser.t =
  (attempt restriction_graduates) <|>
  (attempt restriction_unrestricted)
  
let parse_description : (description, bytes list) MParser.t =
   parse_group >>= fun g -> 
   parse_restriction >>= fun r -> 
  if Poly.(r = D.R.Unrestricted) then return g else return (D.Restriction(r,g))

let parse_class : (grant,  bytes list ) MParser.t =
  to' >>   
  parse_description >>= fun d -> 
  parse_sharing >>= fun s -> 
  parse_duration >>= fun dur -> return (d,s,dur)

let grant : (combination, bytes list) MParser.t =
  (attempt parse_class) >>= fun g -> return (Single g)
  
let limitation : (predicate , bytes list) MParser.t =
  attempt ((optional (symbol "or")) >> until' >> predicate >>= fun pr ->
           return (P.Not pr)) <|>
  attempt ((optional and') >>  
           (while' <|> provided' <|> solongas' <|> oncondition' <|> during') >>
           predicate >>= fun pr -> return pr)

let oncondition : ( P.t , bytes list) MParser.t =
  (if' <|> oncondition') >> predicate >>= fun pr ->
  return pr

let oncondition_wait : ( P.t , bytes list) MParser.t =
  if' >> and' >> when' >> predicate >>= fun pr ->
  return pr
  
let mayreenter : ( P.t , bytes list) MParser.t =
  opt_comma >> but'' >> if' >> predicate  >>= fun pr -> opt_comma >>
  the' >> grantor' >> may' >> reenter' >> return pr

let butif : ( P.t , bytes list) MParser.t =
  opt_comma >> but'' >> if' >>
  predicate >>= fun pr ->
  optional andthen >> return pr

let rec rest prev: (combination, bytes list) MParser.t =
  attempt (limitation  >>= fun pr -> rest (While(prev,pr))) <|>
  attempt (andthen >> clauses 0 >>= fun cl -> return (Then(prev,cl)) ) <|>
  attempt (butif >>= fun pr -> clauses 0 >>= fun cl -> return (But(prev,pr,cl)) ) <|>
  attempt (mayreenter >>= fun pr -> return (Reentry(prev,pr)) ) <|>
  return prev

and parens depth : (combination, bytes list) MParser.t =
  attempt (lparen' >> clauses depth >>= fun cl -> rparen' >> return cl) <|>
  grant

and clauses depth : (combination, bytes list) MParser.t =
  if depth > 5 then zero
  else
    let depth' = depth + 1 in
    attempt (after' >> pred >>= fun cp -> parens depth' >>= fun cl1 -> rest (After(cl1,cp))) <|>
    attempt (oncondition >>= fun cp -> parens depth' >>= fun cl1 ->
      orelse' >> parens depth' >>= fun cl2 -> rest (CondPred(cl1,cl2,cp))) <|>
    attempt (oncondition >>= fun cp -> parens depth' >>= fun cl -> rest (CondPred(cl,Nothing,cp))) <|> 
    attempt (oncondition_wait >>= fun cp -> parens depth' >>= fun cl1 ->
      orelse' >> parens depth' >>= fun cl2 -> rest (CondPredWait(cl1,cl2,cp))) <|>
    attempt (oncondition_wait >>= fun cp -> parens depth' >>= fun cl -> rest (CondPredWait(cl,Nothing,cp))) <|>
    attempt (parens depth' >>= fun cl -> rest cl)


(* Conveyances -- A conveys [Property] [grants]. *)
let conveyance : (combination stmt, bytes list) MParser.t =
  attempt (person >>= fun grantor ->
  (optional conveys') >>
  (optional property) >>= fun _ -> spaces >>
  clauses 0 >>= fun cs ->
  return ( Conveys (grantor, cs))) <|>
  attempt (person >>= fun grantor ->
  (optional conveys') >>= fun _ -> spaces >>
  clauses 0 >>= fun cs ->
  return ( Conveys (grantor, cs))) <|>
  attempt (
  (optional conveys') >>= fun _ -> spaces >>
  clauses 0 >>= fun cs ->
  return ( Conveys (Person.of_string "O", cs)))

(* Wills -- A makes a will [leaving <property>] [grants]. *)
let will : (combination stmt, bytes list) MParser.t =
  attempt (person >>= fun testator ->
  makes' >> a' >> will' >>
  clauses 0 >>= fun cs ->
  return ( Wills (testator, cs))) <|>
  attempt (person >>= fun testator ->
    makes' >> a' >> will' >> leaving' >>
    oneword >>= fun _ ->
    clauses 0 >>= fun cs ->
    return ( Wills (testator, cs)))

let occurs : (combination stmt, bytes list) MParser.t =
  event >>= fun event ->
  return (Occurs event)

let owns : (combination stmt, bytes list) MParser.t =
  person >>= fun person ->
  owns' >>
  (optional property) >>= fun _ ->
  return (Owns person)

let boolean : (bool, bytes list) MParser.t =
  attempt (true' >> return true) <|>
  attempt (false' >> return false)

let parameter : (Parameters.t, bytes list) MParser.t =
  let module P = Parameters in
  attempt (afs' >> to' >> amb_interest >>= fun b -> return (P.AmbiguousInterest b)) <|>
  attempt (atc' >> to' >> amb_tenancy >>= fun b -> return (P.AmbiguousTenancy b)) <|>
  attempt (dcr' >> to' >> boolean >>= fun b -> return (P.DestructibilityOfContintentRemainders b)) <|>
  attempt (dwt' >> to' >> boolean >>= fun b -> return (P.DoctrineOfWorthierTitle b)) <|>
  attempt (rsc' >> to' >> boolean >>= fun b -> return (P.RuleInShelleysCase b)) <|>
  attempt (rap' >> to' >> boolean >>= fun b -> return (P.RuleAgainstPerpetuities b)) <|>
  attempt (dbc' >> to' >> boolean >>= fun b -> return (P.DisentailByConveyance b)) <|>
  attempt (iuh' >> to' >> boolean >>= fun b -> return (P.ImplyUnspecifiedHeirs b)) <|>
  attempt (hir' >> to' >> boolean >>= fun b -> return (P.HideImpliedReversions b)) <|>
  attempt (verbosity' >> to' >> verbosity >>= fun v -> return (P.Verbosity v)) <|>
  attempt (intestacy' >> to' >> branch >>= fun b -> return (P.IntestacyBranchType b ))
  
let set_parameter : (combination stmt, bytes list) MParser.t =
  set' >> parameter >>= fun p -> return (Set p)

(* Take a parser p and allows for spaces before and a period and spaces after *)
let pad p =
  spaces >> p >>= fun s ->
  spaces >> dot >> spaces >>
  return s

(* Statements *)
let statements : (t, bytes list) MParser.t =
  many_until ( attempt (pad owns) <|>
               attempt (pad occurs) <|>
               attempt (pad conveyance) <|>
               attempt (pad will) <|>
               pad set_parameter) eof

(* Empty program *)
let blank : (t, bytes list) MParser.t =
  eof >> return []

let program : (t, bytes list) MParser.t =
  statements <|>
  blank

let parse string  =
  match MParser.parse_string program string [] with
  | MParser.Success p -> Ok p
  | MParser.Failed(msg,_) -> Error msg

let parse' string = MParser.parse_string program string []
