open Core_kernel
open Common
open Event
open Family
open Classgift

module AtomicPredicate = struct

  module CG = ClassGifts
  module D = Description
  module ESet = EventSet
  module F = Family
  module P = Person
  module E = Event

  type t =
    | IsAlive of person
    | HasIssue of person
    | Survives of person * person
    | Marry of person * person option
    | IsMarried of person * person option
    | Divorce of person * person option
    | Consume of person * string
    | Graduate of person * string
    | UsedAs of string
    | State of string
    | Reentry of person
    | Years of int 
    | Known of description [@@deriving sexp]
  
   let to_string ?negated:(negated=false) ?past:(past=false) ap =
     let tenses neg past (pospres, negpres, pospast, negpast) =
      if neg && past then negpast else
      if not neg && past then pospast else
      if neg && not past then negpres else
      (* not pos && not pres *) pospres
  in match ap with
    | IsAlive p -> (P.to_string p) ^ " " ^ if negated then "is dead" else "is alive"
    | HasIssue p ->  (P.to_string p) ^ " " ^ if negated then "has no descendants" else "has descendants"
    | Survives (p1,p2) ->  (P.to_string p1) ^ " " ^ (if negated then "does not survive"  else "survives") ^ " " ^  (P.to_string p2)
    | Marry(p1,None) ->  (P.to_string p1) ^ " " ^ tenses negated past ("marries", "does not marry", "has married", "has not married")
    | Marry(p1,Some p2) ->  (P.to_string p1) ^ " and " ^  (P.to_string p2) ^ " " ^ tenses negated past ("marry", "do not marry", "have married", "have not married")
    | IsMarried(p1,None) ->  (P.to_string p1) ^ " " ^ if negated then "is not married" else "is married"
    | IsMarried(p1,Some p2) ->  (P.to_string p1) ^ " and " ^  (P.to_string p2) ^ " " ^ if negated then "are not married" else "are married"
    | Divorce(p1,None) ->  (P.to_string p1) ^ " " ^ tenses negated past ("divorces", "does not divorce", "has divorced", "has not divorced")
    | Divorce(p1,Some p2) ->  (P.to_string p1) ^ " and " ^  (P.to_string p2) ^ " " ^tenses negated past ("divorce", "do not divorce", "have divorced", "have not divorced")
    | Consume(p,s) ->  (P.to_string p) ^ " " ^ tenses negated past ("consumes", "does not consume", "has consumed", "has not consumed") ^ " " ^ s
    | Graduate(p,i) ->  (P.to_string p) ^ " " ^ tenses negated past ("graduates", "does not graduate", "has graduated", "has not graduated") ^ " " ^ i
    | UsedAs(s) -> "the property " 
                    ^ tenses negated past ("is", "is not", "has been", "has not been")
                    ^ " used for " ^ s
    | State(s) -> s ^ " " ^ tenses negated past ("becomes", "does not become", "has become", "has not become") ^ " a state"
    | Reentry(p) ->  (P.to_string p) ^ " " ^ tenses negated past ("reenters", "does not reenter", "has reentered", "has not reentered")
    | Years(n) ->  sprintf "%d years remain" n
    | Known(d) -> D.to_string d ^ " " ^ (if D.plural d then "are" else "is") ^ " " ^ (if negated then "unknown" else "known")
  
   let events ap = match ap with
   | IsAlive p ->  ESet.singleton (Dies p)
   | HasIssue p -> ESet.singleton (FailureOfIssue p)
   | Survives(p1,p2) ->  ESet.add (ESet.singleton (Dies p1)) (Dies p2)
   | Marry(p1,None) -> ESet.singleton (Marry (p1,None))
   | Marry(p1,Some p2) -> ESet.singleton (Marry (p1,Some p2))
   | IsMarried(p1,None) -> ESet.singleton (Marry (p1,None))
   | IsMarried(p1,Some p2) -> ESet.singleton (Marry (p1,Some p2))
   | Divorce(p1,None) -> ESet.singleton (Divorce (p1, None))
   | Divorce(p1,Some p2) -> ESet.singleton (Divorce (p1, Some p2))
   | Consume(p,s) -> ESet.singleton (Consumes (p,s))
   | Graduate(p,i) -> ESet.singleton (Graduates (p,i))
   | UsedAs(s) -> ESet.singleton (IsUsedAs (s))
   | State(s) -> ESet.singleton (BecomesState (s))
   | Reentry(p) ->  ESet.singleton (Reenters (p))
   | Years(n) ->ESet.singleton (YearsPass (n))
   | Known(_) -> ESet.empty (*JG: Wrong *)
  
  
    let eval ap h =
      let (=) = Poly.(=) in
      match ap with
      | IsAlive p  -> F.is_alive_after p h
      | HasIssue p -> F.has_issue p h
      | Survives (p1,p2) -> (match (F.survivor p1 p2 h) with | FirstP -> true | SecondP -> false | Neither -> false)
      | Marry(p,None) -> ( match h with
          | Marry(p1,None)::_ when p = p1 -> true
          | Marry(p1,Some p2)::_ when p = p1 || p = p2 -> true
          | _ -> false )
      | Marry(p1,Some p2) -> ( match h with
          | Marry(p3,Some p4)::_ when p1 = p3 && p2 = p4 -> true
          | Marry(p3,Some p4)::_ when p1 = p4 && p2 = p3 -> true
          | Marry(p3,None)::_    when p1 = p3 || p2 = p3 -> true
          | _ -> false )
      | IsMarried(p1,None) -> (match F.spouse_of p1 h with
          | Known _   -> true
          | Unknown   -> true
          | Unmarried -> false)
      | IsMarried(p1,Some p2) -> (match F.spouse_of p1 h with
          | Known p' when p' = p2 -> true
          | _ -> false)
      | Divorce(p,None) -> ( match h with
          | Divorce(p1,None)::_ when p = p1 -> true
          | Divorce(p1,Some p2)::_ when p = p1 || p = p2 -> true
          | _ -> false )
      | Divorce(p1,Some p2) -> ( match h with
          | Divorce(p3,Some p4)::_ when p1 = p3 && p2 = p4 -> true
          | Divorce(p3,Some p4)::_ when p1 = p4 && p2 = p3 -> true
          | Divorce(p3,None)::_    when p1 = p3 || p2 = p3 -> true
          | _ -> false )
      | Consume(p,s) -> ( match h with
          | Consumes(p1,s1)::_ when p = p1 && s = s1 && F.is_alive_after p h -> true
          | _ -> false )
      | Graduate(p,i) -> ( match h with
          | Graduates(p1,i1)::_ when p = p1 && i = i1 && F.is_alive_after p h -> true
          | _ -> false )   
      | UsedAs(s) -> List.mem h ~equal:(=) (IsUsedAs s) 
      | State(s) -> List.mem h ~equal:(=) (BecomesState s)
      | Reentry(p) -> ( match h with
          | Reenters(p1)::_ when p = p1 -> true (* Reentry allowed by heirs *)
          | _ -> false )
      | Years(n) -> (F.years_passed h) >= n
      | Known(d) -> (CG.closed d h) || Poly.(CG.eval d h <> [])
  
    let always ap h =
      match ap with
      | IsAlive _ -> false
      | HasIssue _ -> false
      | Survives (_,_) -> eval ap h
      | Marry (_,_) -> false
      | IsMarried(_,_) -> false
      | Divorce (_,_) -> false
      | Consume (_,_) -> false
      | Graduate (_,_) -> false
      | Reentry (_) -> false
      | UsedAs (_) -> eval ap h
      | State (_) -> eval ap h
      | Years(_) -> false
      | Known(d) -> (CG.closed d h) ||  Poly.(CG.eval d h <> [])
  
    let never ap h =
      match ap with
      | IsAlive p -> F.is_dead_after p h
      | HasIssue p -> List.mem h ~equal:Poly.(=) (FailureOfIssue p)
      | Survives (p1,p2) ->  always (Survives(p2,p1)) h
      | Marry (p1,None) -> F.is_dead_after p1 h
      | Marry (p1,Some p2) -> F.is_dead_after p1 h || F.is_dead_after p2 h
      | IsMarried(p1,None) -> F.is_dead_after p1 h
      | IsMarried(p1,Some p2) -> F.is_dead_after p1 h || F.is_dead_after p2 h
      | Divorce (p1,None) -> F.is_dead_after p1 h
      | Divorce (p1,Some p2) -> F.is_dead_after p1 h || F.is_dead_after p2 h
      | Consume (p,_) -> F.is_dead_after p h
      | Graduate (p,_) -> F.is_dead_after p h
      | Reentry (_) -> false
      | UsedAs (_) -> false
      | State (_) -> false
      | Years (_) -> false
      | Known(_) -> false 
  
      let must_wait ap = match ap with
      | Years(_) -> true
      | _        -> false
  
      let possibly_remote ap h = match ap with
      | IsAlive _ -> false
      | HasIssue _ -> true
      | Survives _ -> false
      | Marry _ -> false
      | IsMarried _ -> false
      | Divorce _ -> false
      | Consume _ -> false
      | Graduate _ -> false
      | UsedAs _ -> true
      | State _ -> true
      | Reentry _ -> false
      | Years n -> (n > 21)
      | Known(d) -> CG.possibly_remote_ascertain d h
  
  end
  
  
  
  module Predicate = struct
  
    module ESet = EventSet
    module A  = AtomicPredicate
    module E = Event
  
    type t =
      | True
      | False
      | Atomic of A.t
      | Occurs of E.t
      | Past of t
      | Not of t
      | Or of t * t
      | And of t * t 
      | Never of t [@@deriving sexp]
  
    let rec to_string pr = match pr with
      | True -> "true"
      | False -> "false"
      | Atomic p -> A.to_string p
      | And(Not p1, Not (Never p2)) when Poly.(p1 = p2) ->
        to_string (Not p1) ^ " (but it is still possible)"
      | Not (Atomic (ap)) -> A.to_string ap ~negated:true
      | Past (Atomic (ap)) -> A.to_string ap ~past:true
      | Not (Past (Atomic (ap))) -> A.to_string ap ~negated:true ~past:true
      | Occurs e -> sprintf "%s" (E.to_string e)
      | Past pr1 -> sprintf "past (%s)" (to_string pr1)
      | Not pr1 -> sprintf "not %s" (to_string pr1)
      | And (pr1,pr2) -> sprintf "%s and %s" (to_string pr1) (to_string pr2)
      | Or (pr1,pr2) -> sprintf "%s or %s" (to_string pr1) (to_string pr2)
      | Never pr1 -> sprintf "%s is impossible" (to_string pr1)
  
    let to_json pr = to_string pr
    let rec events pr = match pr with
      | True
      | False -> ESet.empty
      | Atomic ap -> A.events ap
      | Occurs e -> ESet.singleton e
      | Past pr1 -> events pr1
      | Not pr1 -> events pr1
      | Or (pr1,pr2)
      | And (pr1,pr2) -> ESet.union (events pr1) (events pr2)
      | Never pr1 -> events pr1
  
  let (<=>) = E.(=)
  
    let rec eval pr h  = match pr with
      | True -> true
      | False -> false
      | Atomic ap -> A.eval ap h
      | Occurs e -> (match List.hd h with
          | Some e' -> e' <=> e
          | None -> false)
      | Past pr1 ->  (eval pr1 h || match List.tl h with
          | Some h -> eval (Past pr1) h
          | None         -> false )
      | Not p1-> not (eval p1 h)
      | Or (p1,p2) -> eval p1 h || eval p2 h
      | And (p1,p2) -> eval p1 h && eval p2 h
      | Never pr1 -> never pr1 h
    (* Makes logically entailed simplifications to predicates *)
  and simplify p h =
      let rec simplify' pr = match pr with
      | True -> True
      | False -> False
      | Atomic ap -> Atomic ap
      | Occurs(e) -> Occurs (e)
      | Past(pr1) ->  if eval pr h then True else Past (simplify' pr1) (*Once something has been true, it always will have *)
      | Not(Not(pr1)) -> simplify' pr1
      | Not(Or(pr1,pr2)) -> And(Not(simplify' pr1),Not(simplify' pr2))
      | Not(And(pr1,pr2)) -> Or(Not(simplify' pr1),Not(simplify' pr2))
      | Or(False,pr1) | Or(pr1,False) -> simplify' pr1
      | Or(True,_) | Or(_,True) -> True
      | Or(pr1,pr2) -> Or(simplify' pr1, simplify' pr2)
      | And(True,pr1) | And(pr1, True) -> simplify' pr1
      | And(False,_) | And(_, False) -> False
      | And(pr1,pr2) -> And(simplify' pr1, simplify' pr2)
      | Not (True) -> False
      | Not (False) -> True
      | Not(pr1) -> Not (simplify' pr1)
      | Never(pr1) -> if (never pr1 h) then True else Never (simplify pr1 h)
       in
     let p' = simplify' p in
     if Poly.(=) p p' then p else simplify p' h
  and always p h  : bool = match p with
      | True -> true
      | False -> false
      | Atomic ap -> A.always ap h
      | Occurs _ -> false
      | Past _ -> eval p h (* If p1 currently has been true, it always will have been true *)
      | Not p1 -> never p1 h
      | Or(p1,p2) -> always p1 h || always p2 h   (* If more is known, use AtomicPredicate *)
      | And(p1,p2) -> always p1 h && always p2 h
      | Never(pr1) -> never pr1 h
    and
      never p h : bool = match p with
      | True -> false
      | False -> true
      | Atomic ap -> A.never ap h
      | Occurs _ -> false       (* If more is known, use AtomicPredicate *)
      | Past p1 -> not (eval p h) && never p1 h
      | Not p1 -> always p1 h
      | Or(p1,p2) -> never p1 h && never p2 h 
      (* AM: always p1 h && always p2 h ?
             not sure how always and never are interacting and de-morganing *)
      | And(p1,p2) -> never p1 h || never p2 h  (* Too conservative *)
      | Never (_) -> false  (* Too conservative *)
  
      let rec possibly_remote pr h  =  match pr with
      | True -> false
      | False -> false
      | Atomic ap -> (A.possibly_remote ap h)
      | Occurs e -> (E.possibly_remote e)
      | Past pr1 -> (possibly_remote pr1 h)
      | Not pr1 -> (possibly_remote pr1 h)
      | Or (pr1,pr2) -> (possibly_remote pr1 h) || (possibly_remote pr2 h)
      | And (pr1,pr2)  -> (possibly_remote pr1 h) || (possibly_remote pr2 h) (* Possilbly remote if _either_ is possibly remote *)
      | Never (pr1)  -> (possibly_remote pr1 h)
  
     (* This is currently far too simple; it deals only with fixed times *)
      let rec must_wait pr = match pr with
      | True -> false
      | False -> false
      | Atomic ap -> A.must_wait ap
      | Occurs _ -> false
      | Past pr1 -> must_wait pr1
      | Not pr1  -> must_wait pr1
      | Or (pr1,pr2)   -> must_wait pr1 || must_wait pr2
      | And (pr1,pr2)  -> must_wait pr1 || must_wait pr2
      | Never (pr1)    -> must_wait pr1
     
  
  
    (* TODO(basus): fill out this stub *)
    let implies _ _ = false
  end
  module PredicateType = Predicate
  type predicate = PredicateType.t [@@deriving sexp]
  
  

