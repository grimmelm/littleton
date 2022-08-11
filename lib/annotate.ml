open Core_kernel
open Term
open Common
open Classify
open Interest
open Condition
open Classgift

module U = UnnamedTerm
module N = NamedTerm
module F = Family
module CG = ClassGifts

(* Class gifts interface for interests *)

module CGI = struct
  let cgi_apply f (i:U.interest) = f i.grantee i.history 
  let eval = cgi_apply CG.eval
  let always = cgi_apply CG.always
  let closed = cgi_apply CG.closed
  let empty = cgi_apply CG.empty
  let track = cgi_apply CG.track
  let possibly_fail_ascertain = cgi_apply CG.possibly_fail_ascertain
  let possibly_remote_ascertain = cgi_apply CG.possibly_remote_ascertain
end



  
module type FORWARD = sig
  type t [@@deriving sexp]
  val init :  t
  val zero : t
  val before : term -> t -> t
  val f_or : t -> t -> t
  val after : term -> t -> t -> t
end 

(* Forward: is this interest reachable? *)
module Reachability : FORWARD with type t = bool = struct

  type t = bool [@@deriving sexp]
  let init = true
  let zero = false
  let before _ v = v
  let f_or v1 v2 = v1 || v2
  let after (t:term) v_in v_out = match t with
    | Bottom       -> v_in
    | Atom _       -> false
    | Class (i,_)  -> CGI.possibly_fail_ascertain i || v_out
    | While(_,_) -> v_in
    | Tail(_,_,_)    -> v_in
    | _            -> v_out

end

(* Forward: is this interest possessory? *)
module Possessory : FORWARD with type t = bool = struct
  type t = bool [@@deriving sexp]
  let init = true
  let zero = false
  let before _ v = v
  let f_or v1 v2 = v1 || v2
  let after t _ v_out = match t with
  | U.Atom _         -> false
  | _                -> v_out
end 

(* Forward: can this interest divest a previous interest? *)
module Divesting : FORWARD with type t = divest_t = struct
    type t = divest_t [@@deriving sexp]
    let init: t = NoDivest
    let zero: t = NoDivest
    let before (_:term) (v:divest_t) : divest_t = v  
    let f_or = Poly.max
    let after (t:term) _ v_out:divest_t = 
      match t with
    | While({source = ImpliedReversion;_},_) -> Springing
    | While({source = Executory;_},_)        -> Shifting
    | While({source = Subsequent;_},_)       -> Termination
    | Atom i -> NoDivest
    | _                     -> v_out
end

(* Forward: does this interest follow a fee simple determinable? *)
module FeeSimpleDeterminable : FORWARD with type t = bool * bool =  struct
  type t = bool * bool [@@deriving sexp]
  let init = (false, false)
  let zero = (false,false)
  let before _ v = v
  let f_or (v1d,v1l) (v2d,v2l) = (v1d || v2d, v1l || v2l)
  let after (t:term) _ (v_out_l,v_out_d) = match t with
  | Atom _               -> (false,   false)   (* reset *)
  | While({source = Natural _;_},_) -> (v_out_l, true)    (* there is a duration less than fee simple *)
  | While({source = Added;_},_)     -> (true,    v_out_d) (* there is a limitation *)
  | Tail(_,_,_)            -> (v_out_l,true)  (* there is a duration less than fee simple *)
  | _                      -> (v_out_l, v_out_d) (* pass through *)

end 

(* Forward: is this interest subject to an implicit condition precedent? *)
module ImplicitCondition : FORWARD with type t = bool = struct
  type t = bool  [@@deriving sexp]
  let init = false
  let zero = false
  let before _ v = v
  let f_or v1 v2 = v1 || v2
  let after (t:term) v_in v_out = match t with
  | U.Atom _                 -> true
  | U.While({source = Natural _;_},_)   -> v_in
  | U.Tail(_,_,_)              -> v_in
  | _                        -> v_out

end

(* Forward: must this interest wait after all previous interests have terminated to take possession *)
module Wait : FORWARD with type t = bool =  struct
  type t = bool [@@deriving sexp]
  let init = false
  let zero = false
  let before _ v = v 
  let f_or v1 v2 = v1 && v2
  let after (t:term) _ v_out = match t with
  | Atom _                -> false  (* reset *)
  | While({source = Precedent;_} as c,_)  -> UnnamedCondition.must_wait c
  | _                     ->  v_out (* pass through *)
end

module Remote : FORWARD with type t = remoteness = struct
  
  type t = remoteness [@@deriving sexp]
  let init = RNone
  let zero = RNever 
  let before (t:term) v = match t with 
  | Class(i,_) -> ( match v, CGI.possibly_remote_ascertain i with 
                    | RNever, _     -> RNever
                    | _, true     -> RRemoteCondition
                    | _, _          -> v )
  | If(c,_,_)   ->  ( match v,  UnnamedCondition.possibly_remote c, !Parameters.destructibility_of_contingent_remainders with 
                    | RNever, _, _      -> RNever
                    | _, true, false     -> RRemoteCondition
                    | RRemote, true, _  -> RRemoteCondition
                    | _, _, _           -> v )
  | _  -> v

  let f_or cp1 cp2 = match cp1, cp2 with
  | RNone, _      | _, RNone      -> RNone
  | RRemote, _    | _, RRemote    -> RRemote  (* This is out of min-max order!*)
  | RCondition, _ | _, RCondition -> RCondition
  | RRemoteCondition, _ | _, RRemoteCondition -> RRemoteCondition
  | RNever, RNever -> RNever

  let r_then v_in c = match c.source, v_in  with 
  | Source.Natural _, _ -> v_in (* These expire naturally within a life in being *)
  | _ , RNone 
  | _, RCondition -> if UnnamedCondition.possibly_remote c then RRemoteCondition else RCondition
  | _, RRemote    
  | _, RRemoteCondition -> RRemoteCondition
  | _, RNever -> RNever

  let after (t:term) v_in v_out = match t with
  | Bottom        -> v_in
  | Atom _        -> RNever 
  | Class (i,_)   ->  if CGI.possibly_fail_ascertain i then
                        if CGI.possibly_remote_ascertain i then RRemote
                        else v_in
                      else RNever
  | While (c,_) -> f_or (r_then v_in c) v_out
  | Tail _ -> ( match v_in with
                | RNever -> RNever
                | RRemoteCondition | RCondition -> RRemoteCondition
                | RRemote | RNone -> RRemote )
  | _ -> v_out

end

module Annotation = struct

  module R = Reachability
  module P = Possessory
  module D = Divesting
  module F = FeeSimpleDeterminable
  module I = ImplicitCondition
  module W = Wait
  module RM = Remote

type forward_t =
  {
    r:   R.t;
    p:   P.t;
    d:   D.t;
    fsd: F.t;
    imcp: I.t;
    wait: W.t;
    rap : RM.t;
  }

type downward_t =
  {
    q: duration;
    i: bool;
    excp: bool;
    asc: bool;
    op: bool;
    div: bool;
  }

type backward_t =
  {
    ar: bool;
    l: FullDuration.t;
  }

(* Common code to convert between named and unnamed terms *)
let name (t:term) =

  let open Source in

(* Downward: what is this interest's quantum? *)
  let quantum_init = Duration.Absolute in
  let quantum_class_description_in _ v = v in
  let quantum_class_share_in _ _ v = v in
  let quantum_in t v = match t with
  | U.While({source = Natural d;_},_)  -> Duration.min v d
  | U.Tail({owner = Person p;_},_,_) -> Duration.min v (Duration.InTail p)
  | _ -> v
  in

(* Downward: can this interest become possessory immediately following the previous interest's termination? *)
  let immediate_init = true in
  let immediate_class_description_in _ v = v in
  let immediate_class_share_in _ _ v = v in
  let immediate_in _ v = v  in

(* Downward: is this interest subject to an explicit condition precedent? *)
  let excp_init = false in
  let excp_class_description_in _ v = v in
  let excp_class_share_in _ _ v = v in 
  let excp_in t v  = match t with
    | U.If(_,_,_) -> true
    | _           -> v
  in

(* Downward: is this interest ascertained? *)
  let ascertain_init = true in
  let ascertain_class_description_in _ _ = false in (* If it were ascertained, it woudln't be a Class node *)
  let ascertain_class_share_in t h v = match t with
  | U.Class(i,_) 
    -> List.Assoc.mem (CGI.always i) ~equal:Poly.(=) h && v 
  | _ -> v in(* Any _other_ nodes are ascertained enough to be identified *)
  let ascertain_in _ v = v in

  (* Downward: is this interest subject to open? *)
  let open_init = false in
  let open_class_description_in t v = match t with
    | U.Class (i,_) -> v || not (CGI.closed i) (* Open if a parent is, or the class is open *)
    | _ -> v in
  let open_class_share_in t _ v = match t with
    | U.Class (i,_) -> v || not (CGI.closed i) (* Open if a parent is, or the class is open *)
    | _ -> v in
  let open_in _ v = v in

  (* Downward: is this interest subject to divestment? *)
  let class_divest_init = false in
  let class_divest_class_description_in _ v = v in (* The description is always contingent, so irrelevant *)
  let class_divest_class_share_in t _ v = match t with
    | U.Class (_,_) -> v (*JG: wrong*)
      (* not (List.mem (CG.locked_members i.grantee i.history) ~equal:Poly.(=)  p) || v *)
    | _ -> v in
  let class_divest_in _ v = v in
  
(* Backward: is the next interest reversionary? *)
  let after_reversionary_init = false in
  let after_reversionary_in _ v = v   in
  let after_reversionary_after t _ v_out = match t with
  | U.Atom (i)          -> Unnamed.is_reversionary i
  | _                     -> v_out
  in

(* Backward: what limitations is this interest subject to? *)
  let limitations_init = FullDuration.make Duration.Absolute in
  let limitations_in t v ar = match t with
    | U.While(c,_)      -> Source.to_duration c.source v ar
    | _                     -> v
    in
  let limitations_after _ v_in _ = v_in in


  let share_init = Q.one in

  let forward_init =
    { r = R.init; p = P.init; d = D.init; fsd = F.init; imcp = I.init; wait = W.init; rap = RM.init} in
  let forward_zero = 
    { r = R.zero; p = P.zero; d = D.zero; fsd = F.zero; imcp = I.zero; wait = W.zero; rap = RM.init} in

  let forward_in (t:term) v =
    {
    r = R.before    t v.r;
    p = P.before    t v.p;
    d = D.before    t v.d;
    fsd = F.before  t v.fsd;
    imcp = I.before  t v.imcp;
    wait = W.before t v.wait;
    rap = RM.before t v.rap;
    }
  in

  let forward_after_apply t (v_in:forward_t) (v_out:forward_t):forward_t =
    { r = R.after   t  v_in.r     v_out.r;
      p = P.after   t  v_in.p     v_out.p;
      d = D.after   t  v_in.d     v_out.d;
      fsd = F.after t  v_in.fsd   v_out.fsd;
      imcp = I.after       t  v_in.imcp  v_out.imcp;
      wait = W.after t  v_in.wait  v_out.wait;
      rap  = RM.after t  v_in.rap  v_out.rap;
    }
  in

  let forward_or (v1:forward_t) (v2: forward_t):forward_t =
    {
      r = R.f_or     v1.r     v2.r;
      p = P.f_or     v1.p     v2.p;
      d = D.f_or     v1.d     v2.d;
      fsd = F.f_or   v1.fsd   v2.fsd;
      imcp = I.f_or  v1.imcp  v2.imcp;
      wait = W.f_or  v1.wait  v2.wait;
      rap  = RM.f_or v1.rap v2.rap;
    }
    in

  let rec forward_after (t:term) v:forward_t =
    let v_out =  match t with
    | U.Bottom          -> v
    | U.Atom _          -> v
    | U.Class (i,ts) 
       -> forward_or (forward_after (Atom i) (forward_in (Atom i) v))
                      (List.fold ts ~init:forward_zero 
                                    ~f:(fun acc (_,t') -> forward_or acc (forward_after t' (forward_in t' v))))
    | U.Shared (_,ts)   -> List.fold ts ~init:forward_zero ~f:(fun acc (t',_) -> forward_or acc (forward_after t' (forward_in t' v))) (* JG: This is wrong *)
    | U.While(_,t1)   -> forward_after t1 (forward_in t v)
    | U.If(_,t1,t2)     -> forward_or (forward_after t1 (forward_in t1 v)) (forward_after t2 (forward_in t2 v))
    | U.Seq(t1,t2)      -> forward_after t2 (forward_after t1 (forward_in t v))
    | U.Tail(_,_,t1)      -> forward_after t1 (forward_in t v)
    in forward_after_apply t v v_out
  in

  let downward_init =
    {
      q = quantum_init;
      i = immediate_init;
      excp = excp_init;
      asc = ascertain_init;
      op = open_init;
      div = class_divest_init;
    }
   in

   let downward_class_description_in t v =
    {
      q = quantum_class_description_in t v.q;
      i = immediate_class_description_in t v.i;
      excp = excp_class_description_in t v.excp;
      asc = ascertain_class_description_in t v.asc;
      op = open_class_description_in t v.op;
      div = class_divest_class_description_in t v.div;
    }
    in

    let downward_class_share_in t h v =
      {
        q = quantum_class_share_in t h v.q;
        i = immediate_class_share_in t h v.i;
        excp = excp_class_share_in t h v.excp;
        asc = ascertain_class_share_in t h v.asc;
        op = open_class_share_in t h v.op;
        div = class_divest_class_share_in t h v.div
      }
      in

   let downward_in t v =
    {
      q = quantum_in t v.q;
      i = immediate_in t v.i;
      excp = excp_in t v.excp;
      asc = ascertain_in t v.asc;
      op = open_in t v.op;
      div = class_divest_in t v.div;
    }
    in


  let backward_init =
     {
       ar = after_reversionary_init;
       l = limitations_init;
     }
  in

  let backward_in t v =
    {
      ar = after_reversionary_in   t v.ar;
      l = limitations_in           t v.l v.ar;
    }
  in

  let backward_after_apply t (v_in:backward_t) (v_out:backward_t):backward_t =
    {
      ar = after_reversionary_after t v_in.ar v_out.ar;
      l = limitations_after         t v_in.l  v_out.l;
    }
  in

  let rec backward_after t v:backward_t = match t with
    | U.Bottom        -> backward_after_apply t v v
    | U.Atom _          -> backward_after_apply t v v
    | U.Class _    -> backward_after_apply t v v (* JG: This is wrong *)
    | U.Shared _          -> backward_after_apply t v v (* JG: This is wrong *)
    | U.While(_,t1) -> backward_after_apply t v (backward_after t1 (backward_in t v))
    | U.If(_,t1,_)      -> backward_after_apply t v (backward_after t1 (backward_in t v))
    | U.Seq(t1,t2)    -> backward_after_apply t v (backward_after t1 (backward_after t2 (backward_in t v)))
    | U.Tail(_,_,t1)    -> backward_after_apply t v (backward_after t1 (backward_in t1 v))
  in


  let name_interest (t:unnamed_interest) (f, d, b, s) =
    let module V = Vesting in
    let fsd = let (lim,dur) = f.fsd in (lim && not dur) in
    let scp = f.imcp || d.excp in
    { owner     = t.owner ;
      grantee   = t.grantee ;
      grantor   = t.grantor ;
      history   = t.history ;
      reachable = f.r;
      quantum   = d.q;
      immediate = d.i;
      divest    = f.d;
      afterfsd  = fsd;
      scp       = scp;
      wait      = f.wait;
      nature    = if f.p                               then Nature.Possessory
                  else if (Unnamed.is_reversionary t)
                    then if (Poly.(f.d = Termination)) then Nature.PowerOfTermination
                      else if scp                      then Nature.PossibilityOfReverter
                                                       else Nature.Reversion
                  else if d.i && Poly.(f.d = NoDivest) 
                          && not fsd && not f.wait     then Nature.Remainder
                                                       else Nature.ExecutoryInterest;
      vesting = 
        V.min (V.min (if scp then V.Contingent else V.Vested)
                     (if FullDuration.has_limit b.l then V.VestedSubject else V.Vested))
             (if not d.asc then V.Contingent 
             else if d.div then V.VestedSubject
             else if d.op then V.VestedOpen
             else V.Vested) ;
      duration = {b.l with quantum = d.q};
      share   = s;
      remote = f.rap;
    } 
  in

  let name_condition (c:unnamed_condition) (_, _, _, _): named_condition =
    let c_rap = if UnnamedCondition.possibly_remote c then RRemoteCondition else RNone in
    (c, c_rap) in

  let rec name' (t:term) (f, d, b, s):N.t =
    let f1 = forward_in   t f in
    let d1 = downward_in  t d in
    let b1 = backward_in  t b in
    let fdbs1 = (f1,d1,b1,s) in
    match t with
    | U.Bottom           -> N.Bottom
    | U.Atom (i)        -> N.Atom(name_interest i fdbs1)
    | U.Class (i,ts)  -> 
      let d_desc = downward_class_description_in t d1 in
        N.Class ((name_interest i (f1,d_desc,b1,s)),
                      List.map ts ~f:(fun (p,t') -> 
                        let h_shares = CGI.track i in
                        let n = List.Assoc.find_exn h_shares p ~equal:Poly.(=) in
                        (* let total = Shares.total h_shares in *)
                        (* let s1 = (s_num * n, s_den * total) in *)
                        let d_share = downward_class_share_in t p d1 in
                        let s1 = Q.(n * s) in
                        (p, name' t' (f1,d_share,b1,s1))))
    | U.Shared (ct, ts) -> N.Shared (ct, (List.map ts ~f:(fun (t',n) -> 
                            let s1 = Q.(s * n) in   
                            (name' t' (f1,d1,b1,s1), n))) )
    | U.While(c,t1)   -> N.While(name_condition c fdbs1, name' t1 fdbs1)
    | U.If (c,t1,t2)    -> N.If(name_condition c fdbs1, name' t1 fdbs1, name' t2 fdbs1)
    | U.Seq (t1,t2)     -> N.Seq(
                             name' t1 (f1, d1, backward_after t2 b1, s),
                             name' t2 (forward_after t1 f1, d1, b1, s))
    | U.Tail (i,ps,t1)  -> N.Tail(name_interest i fdbs1, ps, name' t1 fdbs1)

  in
  name' t (forward_init, downward_init, backward_init, share_init)

let unname_i i = {grantor = i.grantor; grantee = i.grantee; owner = i.owner; history = i.history}
let unname_c (c,_) = c
let rec unname : N.t -> term = function 
  | Bottom        -> Bottom
  | Atom (i)      -> Atom(unname_i i)
  | Class (i,l)   -> Class((unname_i i),List.map l ~f:(fun (p,t') -> (p,unname t') ))
  | Shared (ct,l) -> Shared(ct, List.map l ~f:(fun (t',n) -> unname t', n))
  | Seq (t1, t2)  -> Seq(unname t1, unname t2)
  | While (c,t) -> While(unname_c c, unname t)
  | If (c,t1,t2)  -> If(unname_c c, unname t1 , unname t2)
  | Tail (i,ps,t1)   -> Tail(unname_i i, ps, unname t1)

end

 
