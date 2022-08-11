open Core_kernel
open Common
open Event
open Classify

module type INTEREST = sig
  type t [@@deriving sexp]

  val self : person -> t
  val owner : t -> holder
  val grantor : t -> person
  val grantee : t -> description
  val history : t -> history
  val create : person -> description -> t
  val transfer_to : t -> holder -> t
  val create_vested : person -> description -> holder -> t
  val add_event : t -> event -> t
  val replace_history : t -> history -> t
  val is_reversionary : t -> bool
  val to_string : t -> string
  val to_abstract : t -> string
end

type unnamed_interest = 
  { grantor: person
  ; grantee: description
  ; owner: holder
  ; history : history} [@@deriving sexp]

module Unnamed : INTEREST with type t = unnamed_interest = struct
  type t = unnamed_interest [@@deriving sexp]

  let create grantor grantee = {grantor; grantee; owner = Description grantee; history = []}
  let transfer_to i new_owner = {i with owner = new_owner}
  let create_vested grantor grantee owner = transfer_to (create grantor grantee) owner
  let add_event t e = {t with history = e::t.history}
  let replace_history t history = {t with history}
  let self p = {grantor = p; grantee = Description.Individuals [p]; owner = Person p; history = []}
  let owner i = i.owner
  let grantor i = i.grantor
  let grantee i = i.grantee
  let history i = i.history

  let is_reversionary t =  Poly.(Description.root_of_title t.grantee = Some t.grantor)
    (* let (=) = Poly.(=) in
    List.mem (Description.root_of_title t.grantee) ~equal:(=) t.grantor *)

  let to_string t = Holder.to_string t.owner

  let to_abstract t = sprintf "To(i, %s, %s)" (Person.to_string t.grantor) (Description.to_string t.grantee)
end

type named_interest  =
  { grantor   : person
  ; grantee   : description
  ; owner     : holder 
  ; history   : history
  ; reachable : bool
  ; quantum   : Duration.t
  ; immediate : bool
  ; divest    : divest_t
  ; afterfsd  : bool
  ; wait      : bool
  ; scp       : bool
  ; nature    : Nature.t
  ; duration  : FullDuration.t
  ; vesting   : Vesting.t 
  ; share     : share_t
  ; remote    : remoteness  } [@@deriving sexp]

module Named : INTEREST with type t = named_interest = struct

  type t = named_interest [@@deriving sexp]

  let owner i = i.owner
  let grantor i = i.grantor
  let grantee i = i.grantee
  let history i = i.history
  let create grantor grantee =
    { grantor   = grantor
    ; grantee   = grantee
    ; owner     = Description grantee
    ; history   = []
    ; reachable = true
    ; nature    = Nature.Possessory
    ; quantum   = Duration.Absolute
    ; immediate = true
    ; divest    = NoDivest
    ; afterfsd  = false
    ; wait      = false
    ; scp       = false
    ; duration  = FullDuration.make Duration.Absolute
    ; vesting  = Vesting.Possessory 
    ; share    = Q.one
    ; remote   = RNone}

  let transfer_to i new_owner = {i with owner = new_owner}
  let create_vested grantor grantee owner = transfer_to (create grantor grantee) owner
  let add_event t e = {t with history = e::t.history}  
  let replace_history t history = {t with history}
  let self p = let i = create p (Description.Individuals [p]) in {i with owner = Person p;}

  let is_reversionary t =  Poly.(Description.root_of_title t.grantee = Some t.grantor)
  (* let is_reversionary t = 
    let (=) = Poly.(=) in
    List.mem (Description.root_of_title t.grantee) ~equal:(=) t.grantor *)

  let to_string i = match !Parameters.verbosity with 
  | Parameters.Verbosity.Verbose 
    -> String.concat ~sep:" "
        [  Holder.to_string i.owner;
          (if Holder.plural i.owner then "have a" else "has a");
          if Poly.(i.nature = Nature.Remainder) then Vesting.to_string i.vesting 
          else if Poly.(i.nature = Nature.ExecutoryInterest) then Divest.to_string i.divest 
          else "";  
          if Q.(i.share = one) then ""
          else if Q.(i.share = zero) then ""
          else sprintf "%d/%d share of a" 
          (i.share |> Q.num) 
          (i.share |> Q.den) ;
          Nature.to_string i.nature;
          "in";
          FullDuration.to_full_string i.duration i.owner;        
        ]
    | Parameters.Verbosity.Brief
      -> String.concat ~sep:" "
        [  Holder.to_string i.owner;
        (if Holder.plural i.owner then "have a" else "has a");
        if Q.(i.share = one) then ""
        else if Q.(i.share = zero) then ""
        else sprintf "a %d/%d share of a" 
        (i.share |> Q.num)
        (i.share |> Q.den);
        Nature.to_string i.nature;
        "in";
        FullDuration.to_full_string i.duration i.owner;        
      ]
     | Parameters.Verbosity.Superbrief
        -> Holder.to_string i.owner

  let to_abstract t = sprintf "To(i, %s, %s)" (Person.to_string t.grantor) (Holder.to_string t.owner)
end


