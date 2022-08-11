open Core_kernel
open Common
open Event
open Graph

module E = Event
module P = Person

(* HERE BE GRAPH-DRAGONS *)

(** This is the type of nodes in the family graph. *)
module Entity = struct

  (* A node in the graph is either a person, or a GrimReaper node, that
     has edges to people who are dead. This leaves open the option for
     entities to be non-people in the future. *)
  type t = Person of P.t | GrimReaper [@@deriving hash]

  let equal p1 p2 =
    match p1, p2 with
    | Person p1, Person p2 -> P.equal p1 p2
    | GrimReaper, GrimReaper -> true
    | _ -> false

  let compare p1 p2 =
    match p1, p2 with
    | Person p1, Person p2 -> Person.compare p1 p2
    | GrimReaper, GrimReaper -> 0
    | Person _, GrimReaper -> 1
    | GrimReaper, Person _ -> -1

  let person (e:t) : P.t option =
    match e with
    | Person p -> Some p
    | GrimReaper -> None

  let people l = List.filter_map ~f:person l

  let to_string (e:t) = match e with
    | Person p -> P.to_string p
    | GrimReaper -> "Grim Reaper"

end

(** This is the type of edges in the family graph. This relationship as
    defined prevents you from, e.g., marrying your child/parent.

  * Note: you are unrelated to your grandmother and can marry her.
    Decision: check programatically *)
module Relationship = struct

  (** An edge in the graph can represent familial relationships, or death *)
  type t = Child | Spouse | Widow | ExSpouse | Death

  (* Two edges between a pair of vertices will always be equal, and
     so will always clash. This gives us not-multigraphs, but this might be a
     source of bugs because Ocamlgraph expects {compare} to be a strict
     ordering relation. *)
  let compare _ _ = 0
  let default = Spouse
  let to_string (r:t) = match r with
    | Child -> "Child"
    | Spouse -> "Spouse"
    | Widow -> "Widow"
    | ExSpouse -> "ExSpouse"
    | Death -> "Death"
end

(** A persistent directed graph with concrete vertices and labeled edges **)
module G = Persistent.Digraph.ConcreteLabeled(Entity)(Relationship)

let to_string g =
  let edges = G.fold_edges_e (fun (src, rel, dst) edges ->
      let edge = (sprintf "%s --%s--> %s"
         (Entity.to_string src) (Relationship.to_string rel)
         (Entity.to_string dst) ) in
      edge::edges
    ) g [] in
  String.concat ~sep:"\n" edges


(** Check if the given person exists in the family graph **)
let knows_person p g = G.mem_vertex g (Person p)

(** Check if the given person is alive in the family graph. This assumes that
    anyone not explicitly dead in the graph is still alive, even if they're
    unknown. *)
let is_alive p g =
  let dead = (G.succ g Entity.GrimReaper) in
  if knows_person p g then
    not (List.mem (Entity.people dead) p ~equal:P.equal)
  else true

(** Check if the given person is dead in the given graph. *)
let is_dead p g = not (is_alive p g)



(** *** Checks for relationships between people *** **)

(** Do p1 and p2 have the relation r in either direction? *)
let have_relation p1 p2 (r:Relationship.t) g =
  ((G.mem_edge g (Person p1) (Person p2)) && phys_equal (G.E.label (G.find_edge g (Person p1) (Person p2))) r) ||
  ((G.mem_edge g (Person p2) (Person p2)) &&  phys_equal (G.E.label (G.find_edge g (Person p2) (Person p1))) r)

(** Get the list of mortal neighbors with any relation *)
let neighbors p g =
  Entity.people ((G.succ g (Person p)) @ (G.pred g (Person p)))

(** Find the spouse (singular) of the given person in the graph. It gives
    Known and Unknown spouses equal treatment. So far, `spouse` is the only
    symmetric relation in a directed graph, so this is a little more
    complicated. **)
let spouse_of (p: P.t) g =
  (* scan a list of relations for a spousal relation *)
  let rec spouse_of' edges f = match edges with
    | [] -> None
    | h :: t -> ( match (G.E.label h) with
        | Spouse -> Some (f h) (* dst for outgoing, src for incoming *)
        | _ -> spouse_of' t f
      ) in
  (* Only look for a spouse if the person is already known, otherwise assume
     they are unmarried. *)
  if knows_person p g then
    (* replicate for both in- and out-pointers *)
    match spouse_of' (G.succ_e g (Person p)) (G.E.dst), spouse_of' (G.pred_e g (Person p)) (G.E.src) with
    | Some (Person p'), _ | _, Some (Person p') -> Some p'
    | _, _ -> None
  else None

(** Are p1 and p2 married in the graph g?  *)
let are_married p1 p2 g =
  phys_equal (spouse_of p1 g) (Some p2) && phys_equal (spouse_of p2 g) (Some p1)

(** Can p1 and p2 married in the graph g?  *)
let can_marry p g = is_alive p g && phys_equal (spouse_of p g) None



(** *** Operations on relationships between people *** **)

(** Add the relationship r between people p1 and p2. Automatically adds p1
    and/or p2 if they are not in g already. *)
let add_rel p1 p2 r g =
  G.add_edge_e g (G.E.create (Person p1) r (Person p2))

(** Remove *all* the relations between p1 and p2 in the graph. Need to try
    both directions because the graph is directed. **)
let remove_relations p1 p2 g =
  if (knows_person p1 g) && (knows_person p2 g)
  then G.remove_edge (G.remove_edge g (Person p1) (Person p2)) (Person p2) (Person p1)
  else g

(** Add a spousal relationship, after clearing old relations, e.g. they could
    be ExSpouses getting remarried *)
let add_spouse p1 p2 g = add_rel p1 p2 Relationship.Spouse (remove_relations p1 p2 g)

(** Add a child relationship between the people *)
let add_child p1 p2 g = add_rel p1 p2 Relationship.Child g

(** If p1 and p2 have the relation r1 (in either direction), swap it with the
    relation r2 from p1 to p2 *)
let edit_relation p1 p2 r1 r2 g =
  if have_relation p1 p2 r1 g
  then add_rel p1 p2 r2 (remove_relations p1 p2 g)
  else g

(** For any incoming or outgoing spousal relations that p1 has, replace them
    with incoming widow relations. **)
let make_widow p1 g =
  List.fold_right (neighbors p1 g) ~init:g
    ~f:(fun p2 -> edit_relation p2 p1 Relationship.Spouse Relationship.Widow)



(** *** Functions that change the graph in response to events *** *)

(** Change the graph g according to the given event e *)
let family_state_one_event e g =
  match e with
  | E.Marry(p1,None) ->
    if can_marry p1 g
    then add_spouse p1 P.Anonymous g
    else g
  | E.Marry(p1, Some p2) ->
    if can_marry p1 g && can_marry p2 g then
      let g' = add_spouse p1 p2 g in
      add_spouse p2 p1 g'
    else g
  | E.Dies(p) ->
    if not (knows_person p g) then
      let g' = G.add_vertex g (Person p) in
      make_widow p (G.add_edge_e g' (G.E.create Entity.GrimReaper Death (Person p)))
      (* Child edge --> Dead edge *)
      (* Grim Reaper points to p with the Death edge *)
    else make_widow p (G.add_edge_e g (G.E.create Entity.GrimReaper Death (Person p)))
  | E.Divorce(p1,None) ->
    ( match spouse_of p1 g with
      | Some p2 -> edit_relation p1 p2 Spouse Relationship.ExSpouse g
      | None -> g (* p1 wasn't married *)
    )
  | E.Divorce(p1,Some p2) ->
    if are_married p1 p2 g
    then edit_relation p1 p2 Spouse Relationship.ExSpouse g
    else g
  | E.Child(p1,p2) ->
    if is_dead p1 g || knows_person p2 g then g else add_child p1 p2 g
  | _ -> g

(** Slurp up an entire history and create a graph version of it.

 * The history may have an event that contradicts a previous event, or is
 *  otherwise impossible.

 * In these cases the offending event is ignored.
 * e.g. [...no marriages for a or b...; a divorces b; ...more...]
    -> ignore the divorce
 * e.g. [a marries b; ...no divorces for a or b...; a marries c; ...more...]
    -> ignore the second wedding
 * In either case, the ...more... after the offending event _is_ added to the graph.
*)
let family_state (h : event list) =
  let init = G.add_vertex G.empty Entity.GrimReaper in
  (* Fold right because the most recent event is at the head of the list *)
  List.fold_right h ~init:init ~f:family_state_one_event

(** *** Public Interface Begins Here *** **)

(** *** Useful types *** **)

(** Type for who survives whom in a couple *)
type survivor = Neither | FirstP | SecondP [@@deriving sexp]

(** Type of (possible) spouses *)
type spouse = Unmarried | Unknown | Known of person [@@deriving sexp]

(** *** Find relationships of the person after the given history *** **)

(** Find the survivor after a given history *)
let survivor p1 p2 h =
  List.fold_right h
    ~init:Neither
    ~f:(fun e acc -> match acc, e with
        | Neither, E.Dies p when Poly.(p = p2) -> FirstP
        | Neither, E.Dies p when Poly.(p = p1) -> SecondP
        | _,_ -> acc )

(** Find the (singular) widow of person p after history h *)
let widow_of p h =
  let widows_of g =
    G.fold_pred_e
      (fun e acc ->
         match G.E.label e with
         | Widow -> G.E.src e :: acc
         | _ -> acc)
      g (Person p) [] in

  let g = family_state h in

  if knows_person p g
  then
    ( match widows_of g with
      | [] -> Unmarried
      | [Person p'] -> if (P.is_anon p') then Unknown else Known p'
      | _ -> failwith "Should not get here [widow_of]" )
  else Unmarried

(** Find the (singular) spouse of person p after history h *)
let spouse_of p h =
  match spouse_of p (family_state h) with
  | Some p' -> if (P.is_anon p') then Unknown else Known p'
  | None -> Unmarried

(** Find the children of person p after history h. Lists children from oldest
    to youngest, assuming that the children have been added to the graph in
    the order that they were born. *)
let children_of p h =
  let children_of' g =
    G.fold_succ_e
      (fun e acc ->
         match G.E.label e with
         | Child -> G.E.dst e :: acc
         | _ -> acc)
      g (Person p) [] in

  let g = family_state h in

  if knows_person p g
  then List.rev (Entity.people (children_of' g))
  else []

(** Find the grandchildren of person p after history h *)
let grandchildren_of p h : person list =
  let kids = children_of p h in
  List.concat (List.map kids ~f:(fun k -> children_of k h))

(** Find the parents of person p after history h *)
let parents_of p h : person list =
  let parents_of' g =
    G.fold_pred_e
      (fun e acc ->
         match G.E.label e with
         | Child -> G.E.src e :: acc
         | _ -> acc)
      g (Person p) [] in
  Entity.people (parents_of' (family_state h))

(** Find your firstborn living descendant, based on your children. You are not
    your own FLD *)
let firstborn_living_descendant p h =
  (* Recursive traversal maintains a list of children to check on a depth-first search *)
  let rec fld = function
    | p::t ->
      if is_alive p (family_state h)
      then Some p
      else ( match fld (children_of p h) with
          | Some p1 -> Some p1
          | None -> fld t)
    | [] -> None in
  fld (children_of p h)



(** *** Useful predicates after a history *** **)

(** Is p alive after the events of h have happened? *)
let is_alive_after p h = is_alive p (family_state h)

(** Is p dead after the events of h have happened? *)
let is_dead_after p h = not (is_alive_after p h)

(** You are your own issue, if you are still alive, otherwise your children *)
let rec has_issue p h = is_alive_after p h || List.exists (children_of p h) ~f:(fun x -> has_issue x h)

(** How many years have passed after after a history has happened.
    TODO: this should probably go somewhere else. *)
let years_passed h =
  fst (List.fold_right h  ~init:(0,false)
         ~f:(fun e acc -> match acc, e with
             | (n,_),    E.Start        -> (n,   true)
             | (n,true), E.YearsPass(i) -> (n+i, true)
             | _,        _              -> acc ) )
