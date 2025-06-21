open Ast

module IntSet = Set.Make(Int)

(* Transition: from a state to a set of states, labelled with a label or base *)
type transition_label =
  | Label of label
  | Base of base

(* A transition: from a state to a set of states, with a label *)
type transition = {
  src : int;
  label : transition_label;
  dsts : int list;  (* set of destination states *)
}

(* Detailed description of a state's outgoing behaviour *)
type state_kind =
  | Msg of base option * IntSet.t                  (* optional base + successor set *)
  | Bra of (label * IntSet.t) list                 (* branching on labels *)

(* The automaton/graph: *)
type graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_states : IntSet.t;             (* S_0: set of start states        *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : (role * role) array;  (* id ↦ (p,q)                       *)
  kinds        : state_kind array;     (* id ↦ behaviour (Msg | Bra)      *)
}

(* Helper: collect all binder ids in the global type *)
let rec collect_binders (g : int global) : int list =
  match g with
  | GEnd _ -> []
  | GVar (v, _) -> [v]
  | GRec (v, body, _) -> v :: collect_binders body
  | GMsg (_, _, _, cont, _) -> collect_binders cont
  | GBra (_, _, branches, _) ->
      List.flatten (List.map (fun (_, g) -> collect_binders g) branches)
  | GPar (g1, g2, _) -> collect_binders g1 @ collect_binders g2

(* ------------------------------------------------------------------ *)
(*  Clear, environment-based translation from an *encoded* global type *)
(*  to a session-type automaton.                                       *)
(*                                                                    *)
(*  –  We allocate a fresh state for every syntactic occurrence of     *)
(*     GMsg or GBra.                                                   *)
(*  –  Recursion [rec v. g] creates a fresh binder state s and adds    *)
(*     [v ↦ {s}] to the environment while translating g into s.       *)
(*  –  A free occurrence of the variable v (GVar v) triggers a fresh   *)
(*     *copy* of the binder: we create a new state s' and translate    *)
(*     the stored body of v again into s'.  Inside that copy any       *)
(*     further occurrence of v now refers to s', hence at most one     *)
(*     duplication per unfold step just like the expected semantics.   *)
(*  –  Parallel composition returns the union of start-state sets.     *)
(*                                                                    *)
(*  The algorithm keeps three global mutable structures while walking  *)
(*  the tree once:                                                     *)
(*      • [counter]      – next fresh state id                         *)
(*      • [role_tbl]     – id ↦ (p,q)                                  *)
(*      • [tr_list]      – list of transitions to build                *)
(*  plus                                                               *)
(*      • [defs]         – var id ↦ body (for later unfolding)         *)
(*  Everything else is purely functional (the environment map).        *)
(* ------------------------------------------------------------------ *)

let of_global (g : int global) : graph =
  (* Fresh state ids *)
  let counter = ref 0 in
  let fresh_state () = let id = !counter in incr counter; id in

  (* Mutable stores we fill on the fly *)
  let role_tbl : (int, (role*role)) Hashtbl.t = Hashtbl.create 17 in
  type raw_transition = { src:int; label:transition_label; dsts:IntSet.t }  
  in
  let tr_list : raw_transition list ref = ref [] in

  (* Variable definitions (rec-bodies) *)
  let defs : (int, int global) Hashtbl.t = Hashtbl.create 17 in

  module IM = Map.Make(Int) in
  type env = IntSet.t IM.t

  (* Forward declaration *)
  let rec build       : int global -> env -> IntSet.t
  and      build_into : int -> int global -> env -> unit
  = fun g env ->
    match g with
    | GEnd _ -> IntSet.empty

    | GPar (g1,g2,_) ->
        IntSet.union (build g1 env) (build g2 env)

    | GMsg _ | GBra _ ->
        let s = fresh_state () in
        build_into s g env; IntSet.singleton s

    | GRec (v, body, _) ->
        (* Remember the body for later unfolding *)
        Hashtbl.replace defs v body;
        let s = fresh_state () in
        let env' = IM.add v (IntSet.singleton s) env in
        build_into s body env';
        IntSet.singleton s

    | GVar (v, _) ->
        (* Is v already mapped in the current environment? *)
        (match IM.find_opt v env with
         | Some set -> set                               (* refer to existing binder *)
         | None ->                                       (* first free occurrence – unfold once *)
             (match Hashtbl.find_opt defs v with
              | None -> IntSet.empty                     (* ill-formed, but keep going *)
              | Some body ->
                  let s = fresh_state () in
                  let env' = IM.add v (IntSet.singleton s) env in
                  build_into s body env';
                  IntSet.singleton s))
  
  and build_into s g env =
    match g with
    | GMsg (p,q,b,cont,_) ->
        Hashtbl.replace role_tbl s (p,q);
        let dsts = build cont env in
        tr_list := { src=s; label=Base b; dsts } :: !tr_list

    | GBra (p,q,branches,_) ->
        Hashtbl.replace role_tbl s (p,q);
        List.iter (fun (lbl, g') ->
          let dsts = build g' env in
          tr_list := { src=s; label=Label lbl; dsts } :: !tr_list
        ) branches

    | g' ->
        (* We should only ever be handed Msg or Bra as the body of a binder *)
        let _ = build g' env in       (* fall back to normal build *)
        ()
  in

  (* Kick off translation – start states are what [build] returns. *)
  let start_states = build g IM.empty in

  (* If no states were allocated we return the empty graph. *)
  if !counter = 0 then
    { num_states=0; start_states=IntSet.empty; roles=[||]; kinds=[||] }
  else
    (* Collect all state ids that appear anywhere. *)
    let all_ids =
      let from_trans =
        List.fold_left (fun acc tr ->
          IntSet.union acc (IntSet.add tr.src tr.dsts)
        ) IntSet.empty !tr_list
      in
      IntSet.elements (IntSet.union from_trans start_states)
    in
    let num = List.length all_ids in

    (* Deterministic re-ordering by (p,q) then id. *)
    let sort_key id =
      match Hashtbl.find_opt role_tbl id with
      | Some rp -> (rp, id)
      | None    -> (("_","_"), id)
    in
    let sorted = List.sort (fun a b -> compare (sort_key a) (sort_key b)) all_ids in

    (* Map old ids to new dense indices 0..n-1 *)
    let id_to_idx = Hashtbl.create num in
    List.iteri (fun i id -> Hashtbl.add id_to_idx id i) sorted;

    (* Fill role array *)
    let roles = Array.make num ("_","_") in
    List.iter (fun id ->
      match Hashtbl.find_opt role_tbl id with
      | Some rp -> roles.(Hashtbl.find id_to_idx id) <- rp
      | None    -> ()
    ) sorted;

    (* Group transitions by (translated) src *)
    let grouped = Hashtbl.create num in
    List.iter (fun tr ->
      let src' = Hashtbl.find id_to_idx tr.src in
      let dsts' = IntSet.fold (fun d acc -> IntSet.add (Hashtbl.find id_to_idx d) acc) tr.dsts IntSet.empty in
      let tr' = { src=src'; label=tr.label; dsts=IntSet.elements dsts' } in
      let existing = match Hashtbl.find_opt grouped src' with Some l -> l | None -> [] in
      Hashtbl.replace grouped src' (tr' :: existing)
    ) !tr_list;

    (* Build kind array *)
    let kinds = Array.make num (Msg (None, IntSet.empty)) in
    Array.iteri (fun idx _ ->
      match Hashtbl.find_opt grouped idx with
      | None -> ()
      | Some trs ->
          let lbl_trs = List.filter (function {label=Label _; _} -> true | _ -> false) trs in
          if lbl_trs <> [] then
            let pairs = List.map (function {label=Label l; dsts; _} -> (l, IntSet.of_list dsts) | _ -> assert false) lbl_trs in
            kinds.(idx) <- Bra pairs
          else
            match trs with
            | [{label=Base b; dsts; _}] -> kinds.(idx) <- Msg (Some b, IntSet.of_list dsts)
            | [{label=Base _; _}; _] | _::_ ->
                let dsts = List.flatten (List.map (fun {dsts; _}-> dsts) trs) |> List.sort_uniq compare |> IntSet.of_list in
                kinds.(idx) <- Msg (None, dsts)
            | _ -> ()
    ) roles;

    (* Translate start-state ids *)
    let start_states' = IntSet.map (fun id -> Hashtbl.find id_to_idx id) start_states in

    { num_states=num; start_states=start_states'; roles; kinds }

(* Pretty printing for debugging and tests *)
let pp_transition_label fmt = function
  | Label l -> Format.fprintf fmt "label %s" l
  | Base b  -> Format.fprintf fmt "base %s" b

let pp_state fmt (id : int) (roles : (role * role) array) =
  let (p, q) = roles.(id) in
  Format.fprintf fmt "state %d (%s,%s)" id p q

let pp_state_kind fmt (id : int) (kinds : state_kind array) =
  match kinds.(id) with
  | Msg (None, dsts) when IntSet.is_empty dsts -> () (* no transitions *)
  | Msg (Some b, dsts) ->
      Format.fprintf fmt "from %d --base %s--> %s\n" id b
        (String.concat "," (List.map string_of_int (IntSet.elements dsts)))
  | Msg (None, dsts) ->
      Format.fprintf fmt "from %d --> %s\n" id
        (String.concat "," (List.map string_of_int (IntSet.elements dsts)))
  | Bra branches ->
      List.iter (fun (lbl, dsts) ->
        Format.fprintf fmt "from %d --label %s--> %s\n" id lbl
          (String.concat "," (List.map string_of_int (IntSet.elements dsts)))
      ) branches

let pp_graph fmt (g : graph) =
  let state_ids = List.init g.num_states (fun i -> i) in
  Format.fprintf fmt "States: %s\n" 
    (String.concat ", " (List.map string_of_int state_ids));
  Format.fprintf fmt "Start states: %s\n" 
    (String.concat ", " (List.map string_of_int (IntSet.elements g.start_states)));
  List.iter (fun id -> 
    pp_state fmt id g.roles;
    Format.fprintf fmt "\n"
  ) state_ids;
  (* Print transitions in order of source state ID *)
  List.iter (fun id -> 
    pp_state_kind fmt id g.kinds
  ) state_ids

let string_of_graph g =
  Format.asprintf "%a" pp_graph g
