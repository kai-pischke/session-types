open Ast

(* State: identified by an integer, labelled with (p, q) *)
type state = {
  id : int;
  label : role * role;
}

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

(* The automaton/graph: *)
type graph = {
  num_states : int;         (* n: states are 0..n-1 *)
  start_states : int list;  (* S_0: set of start states *)
  states : state list;      (* all states, indexed by id *)
  transitions : transition list;
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

(* Main translation function *)
let of_global (g : int global) : graph =
  let state_tbl = Hashtbl.create 8 in
  let transitions = ref [] in

  (* Compute start states: for a top-level GPar, use all branch states; for GRec, use the binder *)
  let rec get_start_states g =
    match g with
    | GPar (g1, g2, _) -> get_start_states g1 @ get_start_states g2
    | GRec (v, body, _) ->
        (match body with
         | GPar _ -> get_start_states body
         | _ -> [v])
    | GMsg _ | GBra _ | GEnd _ | GVar _ -> []
  in

  (* Helper: recursively look through nested recs to find start states of a parallel *)
  let rec get_parallel_start_states g =
    match g with
    | GRec (_, body, _) ->
        (match body with
         | GPar _ -> get_parallel_start_states body
         | _ -> [])
    | GPar _ -> get_start_states g
    | _ -> []
  in

  (* Recursively build the automaton. Returns the set of possible next states *)
  let rec build (g : int global) (current : int) : int list =
    match g with
    | GEnd _ -> []
    | GVar (v, _) -> [v]
    | GRec (v, body, _) ->
        build body v
    | GMsg (p, q, b, cont, _) ->
        if not (Hashtbl.mem state_tbl current) then
          Hashtbl.add state_tbl current { id = current; label = (p, q) };
        let built = build cont current in  (* ensure all states/transitions are created *)
        let dsts =
          match cont with
          | GPar _ -> get_start_states cont
          | GRec (_, body, _) -> (match body with | GPar _ -> get_parallel_start_states cont | _ -> built)
          | GVar (v, _) -> [v]
          | _ -> built
        in
        let tr = { src = current; label = Base b; dsts } in
        transitions := tr :: !transitions;
        [current]
    | GBra (p, q, branches, _) ->
        if not (Hashtbl.mem state_tbl current) then
          Hashtbl.add state_tbl current { id = current; label = (p, q) };
        let all_dsts =
          List.map (fun (lbl, g') ->
            let built = build g' current in  (* ensure all states/transitions are created *)
            let dsts =
              match g' with
              | GPar _ -> get_start_states g'
              | GRec (_, body, _) -> (match body with | GPar _ -> get_parallel_start_states g' | _ -> built)
              | GVar (v, _) -> [v]
              | _ -> built
            in
            let tr = { src = current; label = Label lbl; dsts } in
            transitions := tr :: !transitions;
            dsts
          ) branches
        in
        List.flatten all_dsts
    | GPar (g1, g2, _) ->
        let s1 = build g1 current in
        let s2 = build g2 current in
        s1 @ s2
  in

  let _ =
    match g with
    | GRec (v, body, _) -> ignore (build body v)
    | _ -> ignore (build g 0)
  in

  let all_dsts =
    List.fold_left (fun acc tr -> List.rev_append tr.dsts acc) [] !transitions
    |> List.sort_uniq compare
  in
  List.iter (fun id ->
    if not (Hashtbl.mem state_tbl id) then
      Hashtbl.add state_tbl id { id; label = ("_", "_") }
  ) all_dsts;
  let used_ids =
    let srcs = List.map (fun tr -> tr.src) !transitions in
    let ids = srcs @ all_dsts @ (get_start_states g) in
    List.sort_uniq compare ids
  in
  let states =
    Hashtbl.fold (fun id s acc -> if List.mem id used_ids then s :: acc else acc) state_tbl []
  in
  (* Sort states deterministically by (role, role) label, then by original id *)
  let states_sorted =
    List.sort (fun (a : state) (b : state) ->
      match compare a.label b.label with
      | 0 -> compare a.id b.id
      | c -> c
    ) states
  in
  (* Assign new state numbers 0..n-1 in this order *)
  let id_map =
    List.mapi (fun new_id s -> (s.id, new_id)) states_sorted
    |> List.to_seq |> Hashtbl.of_seq
  in
  let remap_id id = Hashtbl.find id_map id in
  let remap_state s = { id = remap_id s.id; label = s.label } in
  let remap_transition tr =
    { src = remap_id tr.src;
      label = tr.label;
      dsts = List.map remap_id tr.dsts }
  in
  let remap_start_states = List.map remap_id (List.sort_uniq compare (get_start_states g)) in
  {
    num_states = List.length states_sorted;
    start_states = remap_start_states;
    states = List.map remap_state states_sorted;
    transitions = List.rev_map remap_transition !transitions;
  }

(* Pretty printing for debugging and tests *)
let pp_transition_label fmt = function
  | Label l -> Format.fprintf fmt "label %s" l
  | Base b  -> Format.fprintf fmt "base %s" b

let pp_state fmt (s : state) =
  Format.fprintf fmt "state %d (%s,%s)" s.id (fst s.label) (snd s.label)

let pp_transition fmt (tr : transition) =
  Format.fprintf fmt "from %d --%a--> %s"
    tr.src pp_transition_label tr.label
    (String.concat "," (List.map string_of_int tr.dsts))

let pp_graph fmt (g : graph) =
  Format.fprintf fmt "States: %s\n" (String.concat ", " (List.map (fun s -> string_of_int s.id) g.states));
  Format.fprintf fmt "Start states: %s\n" (String.concat ", " (List.map string_of_int g.start_states));
  List.iter (fun s -> Format.fprintf fmt "%a\n" pp_state s) g.states;
  List.iter (fun tr -> Format.fprintf fmt "%a\n" pp_transition tr) g.transitions

let string_of_graph g =
  Format.asprintf "%a" pp_graph g
