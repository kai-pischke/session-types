open Ast

module IntMap = Map.Make(Int)

(* Type-safe representation of destinations during building *)
type destination =
  | State of int                    (* Direct state reference *)
  | Variable of int                 (* Variable reference to be resolved *)
  | End                             (* End state *)

(* Intermediate state kind during building *)
type intermediate_state_kind =
  | Snd of base * destination        (* send base + single successor *)
  | Rcv of base * destination        (* receive base + single successor *)
  | Int of (label * destination) list   (* internal choice on labels *)
  | Ext of (label * destination) list   (* external choice on labels *)

(* Final state kind after resolution *)
type state_kind =
  | Snd of base * int option         (* send base + successor (None for end) *)
  | Rcv of base * int option         (* receive base + successor (None for end) *)
  | Int of (label * int option) list (* internal choice on labels *)
  | Ext of (label * int option) list (* external choice on labels *)

(* The local automaton/graph: *)
type graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_state  : int option;           (* S_0: start state (None if empty) *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : role array;           (* id ↦ role                        *)
  kinds        : state_kind array;     (* id ↦ behaviour (Msg | Int | Ext) *)
}

(* Int-based version of the graph *)
type int_graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_state  : int option;           (* S_0: start state (None if empty) *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : int array;            (* id ↦ role (as int)               *)
  kinds        : state_kind array;     (* id ↦ behaviour (Msg | Int | Ext) *)
}

(* Helper function to convert destination to int option *)
let destination_to_int_option (dest : destination) (var_map : int IntMap.t) : int option =
  match dest with
  | State id -> Some id
  | Variable var_id ->
      IntMap.find_opt var_id var_map
  | End -> None

(* Convert intermediate state kind to final state kind *)
let resolve_state_kind (kind : intermediate_state_kind) (var_map : int IntMap.t) : state_kind =
  match kind with
  | Snd (base, dest) ->
      let resolved_dest = destination_to_int_option dest var_map in
      Snd (base, resolved_dest)
  | Rcv (base, dest) ->
      let resolved_dest = destination_to_int_option dest var_map in
      Rcv (base, resolved_dest)
  | Int branches ->
      let resolved_branches = List.map (fun (label, dest) ->
        let resolved_dest = destination_to_int_option dest var_map in
        (label, resolved_dest)
      ) branches in
      Int resolved_branches
  | Ext branches ->
      let resolved_branches = List.map (fun (label, dest) ->
        let resolved_dest = destination_to_int_option dest var_map in
        (label, resolved_dest)
      ) branches in
      Ext resolved_branches

(* Helper to create initial intermediate state kind *)
let initial_intermediate_state_kind () : intermediate_state_kind =
  Snd ("", End)

(* Helper to create empty graphs *)
let empty_graph () : graph = {
  num_states = 0;
  start_state = None;
  roles = [| |];
  kinds = [| |];
}

let empty_int_graph () : int_graph = {
  num_states = 0;
  start_state = None;
  roles = [| |];
  kinds = [| |];
}

(* Conversion functions between string and int representations *)
let graph_to_int_graph (g : graph) (role_to_int : role -> int) : int_graph = {
  num_states = g.num_states;
  start_state = g.start_state;
  roles = Array.map role_to_int g.roles;
  kinds = g.kinds;
}

let int_graph_to_graph (g : int_graph) (int_to_role : int -> role) : graph = {
  num_states = g.num_states;
  start_state = g.start_state;
  roles = Array.map int_to_role g.roles;
  kinds = g.kinds;
}

let of_local (l : int local) : graph =
  (* Fresh state ids *)
  let counter = ref 0 in
  let fresh_state () = 
    let id = !counter in 
    if id >= 1000 then 
      failwith "Too many states in local automaton (max 1000)"
    else 
      incr counter; id 
  in
  
  (* Variable mapping: var_name -> state_id *)
  let var_map = ref (IntMap.empty : int IntMap.t) in
  
  (* Pre-allocate arrays with a reasonable size *)
  let max_states = 1000 in
  let roles = Array.make max_states "" in
  let intermediate_kinds = Array.make max_states (initial_intermediate_state_kind ()) in
  
  (* Helper to add a state *)
  let add_state role kind =
    let state_id = fresh_state () in
    roles.(state_id) <- role;
    intermediate_kinds.(state_id) <- kind;
    state_id
  in
  
  (* Build the graph by traversing the local type *)
  let rec build_graph (l : int local) : destination =
    match l with
    | LEnd _ -> 
        (* LEnd doesn't correspond to a state - use End destination *)
        End
    | LVar (v, _) -> 
        (* Variable reference *)
        Variable v
    | LRec (v, body, _) ->
        (* For LRec, we need to know the state ID of the body first *)
        let body_dest = build_graph body in
        (* Store the mapping for variable resolution *)
        (match body_dest with
         | State state_id -> var_map := IntMap.add v state_id !var_map
         | _ -> ());
        body_dest
    | LInt (role, branches, _) ->
        let state_id = add_state role (Int []) in
        (* Process all branches to get their successor destinations *)
        let branch_results = List.map (fun (label, branch_l) ->
          let branch_dest = build_graph branch_l in
          (label, branch_dest)
        ) branches in
        (* Store the branching transitions *)
        intermediate_kinds.(state_id) <- Int branch_results;
        State state_id
    | LExt (role, branches, _) ->
        let state_id = add_state role (Ext []) in
        (* Process all branches to get their successor destinations *)
        let branch_results = List.map (fun (label, branch_l) ->
          let branch_dest = build_graph branch_l in
          (label, branch_dest)
        ) branches in
        (* Store the branching transitions *)
        intermediate_kinds.(state_id) <- Ext branch_results;
        State state_id
    | LRecv (role, base, cont, _) ->
        let state_id = add_state role (Rcv (base, End)) in
        let cont_dest = build_graph cont in
        (* Store the message transition *)
        intermediate_kinds.(state_id) <- Rcv (base, cont_dest);
        State state_id
    | LSend (role, base, cont, _) ->
        let state_id = add_state role (Snd (base, End)) in
        let cont_dest = build_graph cont in
        (* Store the message transition *)
        intermediate_kinds.(state_id) <- Snd (base, cont_dest);
        State state_id
  in
  
  (* First pass: build the graph structure *)
  let start_dest = build_graph l in
  
  (* Second pass: resolve variable references and convert to final format *)
  let final_kinds = Array.init !counter (fun i ->
    resolve_state_kind intermediate_kinds.(i) !var_map
  ) in
  
  (* Determine start state *)
  let start_state = match start_dest with
    | State state_id -> Some state_id
    | Variable var_id ->
        IntMap.find_opt var_id !var_map
    | End -> None
  in
  
  {
    num_states = !counter;
    start_state;
    roles = Array.sub roles 0 !counter;
    kinds = final_kinds;
  }

(* Create int-based graph directly from local type *)
let of_local_int (l : int local) (role_to_int : role -> int) : int_graph =
  let string_graph = of_local l in
  graph_to_int_graph string_graph role_to_int

(* Pretty printing for debugging and tests *)
let pp_state fmt (id : int) (roles : role array) =
  let role = roles.(id) in
  Format.fprintf fmt "state %d (%s)" id role

let pp_state_kind fmt (id : int) (kinds : state_kind array) =
  match kinds.(id) with
  | Snd (base, None) ->
      Format.fprintf fmt "from %d --snd %s--> end\n" id base
  | Snd (base, Some dst) ->
      Format.fprintf fmt "from %d --snd %s--> %d\n" id base dst
  | Rcv (base, None) ->
      Format.fprintf fmt "from %d --rcv %s--> end\n" id base
  | Rcv (base, Some dst) ->
      Format.fprintf fmt "from %d --rcv %s--> %d\n" id base dst
  | Int branches ->
      List.iter (fun (lbl, dst_opt) ->
        let dst_str = match dst_opt with
          | Some dst -> string_of_int dst
          | None -> "end"
        in
        Format.fprintf fmt "from %d --int %s--> %s\n" id lbl dst_str
      ) branches
  | Ext branches ->
      List.iter (fun (lbl, dst_opt) ->
        let dst_str = match dst_opt with
          | Some dst -> string_of_int dst
          | None -> "end"
        in
        Format.fprintf fmt "from %d --ext %s--> %s\n" id lbl dst_str
      ) branches

let pp_graph fmt (g : graph) =
  let state_ids = List.init g.num_states (fun i -> i) in
  Format.fprintf fmt "States: %s\n" 
    (String.concat ", " (List.map string_of_int state_ids));
  Format.fprintf fmt "Start state: %s\n" 
    (match g.start_state with
     | Some id -> string_of_int id
     | None -> "none");
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

(* Pretty printing for int-based graphs *)
let pp_int_state fmt (id : int) (roles : int array) =
  let role = roles.(id) in
  Format.fprintf fmt "state %d (role %d)" id role

let pp_int_graph fmt (g : int_graph) =
  let state_ids = List.init g.num_states (fun i -> i) in
  Format.fprintf fmt "States: %s\n" 
    (String.concat ", " (List.map string_of_int state_ids));
  Format.fprintf fmt "Start state: %s\n" 
    (match g.start_state with
     | Some id -> string_of_int id
     | None -> "none");
  List.iter (fun id -> 
    pp_int_state fmt id g.roles;
    Format.fprintf fmt "\n"
  ) state_ids;
  (* Print transitions in order of source state ID *)
  List.iter (fun id -> 
    pp_state_kind fmt id g.kinds
  ) state_ids

let string_of_int_graph g =
  Format.asprintf "%a" pp_int_graph g 