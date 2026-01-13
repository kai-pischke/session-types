(*--------------------------------------------------------------------*)
(*  Algorithm 1: Global Type Synthesis                                 *)
(*--------------------------------------------------------------------*)
(*
  This module implements Algorithm 1 from the paper for synthesizing
  global session types from safe and live local type contexts.
  
  ALGORITHM:
  Require: Δ is safe and live
  Ensure: G such that Δ ⊑ G
  
  The algorithm constructs a global type from a context similar to 
  the relation Σ ⊢ Δ ⇝ G. It uses:
  - Table T to memoize visited contexts
  - Queue Q to enqueue pending transitions  
  - Guard flag g to check if context is guarded
  
  IMPLEMENTATION DETAILS:
  - Deciding whether a context is parallel can be done in polynomial 
    time by keeping a participant interaction graph
  - Finding if a mu-binder is necessary can be done in linear time by
    identifying SCCs and marking vertices with incoming paths from start
*)

open Ast
open Local_automaton

(*--------------------------------------------------------------------*)
(*  Type Definitions                                                  *)
(*--------------------------------------------------------------------*)

(** CFSM: Communicating Finite State Machine (array of automata) *)
type cfsm = Local_automaton.int_graph array

(** Configuration: state of all participants *)
type configuration = int option array

(** Participant pair for communication *)
type participant_pair = int * int

(** Memoization table mapping configurations to recursion variables *)
module ConfigMap = Map.Make(struct
  type t = configuration
  let compare = compare
end)

(** Fresh variable counter *)
let fresh_var = ref 0

let reset_fresh_var () = fresh_var := 0

let new_fresh_var () =
  let v = !fresh_var in
  incr fresh_var;
  v

(** Call counter for measuring complexity *)
let synth_call_count = ref 0

let reset_call_count () = synth_call_count := 0

let get_call_count () = !synth_call_count

(*--------------------------------------------------------------------*)
(*  Configuration Helpers                                             *)
(*--------------------------------------------------------------------*)

(**
  initial_configuration: cfsm -> configuration
  
  Pre: cfsm is a well-formed communicating finite state machine
  Post: Returns initial configuration with all participants at start states
*)
let initial_configuration (cfsm : cfsm) : configuration =
  Array.map (fun automaton -> automaton.start_state) cfsm

(**
  is_terminated: configuration -> bool
  
  Pre: config is a valid configuration
  Post: Returns true if all participants have terminated (all None)
*)
let is_terminated (config : configuration) : bool =
  Array.for_all (fun state_opt -> state_opt = None) config

(**
  config_to_string: configuration -> string
  
  Pre: config is a valid configuration
  Post: Returns string representation for debugging
*)
let config_to_string (config : configuration) : string =
  let states = Array.to_list config in
  let state_strs = List.map (function
    | None -> "⊥"
    | Some s -> string_of_int s
  ) states in
  "[" ^ String.concat ", " state_strs ^ "]"

(*--------------------------------------------------------------------*)
(*  Transition Detection                                              *)
(*--------------------------------------------------------------------*)

(**
  can_communicate: cfsm -> configuration -> int -> int -> (string * int option * int option) option
  
  Pre: 0 <= sender, receiver < Array.length cfsm
       config is well-formed
  Post: Returns Some (base, next_sender_state, next_receiver_state) if 
        sender and receiver can communicate, None otherwise
*)
let can_communicate (cfsm : cfsm) (config : configuration) (sender : int) (receiver : int) 
    : (string * int option * int option) option =
  match config.(sender), config.(receiver) with
  | Some sender_state, Some receiver_state ->
      let sender_aut = cfsm.(sender) in
      let receiver_aut = cfsm.(receiver) in
      
      (* Check if sender sends to receiver *)
      if sender_aut.roles.(sender_state) = receiver then
        begin match sender_aut.kinds.(sender_state), receiver_aut.kinds.(receiver_state) with
        | Snd (base1, next_sender), Rcv (base2, next_receiver) 
          when base1 = base2 && receiver_aut.roles.(receiver_state) = sender ->
            Some (base1, next_sender, next_receiver)
        | _ -> None
        end
      else None
  | _ -> None

(**
  can_branch: cfsm -> configuration -> int -> int -> (string * int option * int option) list option
  
  Pre: 0 <= sender, receiver < Array.length cfsm
       config is well-formed
  Post: Returns Some branches if sender and receiver can branch, None otherwise
*)
let can_branch (cfsm : cfsm) (config : configuration) (sender : int) (receiver : int) 
    : (string * int option * int option) list option =
  match config.(sender), config.(receiver) with
  | Some sender_state, Some receiver_state ->
      let sender_aut = cfsm.(sender) in
      let receiver_aut = cfsm.(receiver) in
      
      (* Check if sender has internal choice to receiver *)
      if sender_aut.roles.(sender_state) = receiver then
        begin match sender_aut.kinds.(sender_state), receiver_aut.kinds.(receiver_state) with
        | Int sender_branches, Ext receiver_branches 
          when receiver_aut.roles.(receiver_state) = sender ->
            (* Match labels *)
            let matched_branches = List.filter_map (fun (label, sender_next) ->
              match List.assoc_opt label receiver_branches with
              | Some receiver_next -> Some (label, sender_next, receiver_next)
              | None -> None
            ) sender_branches in
            if List.length matched_branches > 0 then Some matched_branches else None
        | _ -> None
        end
      else None
  | _ -> None

(**
  find_available_transitions: cfsm -> configuration -> participant_pair list
  
  Pre: cfsm is well-formed, config is valid
  Post: Returns list of (sender, receiver) pairs that can communicate
*)
let find_available_transitions (cfsm : cfsm) (config : configuration) : participant_pair list =
  let n = Array.length cfsm in
  let transitions = ref [] in
  for sender = 0 to n - 1 do
    for receiver = 0 to n - 1 do
      if sender <> receiver then
        match can_communicate cfsm config sender receiver with
        | Some _ -> transitions := (sender, receiver) :: !transitions
        | None ->
            match can_branch cfsm config sender receiver with
            | Some _ -> transitions := (sender, receiver) :: !transitions
            | None -> ()
    done
  done;
  !transitions

(**
  apply_transition: configuration -> int -> int option -> int -> int option -> configuration
  
  Pre: config is valid, 0 <= sender, receiver < Array.length config
  Post: Returns new configuration after transition
*)
let apply_transition (config : configuration) (sender : int) (sender_next : int option) 
                     (receiver : int) (receiver_next : int option) : configuration =
  let new_config = Array.copy config in
  new_config.(sender) <- sender_next;
  new_config.(receiver) <- receiver_next;
  new_config

(*--------------------------------------------------------------------*)
(*  Parallel Decomposition                                            *)
(*--------------------------------------------------------------------*)

(**
  get_participants_in_automaton: int_graph -> int option -> int list
  
  Pre: aut is a valid automaton, state_opt is current state
  Post: Returns list of participant indices that appear in transitions from current state
        (i.e., the roles that this participant can interact with)
*)
let get_participants_in_automaton (aut : int_graph) (state_opt : int option) : int list =
  match state_opt with
  | None -> []
  | Some state ->
      if state < 0 || state >= aut.num_states then []
      else
        (* The role at this state indicates which participant we interact with *)
        let role_at_state = aut.roles.(state) in
        if role_at_state >= 0 then [role_at_state] else []

(**
  build_participant_interaction_graph: cfsm -> configuration -> int list array
  
  Pre: cfsm is well-formed, config is valid
  Post: Returns adjacency list for participant interaction graph (PIG)
        There is an edge from p to q iff p ∈ participants(Δ(q))
*)
let build_participant_interaction_graph (cfsm : cfsm) (config : configuration) : int list array =
  let n = Array.length cfsm in
  let adj = Array.make n [] in
  
  (* For each participant q, find all participants p that appear in Δ(q) *)
  for q = 0 to n - 1 do
    match config.(q) with
    | None -> ()  (* Skip terminated participants *)
    | Some _ ->
        let participants_in_q = get_participants_in_automaton cfsm.(q) config.(q) in
        (* Add undirected edges between q and each participant p in Δ(q) *)
        List.iter (fun p ->
          if p <> q && p >= 0 && p < n then begin
            if not (List.mem p adj.(q)) then
              adj.(q) <- p :: adj.(q);
            if not (List.mem q adj.(p)) then
              adj.(p) <- q :: adj.(p)
          end
        ) participants_in_q
  done;
  adj

(**
  find_connected_components: configuration -> int list array -> int list list
  
  Pre: config is valid, adj is adjacency list
  Post: Returns list of connected components (each component is a list of participant indices)
*)
let find_connected_components (config : configuration) (adj : int list array) : int list list =
  let n = Array.length config in
  let visited = Array.make n false in
  let components = ref [] in
  
  let rec dfs i component =
    if not visited.(i) then begin
      visited.(i) <- true;
      match config.(i) with
      | None -> component  (* Skip terminated participants *)
      | Some _ ->
          let new_component = i :: component in
          List.fold_left (fun acc neighbor -> dfs neighbor acc) new_component adj.(i)
    end else
      component
  in
  
  for i = 0 to n - 1 do
    match config.(i) with
    | Some _ when not visited.(i) ->
        let component = dfs i [] in
        if component <> [] then
          components := component :: !components
    | _ -> ()
  done;
  
  !components

(**
  is_parallel: cfsm -> configuration -> bool
  
  Pre: cfsm is well-formed, config is valid
  Post: Returns true if config represents parallel composition
  
  A configuration is parallel if the participant interaction graph (PIG)
  has more than one connected component.
*)
let is_parallel (cfsm : cfsm) (config : configuration) : bool =
  let adj = build_participant_interaction_graph cfsm config in
  let components = find_connected_components config adj in
  List.length components > 1

(**
  split_parallel: cfsm -> role array -> configuration -> (cfsm * role array * configuration * (int -> int option)) list
  
  Pre: cfsm is well-formed, config represents parallel composition
  Post: Returns list of (sub_cfsm, sub_roles, sub_config, index_map) for each parallel component
       where index_map: old_index -> Some(new_index) for participants in this component, None otherwise
*)
let split_parallel (cfsm : cfsm) (roles : role array) (config : configuration) 
    : (cfsm * role array * configuration * (int -> int option)) list =
  let adj = build_participant_interaction_graph cfsm config in
  let components = find_connected_components config adj in
  
  List.map (fun component ->
    let n = List.length component in
    let participant_map = Array.make (Array.length cfsm) (-1) in
    List.iteri (fun new_idx old_idx ->
      participant_map.(old_idx) <- new_idx
    ) component;
    
    (* Create index mapping function *)
    let index_map old_idx =
      if old_idx >= 0 && old_idx < Array.length participant_map && participant_map.(old_idx) >= 0 then
        Some participant_map.(old_idx)
      else
        None
    in
    
    (* Create sub-CFSM with only this component's participants *)
    let sub_cfsm = Array.init n (fun i ->
      let old_idx = List.nth component i in
      let old_aut = cfsm.(old_idx) in
      
      (* Remap roles in the automaton *)
      let new_roles = Array.map (fun old_role ->
        if old_role >= 0 && old_role < Array.length participant_map then
          let new_role = participant_map.(old_role) in
          if new_role >= 0 then new_role else 0  (* Default to 0 if not in component *)
        else 0
      ) old_aut.roles in
      
      { old_aut with roles = new_roles }
    ) in
    
    (* Create sub-roles array *)
    let sub_roles = Array.init n (fun i ->
      let old_idx = List.nth component i in
      roles.(old_idx)
    ) in
    
    (* Create sub-configuration *)
    let sub_config = Array.init n (fun i ->
      let old_idx = List.nth component i in
      config.(old_idx)
    ) in
    
    (sub_cfsm, sub_roles, sub_config, index_map)
  ) components

(*--------------------------------------------------------------------*)
(*  Variable occurrence checking                                       *)
(*--------------------------------------------------------------------*)

(**
  var_occurs_in_global: int -> int global -> bool
  
  Pre: v is a variable, g is a global type
  Post: Returns true if variable v occurs free in g
*)
let rec var_occurs_in_global (v : int) (g : int global) : bool =
  match g with
  | GEnd _ -> false
  | GVar (w, _) -> v = w
  | GRec (w, body, _) ->
      if v = w then false  (* Variable is bound here *)
      else var_occurs_in_global v body
  | GMsg (_, _, _, cont, _) -> var_occurs_in_global v cont
  | GBra (_, _, branches, _) ->
      List.exists (fun (_, g') -> var_occurs_in_global v g') branches
  | GPar (g1, g2, _) ->
      var_occurs_in_global v g1 || var_occurs_in_global v g2

(*--------------------------------------------------------------------*)
(*  Main Synthesis Algorithm                                          *)
(*--------------------------------------------------------------------*)

(**
  synth': cfsm -> role array -> configuration -> int ConfigMap.t -> bool -> int global * int ConfigMap.t
  
  Pre: cfsm is well-formed and safe/live
       roles maps participant indices to role names
       config is current configuration
       memo is memoization table
       guarded is guard flag
  Post: Returns (synthesized_global_type, updated_memo)
*)
let rec synth' (cfsm : cfsm) (roles : role array) (config : configuration) 
               (memo : int ConfigMap.t) (guarded : bool) 
    : int global * int ConfigMap.t =
  
  incr synth_call_count;
  
  (* Case 1: g ∧ Δ ∈ T then return T(Δ) *)
  if guarded && ConfigMap.mem config memo then
    let var = ConfigMap.find config memo in
    (GVar (var, Loc.dummy), memo)
  
  (* Case 2: Δ = Δ₁ || Δ₂ - parallel composition *)
  else if is_parallel cfsm config then begin
    let components = split_parallel cfsm roles config in
    
    (* Synthesize each component recursively *)
    let (parallel_types, final_memo) = List.fold_left 
      (fun (acc_types, acc_memo) (sub_cfsm, sub_roles, sub_config, _index_map) ->
        let (component_type, updated_memo) = 
          synth' sub_cfsm sub_roles sub_config acc_memo guarded in
        (component_type :: acc_types, updated_memo)
      ) ([], memo) components in
    
    (* Build parallel composition from components *)
    match parallel_types with
    | [] -> (GEnd Loc.dummy, memo)
    | [single] -> (single, final_memo)
    | first :: rest ->
        let combined = List.fold_left 
          (fun acc typ -> GPar (acc, typ, Loc.dummy)) 
          first rest in
        (combined, final_memo)
  end
  
  (* Case 3: Δ ∉ T - potential recursion point (guard not checked here!) *)
  else if not (ConfigMap.mem config memo) then
    (* Add config to memo with fresh variable and explore from same config *)
    let fresh_t = new_fresh_var () in
    let new_memo = ConfigMap.add config fresh_t memo in
    (* Recursively call on same config - guard prevents Case 1 from firing *)
    let (body, final_memo) = synth' cfsm roles config new_memo false in
    (* Check if variable is used (indicates recursion) *)
    if var_occurs_in_global fresh_t body then
      (GRec (fresh_t, body, Loc.dummy), final_memo)
    else
      (body, memo)  (* No recursion needed *)
  
  (* Case 4: Δ → - terminated *)
  else if is_terminated config then
    (GEnd Loc.dummy, memo)
  
  (* Case 5: No transitions available but not terminated - deadlock *)
  else if find_available_transitions cfsm config = [] then
    (GEnd Loc.dummy, memo)
  
  else
    (* Case 6: Has transitions, process them *)
    let available = find_available_transitions cfsm config in
    match available with
    | [] -> (GEnd Loc.dummy, memo)
    | (sender, receiver) :: _ ->
        let sender_role = roles.(sender) in
        let receiver_role = roles.(receiver) in
        
        (* Try message communication first *)
        begin match can_communicate cfsm config sender receiver with
        | Some (base, sender_next, receiver_next) ->
            let new_config = apply_transition config sender sender_next receiver receiver_next in
            let (cont, new_memo) = synth' cfsm roles new_config memo true in
            (GMsg (sender_role, receiver_role, base, cont, Loc.dummy), new_memo)
        
        | None ->
            (* Try branching - synthesize all branches recursively and independently *)
            match can_branch cfsm config sender receiver with
            | Some branches ->
                let (branch_types, final_memo) = List.fold_left (fun (acc_branches, acc_memo) (label, sender_next, receiver_next) ->
                  let new_config = apply_transition config sender sender_next receiver receiver_next in
                  (* Recursively synthesize each branch with guarded=true *)
                  (* Guard prevents premature variable returns in Case 1 *)
                  let (branch_type, updated_memo) = synth' cfsm roles new_config acc_memo true in
                  ((label, branch_type) :: acc_branches, updated_memo)
                ) ([], memo) branches in
                (GBra (sender_role, receiver_role, List.rev branch_types, Loc.dummy), final_memo)
            
            | None -> (GEnd Loc.dummy, memo)
        end

(**
  synth: cfsm -> role array -> int global
  
  Pre: cfsm represents a safe and live context Δ
       roles maps participant indices to role names
  Post: Returns synthesized global type G such that Δ ⊑ G
*)
let synth (cfsm : cfsm) (roles : role array) : int global =
  reset_fresh_var ();
  reset_call_count ();
  let initial_config = initial_configuration cfsm in
  let empty_memo = ConfigMap.empty in
  let (result, _) = synth' cfsm roles initial_config empty_memo false in
  result
