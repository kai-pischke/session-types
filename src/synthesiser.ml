(*--------------------------------------------------------------------*)
(*  Global Session Type Synthesis from Local Automata                  *)
(*--------------------------------------------------------------------*)
(*
  This module implements synthesis of global session types from collections
  of local automata (CFSM - Communicating Finite State Machines).
  
  KEY ALGORITHM:
  The synthesis follows a systematic exploration of the global state space:
  
  1. REMEMBER BINDING SITES: To create recursive types, we need to remember the binding sites of the recursive types.
  2. PARALLEL DECOMPOSITION: Use split() to detect independent components
  3. TERMINATION: Handle configurations with no available transitions
  4. COMMUNICATION SYNTHESIS: Generate message passing and choice constructs
  
  GLOBAL TYPE CONSTRUCTS GENERATED:
  - GEnd: Termination when no transitions possible
  - GMsg: Message passing between two participants  
  - GBra: Choice/branching with multiple labeled alternatives
  - GPar: Parallel composition of independent components (n-ary via left-associative tree)
  - GRec/GVar: Recursive types with variable references
  
  COMPLEXITY:
  - Time: Exponential in worst case due to state space exploration
  - Space: Polynomial with memoization for cycle detection
  
  CORRECTNESS PROPERTIES:
  - Generated global types are well-formed
  - Local projections match original automata behavior
  - Parallel components are properly isolated
*)

open Ast
open Local_automaton
open Connected

(* Debug helper to format configuration *)
let format_config (config : configuration) : string =
  let config_strs = Array.mapi (fun i state_opt ->
    match state_opt with
    | Some s -> Printf.sprintf "%d:%d" i s
    | None -> Printf.sprintf "%d:end" i
  ) config in
  "[" ^ String.concat "; " (Array.to_list config_strs) ^ "]"

(* Debug helper to format participant roles *)
let format_roles (participant_roles : role array) : string =
  "[" ^ String.concat "; " (Array.to_list participant_roles) ^ "]"

(* Fresh variable generation for recursion *)
let fresh_var_counter = ref 0

let fresh_var () =
  let v = !fresh_var_counter in
  incr fresh_var_counter;
  v

(* Configuration comparison utility *)
let compare_configs config1 config2 =
  let len1 = Array.length config1 in
  let len2 = Array.length config2 in
  if len1 <> len2 then len1 - len2
  else begin
    let rec compare_elements i =
      if i >= len1 then 0
      else
        let cmp = Option.compare Int.compare config1.(i) config2.(i) in
        if cmp <> 0 then cmp else compare_elements (i + 1)
    in
    compare_elements 0
  end

(* Configuration utilities *)
module ConfigMap = Map.Make(struct
  type t = configuration
  let compare = compare_configs
end)

(* Communication pair utilities *)
type pair_queue = (int * int) list

(* Queue operations for fair scheduling *)
let queue_is_empty (queue : pair_queue) : bool = 
  queue = []

let queue_enqueue (queue : pair_queue) (pairs : (int * int) list) : pair_queue =
  (* Only add pairs not already in queue *)
  let new_pairs = List.filter (fun pair -> not (List.mem pair queue)) pairs in
  queue @ new_pairs  (* append to end *)

let queue_dequeue (queue : pair_queue) : (int * int) * pair_queue =
  match queue with
  | [] -> failwith "Internal error: queue_dequeue called on empty queue (violates invariant)"
  | head :: tail -> (head, tail)

let queue_remove (queue : pair_queue) (pair : int * int) : pair_queue =
  List.filter (fun p -> p <> pair) queue

let queue_size (queue : pair_queue) : int = 
  List.length queue

(* Extract initial configuration from cfsm *)
let initial_configuration (cfsm : cfsm) : configuration =
  Array.map (fun automaton -> automaton.start_state) cfsm

(* Check if configuration has no possible transitions *)
let is_terminal_config (cfsm : cfsm) (config : configuration) : bool =
  Array.for_all2 (fun _ state_opt ->
    match state_opt with
    | None -> true (* terminated *)
    | Some _ -> false
  ) cfsm config

(* Find all participant pairs that can potentially communicate *)
let find_communication_pairs (cfsm : cfsm) (config : configuration) : (int * int) list =
  let n = Array.length cfsm in
  let pairs = ref [] in
  
  for p = 0 to n - 1 do
    for q = 0 to n - 1 do
      if p <> q then
        match config.(p), config.(q) with
        | Some p_state, Some q_state ->
            if p_state < cfsm.(p).num_states && q_state < cfsm.(q).num_states then
              (* Check if p can send to q and q can receive from p *)
              (match cfsm.(p).kinds.(p_state), cfsm.(q).kinds.(q_state) with
               | Snd (_, _), Rcv (_, _) when cfsm.(p).roles.(p_state) = q && cfsm.(q).roles.(q_state) = p ->
                   pairs := (p, q) :: !pairs
               | Int _, Ext _ when cfsm.(p).roles.(p_state) = q && cfsm.(q).roles.(q_state) = p ->
                   pairs := (p, q) :: !pairs
               | _ -> ())
        | _ -> ()
    done
  done;
  !pairs

(* Execute a message communication transition *)
let execute_message_transition (cfsm : cfsm) (config : configuration) (p : int) (q : int) : (base * configuration) option =
  match config.(p), config.(q) with
  | Some p_state, Some q_state ->
      if p_state < cfsm.(p).num_states && q_state < cfsm.(q).num_states then
        (match cfsm.(p).kinds.(p_state), cfsm.(q).kinds.(q_state) with
         | Snd (base, p_next), Rcv (base', q_next) 
           when cfsm.(p).roles.(p_state) = q && cfsm.(q).roles.(q_state) = p && base = base' ->
             let new_config = Array.copy config in
             new_config.(p) <- p_next;
             new_config.(q) <- q_next;
             Some (base, new_config)
         | _ -> None)
      else None
  | _ -> None

(* Execute a choice communication transition *)
let execute_choice_transition (cfsm : cfsm) (config : configuration) (p : int) (q : int) : (label * configuration) list =
  match config.(p), config.(q) with
  | Some p_state, Some q_state ->
      if p_state < cfsm.(p).num_states && q_state < cfsm.(q).num_states then
        (match cfsm.(p).kinds.(p_state), cfsm.(q).kinds.(q_state) with
         | Int p_branches, Ext q_branches 
           when cfsm.(p).roles.(p_state) = q && cfsm.(q).roles.(q_state) = p ->
             (* Find matching labels *)
             List.fold_left (fun acc (p_label, p_next) ->
               match List.assoc_opt p_label q_branches with
               | Some q_next ->
                   let new_config = Array.copy config in
                   new_config.(p) <- p_next;
                   new_config.(q) <- q_next;
                   (p_label, new_config) :: acc
               | None -> acc
             ) [] p_branches |> List.rev
         | _ -> [])
      else []
  | _ -> []

(* Check if config can eventually reach target without visiting forbidden configs *)
let can_reach_config (cfsm : cfsm) (start_config : configuration) (target_config : configuration) (forbidden : ConfigMap.key list) : bool =
  Printf.printf "    DFS: Starting reachability check\n%!";
  Printf.printf "    DFS: Start = %s, Target = %s\n%!" (format_config start_config) (format_config target_config);
  let visited = ref (ConfigMap.empty) in
  let forbidden_set = List.fold_left (fun set config -> ConfigMap.add config () set) ConfigMap.empty forbidden in
  
  let rec dfs config depth =
    Printf.printf "    DFS: Visiting %s (depth %d)\n%!" (format_config config) depth;
    
    (* Check if we reached target after taking at least one step *)
    if compare_configs config target_config = 0 && depth > 0 then (
      Printf.printf "    DFS: Found target after %d steps!\n%!" depth;
      true
    ) else if ConfigMap.mem config !visited || ConfigMap.mem config forbidden_set then (
      Printf.printf "    DFS: Already visited or forbidden\n%!";
      false
    ) else begin
      visited := ConfigMap.add config () !visited;
      
      (* Try all possible transitions *)
      let pairs = find_communication_pairs cfsm config in
      Printf.printf "    DFS: Found %d communication pairs\n%!" (List.length pairs);
      List.exists (fun (p, q) ->
        Printf.printf "    DFS: Trying pair %d -> %d\n%!" p q;
        (* Try message transition *)
        (match execute_message_transition cfsm config p q with
         | Some (_, new_config) -> 
             Printf.printf "    DFS: Message transition leads to %s\n%!" (format_config new_config);
             dfs new_config (depth + 1)
         | None -> false) ||
        (* Try choice transitions *)
        (execute_choice_transition cfsm config p q |> List.exists (fun (_, new_config) -> 
           Printf.printf "    DFS: Choice transition leads to %s\n%!" (format_config new_config);
           dfs new_config (depth + 1)))
      ) pairs
    end
  in
  let result = dfs start_config 0 in
  Printf.printf "    DFS: Final result = %b\n%!" result;
  result

(* Main synthesis algorithm 
   
   PARAMETERS:
   - cfsm: Array of local automata, one per participant
   - participant_roles: Maps participant index to role name  
   - config: Current configuration (state of each participant)
   - memo: Memoization map for recursion detection
   - queue: Set of participant pairs already processed
   - guarded: Whether current synthesis context is guarded
   
   RETURNS: (global_type, updated_memo)
   
   ALGORITHM CASES:
   1. Memoization: Return cached variable if config seen and guarded
   2. Parallel decomposition: Split into independent components
   3. Recursion: Detect loops and create recursive types
   4. Termination: Handle configurations with no transitions
   5. Communication: Synthesize message passing and choices
*)
let rec synth_helper (cfsm : cfsm) (participant_roles : role array) (config : configuration) (memo : int ConfigMap.t) (queue : pair_queue) (guarded : bool) : int global * int ConfigMap.t =
  
  Printf.printf "\n--- synth_helper called ---\n%!";
  Printf.printf "Config: %s\n%!" (format_config config);
  Printf.printf "Guarded: %b\n%!" guarded;
  Printf.printf "Memo size: %d\n%!" (ConfigMap.cardinal memo);
  Printf.printf "Queue size: %d\n%!" (queue_size queue);
  
  (* Case 1: Memoization - if guarded and config already seen *)
  if guarded && ConfigMap.mem config memo then (
    let var = ConfigMap.find config memo in
    Printf.printf "MEMOIZATION HIT: Found var %d for config %s\n%!" var (format_config config);
    (GVar (var, Loc.dummy), memo)
  )

  (* Case 2: Parallel decomposition *)
  else begin
    let components = split cfsm config in
    Printf.printf "PARALLEL DECOMPOSITION: Found %d components\n%!" (List.length components);
    List.iteri (fun i comp -> 
      Printf.printf "  Component %d: %s\n%!" i (format_config comp)
    ) components;
    
    match components with
    | [] -> (* Empty components - shouldn't happen, but handle gracefully *)
        Printf.printf "RESULT: GEnd (empty components)\n%!";
        (GEnd Loc.dummy, memo)
    | [_] -> (* Single component, continue with other cases *)
        Printf.printf "SINGLE COMPONENT: Continuing with communication analysis\n%!";
      
        (* Case 2.5: Recursion detection *)
        let memo_configs = ConfigMap.bindings memo |> List.map fst in
        Printf.printf "RECURSION CHECK: Testing if config can reach itself\n%!";
        Printf.printf "  Current config: %s\n%!" (format_config config);
        Printf.printf "  Forbidden configs (memo): %d\n%!" (List.length memo_configs);
        List.iteri (fun i forbidden_config ->
          Printf.printf "    Forbidden %d: %s\n%!" i (format_config forbidden_config)
        ) memo_configs;
        
        let can_reach = can_reach_config cfsm config config memo_configs in
        Printf.printf "  Can reach result: %b\n%!" can_reach;
        
        if can_reach then (
          Printf.printf "RECURSION DETECTED: Config can reach itself\n%!";
          let fresh_var = fresh_var () in
          Printf.printf "  Created fresh variable: %d\n%!" fresh_var;
          let updated_memo = ConfigMap.add config fresh_var memo in
          Printf.printf "  Added mapping: config -> var %d\n%!" fresh_var;
          let (body, final_memo) = synth_helper cfsm participant_roles config updated_memo queue false in
          Printf.printf "RESULT: GRec(%d, body)\n%!" fresh_var;
          (GRec (fresh_var, body, Loc.dummy), final_memo)
        ) else (
          Printf.printf "NO RECURSION: Config cannot reach itself or would use forbidden configs\n%!";
        
          (* Case 3: Terminal configuration *)
          if is_terminal_config cfsm config then (
            Printf.printf "TERMINAL CONFIG: All participants terminated\n%!";
            Printf.printf "RESULT: GEnd\n%!";
            (GEnd Loc.dummy, memo)
          )
        
          (* Case 4: Communication transitions *)
          else begin
          (* Calculate available pairs once and update queue *)
          let available_pairs = find_communication_pairs cfsm config in
          Printf.printf "COMMUNICATION ANALYSIS: Found %d available pairs\n%!" (List.length available_pairs);
          List.iteri (fun i (p, q) ->
            Printf.printf "  Pair %d: %s -> %s\n%!" i participant_roles.(p) participant_roles.(q)
          ) available_pairs;
          
          (* Enqueue any new pairs not already in queue *)
          let updated_queue = queue_enqueue queue available_pairs in
          Printf.printf "QUEUE UPDATED: Size %d -> %d\n%!" (queue_size queue) (queue_size updated_queue);
          
          match available_pairs with
          | [] -> 
              Printf.printf "NO COMMUNICATION: No pairs available\n%!";
              Printf.printf "RESULT: GEnd\n%!";
              (GEnd Loc.dummy, memo)
          | _ -> 
              (* FIFO queue scheduling: dequeue first pair (all queued pairs are now available by invariant) *)
              if queue_is_empty updated_queue then
                failwith "Internal error: queue is empty but available pairs exist (violates invariant)"
              else
                let (selected_pair, remaining_queue) = queue_dequeue updated_queue in
                let (p, q) = selected_pair in
              Printf.printf "SELECTED PAIR: %s -> %s\n%!" participant_roles.(p) participant_roles.(q);
              
              (* Try message transition first *)
              (match execute_message_transition cfsm config p q with
               | Some (base, new_config) ->
                   Printf.printf "MESSAGE TRANSITION: %s -> %s [%s]\n%!" participant_roles.(p) participant_roles.(q) base;
                   Printf.printf "  New config: %s\n%!" (format_config new_config);
                   (* Pass remaining queue - next synth_helper call will calculate and enqueue new pairs *)
                   let (cont, final_memo) = synth_helper cfsm participant_roles new_config memo remaining_queue true in
                   Printf.printf "RESULT: GMsg(%s -> %s : %s)\n%!" participant_roles.(p) participant_roles.(q) base;
                   (GMsg (participant_roles.(p), participant_roles.(q), base, cont, Loc.dummy), final_memo)
               
               (* Try choice transition *)
               | None ->
                   Printf.printf "NO MESSAGE: Trying choice transitions\n%!";
                   let choice_transitions = execute_choice_transition cfsm config p q in
                   Printf.printf "CHOICE TRANSITIONS: Found %d branches\n%!" (List.length choice_transitions);
                   List.iteri (fun i (label, new_config) ->
                     Printf.printf "  Branch %d: label=%s, config=%s\n%!" i label (format_config new_config)
                   ) choice_transitions;
                   
                   match choice_transitions with
                   | [] -> 
                       Printf.printf "NO CHOICES: No valid transitions\n%!";
                       Printf.printf "RESULT: GEnd\n%!";
                       (GEnd Loc.dummy, memo) (* No valid transitions *)
                   | branches ->
                       Printf.printf "PROCESSING BRANCHES\n%!";
                       let (branch_types, final_memo) = List.fold_left (fun (acc_branches, acc_memo) (label, new_config) ->
                         Printf.printf "  Processing branch: %s\n%!" label;
                         (* Pass remaining queue - next synth_helper call will calculate and enqueue new pairs *)
                         let (branch_type, new_memo) = synth_helper cfsm participant_roles new_config acc_memo remaining_queue true in
                         ((label, branch_type) :: acc_branches, new_memo)
                       ) ([], memo) branches in
                       Printf.printf "RESULT: GBra(%s -> %s)\n%!" participant_roles.(p) participant_roles.(q);
                       (GBra (participant_roles.(p), participant_roles.(q), List.rev branch_types, Loc.dummy), final_memo))
          end
        )
    
    | components -> (* Multiple components - use n-ary parallel composition *)
        Printf.printf "MULTIPLE COMPONENTS: Found %d parallel components\n%!" (List.length components);
        (* 
         * N-ARY PARALLEL COMPOSITION STRATEGY:
         * 1. Synthesize all components in sequence, threading memo state through each
         * 2. Build unbalanced left-associative tree: ((G1 | G2) | G3) | G4...
         * 3. Simple and efficient - no need for balanced trees
         *)
        let rec synthesize_components comps current_memo comp_num =
          match comps with
          | [] -> ([], current_memo)
          | comp :: remaining ->
              Printf.printf "  Synthesizing component %d: %s\n%!" comp_num (format_config comp);
              let (comp_type, updated_memo) = synth_helper cfsm participant_roles comp current_memo [] guarded in
              let (remaining_types, final_memo) = synthesize_components remaining updated_memo (comp_num + 1) in
              (comp_type :: remaining_types, final_memo)
        in
        
        let (component_types, final_memo) = synthesize_components components memo 1 in
        
        (* Build left-associative parallel composition tree (unbalanced is fine) *)
        let build_parallel_tree types =
          match types with
          | [] -> failwith "Empty component list - should not happen"
          | [single] -> 
              Printf.printf "  Single component result\n%!";
              single
          | first :: rest ->
              Printf.printf "  Building parallel tree with %d components\n%!" (List.length types);
              List.fold_left (fun acc_type next_type ->
                GPar (acc_type, next_type, Loc.dummy)
              ) first rest
        in
        
        let parallel_type = build_parallel_tree component_types in
        Printf.printf "RESULT: GPar (parallel composition)\n%!";
        (parallel_type, final_memo)
  end

(* Main synthesis entry point *)
let synth (cfsm : cfsm) (participant_roles : role array) : int global =
  Printf.printf "\n=== SYNTHESIS DEBUG START ===\n%!";
  Printf.printf "Participants: %s\n%!" (format_roles participant_roles);
  Printf.printf "Number of automata: %d\n%!" (Array.length cfsm);
  
  (* Debug automaton information *)
  Array.iteri (fun i automaton ->
    Printf.printf "Automaton %d (%s):\n%!" i participant_roles.(i);
    Printf.printf "  States: %d\n%!" automaton.num_states;
    Printf.printf "  Start: %s\n%!" (match automaton.start_state with Some s -> string_of_int s | None -> "None");
    for s = 0 to automaton.num_states - 1 do
      Printf.printf "    State %d: role=%d, " s automaton.roles.(s);
      (match automaton.kinds.(s) with
       | Local_automaton.Snd (base, dest) ->
           Printf.printf "Snd(%s, %s)\n%!" base (match dest with Some d -> string_of_int d | None -> "end")
       | Local_automaton.Rcv (base, dest) ->
           Printf.printf "Rcv(%s, %s)\n%!" base (match dest with Some d -> string_of_int d | None -> "end")
       | Local_automaton.Int branches ->
           Printf.printf "Int([%s])\n%!" (String.concat ";" (List.map (fun (lbl, dst) ->
             Printf.sprintf "%s->%s" lbl (match dst with Some d -> string_of_int d | None -> "end")) branches))
       | Local_automaton.Ext branches ->
           Printf.printf "Ext([%s])\n%!" (String.concat ";" (List.map (fun (lbl, dst) ->
             Printf.sprintf "%s->%s" lbl (match dst with Some d -> string_of_int d | None -> "end")) branches)))
    done
  ) cfsm;
  
  fresh_var_counter := 0;
  let initial_config = initial_configuration cfsm in
  Printf.printf "Initial configuration: %s\n%!" (format_config initial_config);
  
  (* Initialize queue with all initially available communication pairs *)
  let initial_pairs = find_communication_pairs cfsm initial_config in
  let initial_queue = queue_enqueue [] initial_pairs in
  let (result, _) = synth_helper cfsm participant_roles initial_config ConfigMap.empty initial_queue true in
  Printf.printf "=== SYNTHESIS DEBUG END ===\n\n%!";
  result
