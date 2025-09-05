open Live
open Local_automaton

(* Exhaustive liveness checker following the small‐model bound               *)
(* ------------------------------------------------------------------------ *)

(* Compute the exploration bound m                                           *)
let exploration_bound (automata : Local_automaton.graph array) : int =
  let sizes = Array.to_list (Array.map (fun (g : Local_automaton.graph) -> max 1 g.num_states) automata) in
  let n  = List.fold_left ( + ) 0 sizes in
  let n' = List.fold_left ( * ) 1 sizes in
  (2 * n + 2) * n'

(* Build the initial combined state (array of ints).                         *)
let initial_state (automata : Local_automaton.graph array) : combined_state =
  Array.mapi (fun _idx (g : Local_automaton.graph) -> match g.start_state with Some s -> s | None -> 0) automata

(* Helper for Option.value for older compilers *)
let option_value opt ~default = match opt with Some v -> v | None -> default

(* Enumerate outgoing transitions from a combined state.                     *)
let outgoing_transitions
    ~(participants : string list)
    ~(automata     : Local_automaton.graph array)
    (state : combined_state)
  : (combined_state * ActionSet.t) list =

  let len = Array.length automata in
  let transitions = ref [] in

  for i = 0 to len - 1 do
    let g      = automata.(i) in
    let p_name = List.nth participants i in
    let s_id   = state.(i) in
    if s_id < g.num_states then
      match g.kinds.(s_id) with
      | Local_automaton.Snd (base, next_opt) ->
          let tgt_role = g.roles.(s_id) in
          let next_state = option_value next_opt ~default:s_id in
          let new_arr = Array.copy state in
          new_arr.(i) <- next_state;
          let obs = ActionSet.singleton (Send (p_name, tgt_role, base)) in
          transitions := (new_arr, obs) :: !transitions

      | Local_automaton.Rcv (base, next_opt) ->
          let src_role = g.roles.(s_id) in
          let next_state = option_value next_opt ~default:s_id in
          let new_arr = Array.copy state in
          new_arr.(i) <- next_state;
          let obs = ActionSet.singleton (Receive (p_name, src_role, base)) in
          transitions := (new_arr, obs) :: !transitions

      | Local_automaton.Int branches ->
          let tgt_role = g.roles.(s_id) in
          List.iter (fun (label, next_opt) ->
            let next_state = option_value next_opt ~default:s_id in
            let new_arr = Array.copy state in
            new_arr.(i) <- next_state;
            let obs = ActionSet.singleton (InternalChoice (p_name, tgt_role, label)) in
            transitions := (new_arr, obs) :: !transitions
          ) branches

      | Local_automaton.Ext branches ->
          let src_role = g.roles.(s_id) in
          List.iter (fun (label, next_opt) ->
            let next_state = option_value next_opt ~default:s_id in
            let new_arr = Array.copy state in
            new_arr.(i) <- next_state;
            let obs = ActionSet.singleton (ExternalChoice (p_name, src_role, label)) in
            transitions := (new_arr, obs) :: !transitions
          ) branches
    else
      ()
  done;
  !transitions

(* ------------------------------------------------------------------------ *)
(* Exhaustive search                                                         *)
(* ------------------------------------------------------------------------ *)

(* Top-level exception used for early exit *)
exception Exit_with of liveness_result

let check_liveness
    ~(participants : string list)
    ~(automata     : Local_automaton.graph array) : liveness_result =

  let m = exploration_bound automata in
  let start_state = initial_state automata in

  (* Use DFS stack: each element is (state_list_rev, obs_list_rev) where       *)
  (* the head of state_list_rev is current state.                               *)
  let stack = Stack.create () in
  Stack.push ([start_state], []) stack;

  (* Simple helpers for OCaml < 5 that lack List.take / drop *)
  let rec list_take n l =
    if n <= 0 then []
    else match l with
      | [] -> []
      | x :: xs -> x :: list_take (n-1) xs
  and list_drop n l =
    if n <= 0 then l
    else match l with
      | [] -> []
      | _ :: xs -> list_drop (n-1) xs
  in

  (* helper equality for combined states *)
  let arrays_equal a b =
    let len = Array.length a in
    len = Array.length b &&
    let rec loop i =
      if i = len then true
      else if a.(i) = b.(i) then loop (i+1) else false
    in
    loop 0

  in

  let rec dfs () =
    if Stack.is_empty stack then Live else (
      let states_rev, obs_rev = Stack.pop stack in
      let current_state = List.hd states_rev in
      let path_len = List.length obs_rev in

      (* 1. Finite-path check *)
      let fp = { states = List.rev states_rev; observations = List.rev obs_rev } in
      if path_len > 0 && path_len <= m then (
        if is_counterwitness ~participants ~automata (FinitePath fp) then
          raise (Exit_with (NotLive (string_of_counterexample participants (FinitePath fp))));
      );

      (* Stop expanding if length already m *)
      if path_len < m then (
        let nexts = outgoing_transitions ~participants ~automata current_state in
        List.iter (fun (st', obs_set) ->
          (* Build new path components *)
          let new_states_rev = st' :: states_rev in
          let new_obs_rev    = obs_set :: obs_rev in
          (* Check for cycle to build infinite path candidate *)
          let rec find_repeat idx = function
            | [] -> None
            | s :: rest -> if arrays_equal s st' then Some idx else find_repeat (idx+1) rest
          in
          match find_repeat 0 states_rev with
          | Some k when (path_len + 1) <= m ->
              let full_states = List.rev new_states_rev in
              let full_obs    = List.rev new_obs_rev in
              let prefix_states = list_take (List.length states_rev - k) full_states in
              let prefix_obs    = list_take (List.length states_rev - k) full_obs in
              let cycle_states  = list_drop (List.length states_rev - k) full_states in
              (* For cycle: states[k..n] (including the repeated state) and obs[k..(n-1)] *)
              let cycle_obs     = list_drop (List.length states_rev - k) full_obs in
              (* Ensure cycle has proper state/observation relationship *)
              let cycle_states = cycle_states @ [List.hd cycle_states] in
              let ip = InfinitePath { prefix_states; prefix_observations = prefix_obs; cycle_states; cycle_observations = cycle_obs } in
              if is_counterwitness ~participants ~automata ip then
                raise (Exit_with (NotLive (string_of_counterexample participants ip)));
          | _ -> ();
          Stack.push (new_states_rev, new_obs_rev) stack
        ) nexts
      );
      dfs ()
    )
  in
  try dfs () with Exit_with res -> res 