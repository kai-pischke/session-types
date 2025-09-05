open Local_automaton

(* cfsm[p] = local automaton for participant p *)
type cfsm = int_graph array 

(* configuration[p] = state of local automaton for participant p *)
type configuration = int option array

(* a particpant p is directly connected to a partipant q if p 
can eventually send to q or receive from q, 
(including send, receive and internal/external choices), 
starting from the current state given by the configuration. 
We create a graph of connections between participants. 
We are interested in the connected components of this graph which represent 
partipants that can eventually interact with each other. *)

(* split the configuration into a list of disjoint configurations, 
where each configuration contains only particpants in a 
different connected component of the particpant graph 

COMPLEXITY ANALYSIS:
- Time: O(P * S * T + P^2) where P = participants, S = max states, T = max transitions
- Space: O(P * S + P^2) for visited arrays and adjacency lists

INVARIANTS:
- Input: |cfsm| = |config| = n (same number of participants)
- Input: ∀p ∈ [0,n): config[p] is None or valid state in cfsm[p]
- Output: Partition of participants into disjoint connected components
- Output: Each component preserves original state information
*)
let split (cfsm : cfsm) (config : configuration) : configuration list = 
  (* number of participants *)
  let n = Array.length cfsm in
  (* INVARIANT: n = |cfsm| = |config| *)
  
  (* Special case: if all participants are terminated, return single component *)
  let all_terminated = Array.for_all (fun state -> state = None) config in
  if all_terminated then []
  else
  
    (* build connectivity graph: adj[p] = set of participants that p can communicate with *)
    let adj = Array.make n [] in
    
    (* helper to get all participants that p can reach from given state 
       COMPLEXITY: O(S * T) per call, where S = states, T = transitions per state
       INVARIANT: visited[s] = true iff state s has been explored
       TERMINATES: visited array ensures each state visited at most once *)
      let rec get_reachable_participants (p : int) (state : int option) (visited : bool array) : int list =
        match state with
        | None -> [] (* terminated, no connections *)
        | Some s when s >= Array.length visited || visited.(s) -> [] (* out of bounds or already visited *)
        | Some s ->
            (* INVARIANT: s is valid state index and unvisited *)
            visited.(s) <- true;
            let automaton = cfsm.(p) in
            if s >= automaton.num_states then []
            else
              (* INVARIANT: s < automaton.num_states, so automaton.kinds[s] is valid *)
              match automaton.kinds.(s) with
              | Snd (_, next_state) ->
                  (* send action: communicates with role at this state *)
                  let target_role = automaton.roles.(s) in
                  target_role :: get_reachable_participants p next_state visited
              | Rcv (_, next_state) ->
                  (* receive action: communicates with role at this state *)
                  let target_role = automaton.roles.(s) in
                  target_role :: get_reachable_participants p next_state visited
              | Int branches ->
                  (* internal choice: communicate with role at this state, then explore branches 
                     COMPLEXITY: O(|branches| * recursive_cost) *)
                  let target_role = automaton.roles.(s) in
                  let branch_reachable = List.fold_left (fun acc (_, next_state) ->
                    acc @ get_reachable_participants p next_state visited
                  ) [] branches in
                  target_role :: branch_reachable
              | Ext branches ->
                  (* external choice: communicate with role at this state, then explore branches 
                     COMPLEXITY: O(|branches| * recursive_cost) *)
                  let target_role = automaton.roles.(s) in
                  let branch_reachable = List.fold_left (fun acc (_, next_state) ->
                    acc @ get_reachable_participants p next_state visited
                  ) [] branches in
                  target_role :: branch_reachable
      in
      
      (* build adjacency list for each participant 
         COMPLEXITY: O(P * S * T) where P = participants, S = max states, T = max transitions
         INVARIANT: ∀p ∈ [0,n): adj[p] contains only valid participant indices in [0,n) *)
      for p = 0 to n - 1 do
        (* defensive bounds check to prevent crashes on malformed input *)
        let max_states = if p < Array.length cfsm then cfsm.(p).num_states else 0 in
        let visited = Array.make (max 1 max_states) false in
        let reachable = get_reachable_participants p config.(p) visited in
        (* remove duplicates and filter valid participant indices 
           COMPLEXITY: O(R log R) where R = |reachable| due to sort_uniq *)
        let unique_reachable = List.sort_uniq compare (List.filter (fun q -> q >= 0 && q < n) reachable) in
        adj.(p) <- unique_reachable
        (* POST: adj[p] ⊆ [0,n) and no duplicates *)
      done;
      
      (* find connected components using DFS 
         COMPLEXITY: O(P^2) due to bidirectional connectivity check
         INVARIANT: visited[p] = true iff p has been assigned to a component *)
      let visited = Array.make n false in
      let components = ref [] in
      
      (* DFS to find all participants in same connected component 
         INVARIANT: component contains all participants reachable from starting point
         TERMINATES: visited array prevents revisiting participants *)
      let rec dfs (p : int) (component : int list ref) =
        if not visited.(p) && config.(p) <> None then begin
          visited.(p) <- true;
          component := p :: !component;
          (* explore both directions: who p can reach and who can reach p 
             COMPLEXITY: O(P) per participant due to reverse connectivity check *)
          List.iter (fun q -> dfs q component) adj.(p);
          for q = 0 to n - 1 do
            if List.mem p adj.(q) then dfs q component
          done
        end
      in
      
      (* find all connected components 
         COMPLEXITY: O(P) since each participant visited exactly once
         INVARIANT: components form a partition of active participants only *)
      for p = 0 to n - 1 do
        (* Skip terminated participants - they cannot communicate *)
        if not visited.(p) && config.(p) <> None then begin
          let component = ref [] in
          dfs p component;
          if !component <> [] then
            components := !component :: !components
        end
      done;
  
    (* create separate configurations for each component 
       COMPLEXITY: O(P * C) where C = number of components
       INVARIANT: ∪components = [0,n) and components are pairwise disjoint
       POST: Each output config preserves original states for component participants *)
    List.map (fun component ->
      let new_config = Array.make n None in
      (* INVARIANT: new_config[q] = None for all q ∉ component *)
      List.iter (fun p -> 
        if p >= 0 && p < n then new_config.(p) <- config.(p)
        (* POST: new_config[p] = config[p] for p ∈ component ∩ [0,n) *)
      ) component;
      new_config
    ) !components
    (* POST: Result is list of disjoint configurations, one per connected component *)