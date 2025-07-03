(* live.ml - Liveness checking for session type automata *)

(* ------------------------------------------------------------ *)
(* Basic types                                                   *)
(* ------------------------------------------------------------ *)

(* A mapping from participants to their local automata *)
type participant_map = (string * Local_automaton.graph) list

(* A state in the combined automata with p participants *)
(* Position m gives the state of the automaton for participant m *)
type combined_state = int array

(* An action is a possible transition available to a single automaton *)
type action = 
  | Send of string * string * string          (* p : q ! [msg]  *)
  | Receive of string * string * string       (* p : q ? [msg]  *)
  | InternalChoice of string * string * string(* p : q ! {lbl}  *)
  | ExternalChoice of string * string * string(* p : q ? {lbl}  *)

(* Module for sets of actions *)
module ActionSet = Set.Make(struct
  type t = action
  let compare = compare
end)

(* An observation is an unordered pair / set of the two complementary actions
   that occur on a synchronous communication step. We just record the set. *)
type observation = ActionSet.t

(* ------------------------------------------------------------ *)
(* Path descriptions                                             *)
(* ------------------------------------------------------------ *)

(* A finite path of combined states *)
type finite_path = {
  states : combined_state list;
  observations : ActionSet.t list  (* observations[i] between states[i] and states[i+1] *)
}

(* An infinite path = finite prefix followed by a cycle that repeats forever *)
type infinite_path = {
  prefix_states : combined_state list;
  prefix_observations : ActionSet.t list;
  cycle_states : combined_state list;      (* c0 .. c_{k-1}; edge from last back to c0 *)
  cycle_observations : ActionSet.t list;   (* same length as cycle_states *)
}

(* Either finite or infinite *)
type liveness_counterexample =
  | FinitePath of finite_path
  | InfinitePath of infinite_path

(* ------------------------------------------------------------ *)
(* Pretty-printing                                               *)
(* ------------------------------------------------------------ *)

(* Pretty print a combined state *)
let string_of_combined_state (participants : string list) (state : combined_state) : string =
  let pairs = List.combine participants (Array.to_list state) in
  let state_strings = List.map (fun (p, s) -> 
    Printf.sprintf "%s:%d" p s
  ) pairs in
  "[" ^ String.concat ", " state_strings ^ "]"

(* Pretty print a combined state with custom formatting *)
let pp_combined_state fmt (participants : string list) (state : combined_state) =
  let pairs = List.combine participants (Array.to_list state) in
  Format.fprintf fmt "[";
  let rec print_pairs = function
    | [] -> ()
    | [(p, s)] -> Format.fprintf fmt "%s:%d" p s
    | (p, s) :: rest -> 
        Format.fprintf fmt "%s:%d, " p s;
        print_pairs rest
  in
  print_pairs pairs;
  Format.fprintf fmt "]"

(* Pretty print a finite path *)
let string_of_finite_path (participants : string list) (path : finite_path) : string =
  let rec print_path states observations =
    match states, observations with
    | [], [] -> ""
    | [state], [] -> string_of_combined_state participants state
    | state1 :: state2 :: rest_states, obs :: rest_obs ->
        let state1_str = string_of_combined_state participants state1 in
        let obs_str = String.concat ", " (List.map (fun act -> 
          match act with
          | Send (p1, p2, msg) -> Printf.sprintf "%s:%s![%s]" p1 p2 msg
          | Receive (p1, p2, msg) -> Printf.sprintf "%s:%s?[%s]" p1 p2 msg
          | InternalChoice (p1, p2, label) -> Printf.sprintf "%s:%s!{%s}" p1 p2 label
          | ExternalChoice (p1, p2, label) -> Printf.sprintf "%s:%s?{%s}" p1 p2 label
        ) (ActionSet.elements obs)) in
        state1_str ^ " --{" ^ obs_str ^ "}--> " ^ 
        print_path (state2 :: rest_states) rest_obs
    | _, _ -> failwith "Mismatch between states and observations"
  in
  print_path path.states path.observations

(* Pretty print an infinite path *)
let string_of_infinite_path (participants : string list) (path : infinite_path) : string =
  let prefix_str = 
    if List.length path.prefix_states > 0 then
      string_of_finite_path participants {states = path.prefix_states; observations = path.prefix_observations}
    else ""
  in
  let cycle_str = string_of_finite_path participants {states = path.cycle_states; observations = path.cycle_observations} in
  if List.length path.prefix_states > 0 then
    prefix_str ^ " -> (" ^ cycle_str ^ ")*"
  else
    "(" ^ cycle_str ^ ")*"

(* Pretty print a counterexample *)
let string_of_counterexample (participants : string list) (cex : liveness_counterexample) : string =
  match cex with
  | FinitePath path -> string_of_finite_path participants path
  | InfinitePath path -> string_of_infinite_path participants path

(* ------------------------------------------------------------ *)
(* Result of liveness analysis                                  *)
(* ------------------------------------------------------------ *)

type liveness_result =
  | Live
  | NotLive of string

(* ------------------------------------------------------------ *)
(* Get the set of all individually possible actions at a combined state *)
(* ------------------------------------------------------------ *)

let barb
         (participants : string list)
         (automata : Local_automaton.graph array) 
         (state : combined_state) : ActionSet.t =
  let actions = ref ActionSet.empty in
  
  (* Helper to add an action *)
  let add_action act = actions := ActionSet.add act !actions in
  
  (* Process each participant's automaton *)
  List.iteri (fun i participant ->
    let state_id = state.(i) in
    let automaton = automata.(i) in
    
    (* Check if state_id is valid for this automaton *)
    if state_id < automaton.num_states then
      let state_kind = automaton.kinds.(state_id) in
      
      match state_kind with
      | Local_automaton.Snd (base, _) ->
          let target_role = automaton.roles.(state_id) in
          add_action (Send (participant, target_role, base))
          
      | Local_automaton.Rcv (base, _) ->
          let source_role = automaton.roles.(state_id) in
          add_action (Receive (participant, source_role, base))
          
      | Local_automaton.Int branches ->
          (* For internal choice, we need to find the target participant *)
          List.iter (fun (label, _next_state_opt) ->
            let target_role = automaton.roles.(state_id) in
            add_action (InternalChoice (participant, target_role, label))
          ) branches
          
      | Local_automaton.Ext branches ->
          (* For external choice, we need to find the source participant *)
          List.iter (fun (label, _next_state_opt) ->
            let source_role = automaton.roles.(state_id) in
            add_action (ExternalChoice (participant, source_role, label))
          ) branches
    else
      ()  (* Invalid state, no actions possible *)
  ) participants;
  
  !actions 

(* ------------------------------------------------------------ *)
(* Counter-witness checker                                       *)
(* ------------------------------------------------------------ *)

(* Return [true] iff the given path is a counter-witness, i.e.
   ∃ j such that  barb(j) ⊄ ⋃_{k≥j} obs(k).
   [participants] and [automata] must be in the same order as the
   indices used inside [combined_state] arrays. *)
let is_counterwitness
    ~(participants : string list)
    ~(automata     : Local_automaton.graph array)
    (path : liveness_counterexample) : bool =

  (* Helper: barb set for a single state *)
  let barb_of_state (st : combined_state) : ActionSet.t =
    barb participants automata st
  in

  (* Helper: check finite portion (states list + observations list)   
     Return [Some true] immediately when a violation is found,        
     [None] otherwise.                                                *)
  let check_finite states observations suffix_after : bool =
    (* Convert to arrays for O(1) access *)
    let state_arr = Array.of_list states in
    let n_states = Array.length state_arr in
    (* observations length should be n_states - 1 *)
    let obs_arr  = Array.of_list observations in
    let n_obs    = Array.length obs_arr in
    if n_states = 0 then false else (
      (* Suffix unions of observations; last state uses suffix_after *)
      let suffix = Array.make n_states ActionSet.empty in
      if n_states > 1 then (
        suffix.(n_states-1) <- suffix_after;
        for i = n_obs-1 downto 0 do
          suffix.(i) <- ActionSet.union obs_arr.(i) suffix.(i+1)
        done
      ) else (
        suffix.(0) <- suffix_after
      );
      (* Check property *)
      let rec loop i =
        if i = n_states then false
        else
          let barb_i = barb_of_state state_arr.(i) in
          if not (ActionSet.subset barb_i suffix.(i)) then true
          else loop (i+1)
      in
      loop 0 )
  in

  match path with
  | FinitePath fp ->
      (* For a purely finite path, suffix_after for last state is empty *)
      check_finite fp.states fp.observations ActionSet.empty

  | InfinitePath ip ->
      (* Union of one entire cycle's observations – this is the suffix
         union seen from any position once we are inside the cycle. *)
      let union_cycle =
        List.fold_left (fun acc obs -> ActionSet.union acc obs)
          ActionSet.empty ip.cycle_observations
      in
      (* 1) Check states in prefix.  The suffix union for a prefix
         position is built from the remaining prefix observations
         plus [union_cycle]. *)
      let violation_prefix =
        check_finite ip.prefix_states ip.prefix_observations union_cycle
      in
      if violation_prefix then true else (
        (* 2) Check states in the cycle – same suffix for everyone *)
        let rec exists_violation_cycle states =
          match states with
          | [] -> false
          | st :: rest ->
              let barb_st = barb_of_state st in
              if not (ActionSet.subset barb_st union_cycle) then true
              else exists_violation_cycle rest
        in
        exists_violation_cycle ip.cycle_states ) 