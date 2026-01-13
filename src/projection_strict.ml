(* Strict projection: automaton-based with exact label matching for external choice *)

open Ast

module G = Automaton
module L = Local_automaton
module IntSet = G.IntSet
module StringMap = Map.Make(String)

exception Not_projectable of string

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

(* A knowledge set is the set of global states that the participant considers
   possible at a given point in the execution. *)
type knowledge = IntSet.t

(* Map from knowledge sets to freshly-allocated local state identifiers. *)
module KnowledgeMap = Map.Make(struct
  type t = knowledge
  let compare = IntSet.compare
end)

(* A step extracted from a knowledge set *)
type step_result =
  | Stop                                              (* no outgoing transitions *)
  | SendStep of role * base * knowledge option        (* peer role, base, succ   *)
  | RecvStep of role * base * knowledge option        (* peer role, base, succ   *)
  | IntStep  of role * (label * knowledge option) list(* internal choice with peer role *)
  | ExtStep  of role * (label * knowledge option) list(* external choice *)

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* -------------------------------------------------------------------------- *)

(** Return true if participant [p] is *involved* in state [id] (i.e.
    it appears as either sender or receiver in the state's role pair). *)
let participant_involved (g : G.graph) (p : role) (id : int) : bool =
  let sender, receiver = g.roles.(id) in
  String.equal sender p || String.equal receiver p

(** Successor states of [id] *excluding* transitions that involve [p]. *)
let succs_without_p (g : G.graph) (p : role) (id : int) : IntSet.t =
  if participant_involved g p id then
    (* All outgoing transitions involve p: ignore them when computing knowledge set *)
    IntSet.empty
  else
    match g.kinds.(id) with
    | G.Msg (_, dsts) -> dsts
    | G.Bra branches  ->
        List.fold_left (fun acc (_, dsts) -> IntSet.union acc dsts) IntSet.empty branches

(** [compute_knowledge_set g p init] computes the knowledge set for participant [p]
    starting from the set [init] of global states. This function:
    - Traverses through transitions that do NOT involve [p]
    - Only includes in the result states where [p] IS involved
    - Returns the set of all states involving [p] that are reachable from [init]
      without crossing any transition involving [p]. *)
let compute_knowledge_set (g : G.graph) (p : role) (init : IntSet.t) : IntSet.t =
  (* BFS over transitions that do NOT involve p.                         *)
  (* visited : all states we have processed (for termination)            *)
  (* knowledge: only those visited states that themselves involve p      *)
  let rec bfs worklist visited knowledge =
    match worklist with
    | [] -> knowledge
    | id :: rest ->
        if IntSet.mem id visited then
          bfs rest visited knowledge                    (* already processed *)
        else
          let visited' = IntSet.add id visited in
          let knowledge' =
            if participant_involved g p id then IntSet.add id knowledge else knowledge
          in
          let succs = IntSet.elements (succs_without_p g p id) in
          bfs (succs @ rest) visited' knowledge'
  in
  bfs (IntSet.elements init) IntSet.empty IntSet.empty

(* Utility: pretty-print a knowledge set *)
let string_of_knowledge (ks : knowledge) : string =
  "{" ^
  (String.concat ", " (List.map string_of_int (IntSet.elements ks))) ^
  "}"

(** [compute_initial_knowledge g p] computes the initial knowledge set for
    participant [p] in the global automaton [g]. *)
let compute_initial_knowledge (g : G.graph) (p : role) : IntSet.t =
  let start_states = g.start_states in
  compute_knowledge_set g p start_states

(* -------------------------------------------------------------------------- *)
(* Transition analysis for a knowledge set                                    *)
(* -------------------------------------------------------------------------- *)

(** Gather all outgoing message transitions from states in [kset] that leave
    the set.  Assumes [kset] is computed using compute_knowledge_set.  Performs consistency checks. *)
let analyse_transitions
    ~(g : G.graph)
    ~(participant : role)
    (kset : knowledge)
  : step_result =
  (* Collected information *)
  let dir_ref   = ref None  (* [`Send] or [`Recv] *) in
  let base_ref  = ref None  (* message base          *) in
  let other_ref = ref None  (* counter-party role     *) in
  let final_ref = ref None  (* bool – dest unreachable or no dest *) in
  let union_dests = ref IntSet.empty in

  (* Data for branching made by participant (internal choice) *)
  let branch_mode   = ref false in                     (* are we in branching mode?        *)
  let labels_ref    = ref None in                      (* String Set of labels             *)
  let other_role_ref= ref None in                      (* unique counter-party role         *)
  let branch_map    = ref StringMap.empty in           (* label -> union dests             *)

  (* Data for branching made by others (external choice) *)
  let ext_branch_mode = ref false in                   (* are we in external branching mode? *)
  let ext_branch_map = ref StringMap.empty in          (* label -> union dests for external *)
  let ext_labels_ref = ref None in                     (* String list of labels - must be consistent *)
  let ext_other_ref = ref None in                      (* unique counter-party role for external *)

  let record_transition dir base other fin dst_set =
    (* consistency checks *)
    (match !dir_ref with
     | None -> dir_ref := Some dir
     | Some d when d <> dir -> raise (Not_projectable "conflicting send/recv")
     | _ -> ());
    (match !base_ref with
     | None -> base_ref := Some base
     | Some b when b <> base -> raise (Not_projectable "conflicting message base")
     | _ -> ());
    (match !other_ref with
     | None -> other_ref := Some other
     | Some o when o <> other -> raise (Not_projectable "conflicting peer role")
     | _ -> ());
    (match !final_ref with
     | None -> final_ref := Some fin
     | Some f when f <> fin -> raise (Not_projectable "inconsistent finality")
     | _ -> ());
    union_dests := IntSet.union !union_dests dst_set
  in

  (* Iterate over states in the knowledge set *)
  IntSet.iter (fun id ->
    match g.kinds.(id) with
    | G.Msg (base, dsts) ->
        let sender, receiver = g.roles.(id) in
        let involves_p = participant_involved g participant id in
        if involves_p then (
          let dir, other =
            if String.equal sender participant then (`Send, receiver)
            else (`Recv, sender)
          in
          if IntSet.is_empty dsts then (
            (* Transition to end; treat as final *)
            record_transition dir base other true IntSet.empty
          ) else (
            IntSet.iter (fun dst ->
              (* A transition is final if no states involving the participant are reachable from dst *)
              let dst_kset = compute_knowledge_set g participant (IntSet.singleton dst) in
              let fin = IntSet.is_empty dst_kset in
              record_transition dir base other fin (IntSet.singleton dst)
            ) dsts
          )
        )

    | G.Bra branches ->
        (* Handle branching based on whether participant is involved *)
        let sender, receiver = g.roles.(id) in
        if participant_involved g participant id then (
          if String.equal sender participant then (
            (* Internal choice: participant is the sender *)
            branch_mode := true;
            (* Ensure unique other role *)
            (match !other_role_ref with
             | None -> other_role_ref := Some receiver
             | Some r when r <> receiver -> raise (Not_projectable "conflicting peer role in branching")
             | _ -> ());

            (* Collect label set consistency *)
            let lbls = List.map fst branches |> List.sort_uniq String.compare in
            (match !labels_ref with
             | None -> labels_ref := Some lbls
             | Some existing when existing <> lbls -> raise (Not_projectable "branching label mismatch")
             | _ -> ());

            (* Accumulate destinations per label *)
            List.iter (fun (lbl, dsts) ->
              let acc = match StringMap.find_opt lbl !branch_map with
                | None -> IntSet.empty
                | Some s -> s in
              let acc' = IntSet.union acc dsts in
              branch_map := StringMap.add lbl acc' !branch_map
            ) branches
          ) else (
            (* Receive branching: participant is the receiver – treated as external choice *)
            ext_branch_mode := true;
            (* sender is the peer role; ensure consistency *)
            (match !ext_other_ref with
             | None -> ext_other_ref := Some sender
             | Some o when o <> sender -> raise (Not_projectable "conflicting peer role in external branching recv")
             | _ -> ());
            (* dir must be Recv *)
            (match !dir_ref with
             | None -> dir_ref := Some `Recv
             | Some `Recv -> ()
             | Some `Send -> raise (Not_projectable "conflicting send/recv in branching recv"));
            
            (* STRICT: Check that all branches have the same label set *)
            let lbls = List.map fst branches |> List.sort_uniq String.compare in
            (match !ext_labels_ref with
             | None -> ext_labels_ref := Some lbls
             | Some existing when existing <> lbls -> raise (Not_projectable "external branching label mismatch (strict)")
             | _ -> ());
            
            (* accumulate label dests into ext_branch_map *)
            List.iter (fun (lbl,dsts) ->
              let acc = Option.value ~default:IntSet.empty (StringMap.find_opt lbl !ext_branch_map) in
              let acc' = IntSet.union acc dsts in
              ext_branch_map := StringMap.add lbl acc' !ext_branch_map)
            branches
          )
        ) else (
          (* External choice: participant is not involved *)
          ext_branch_mode := true;
          
          (* STRICT: Check that all branches have the same label set *)
          let lbls = List.map fst branches |> List.sort_uniq String.compare in
          (match !ext_labels_ref with
           | None -> ext_labels_ref := Some lbls
           | Some existing when existing <> lbls -> raise (Not_projectable "external branching label mismatch for uninvolved participant (strict)")
           | _ -> ());
          
          (* Ensure consistent peer role *)
          let sender, receiver = g.roles.(id) in
          let peer_role = if String.equal sender participant then receiver else sender in
          (match !ext_other_ref with
           | None -> ext_other_ref := Some peer_role
           | Some o when o <> peer_role -> raise (Not_projectable "conflicting peer roles in external choice")
           | _ -> ());
          
          (* Collect destinations per label for external choice *)
          List.iter (fun (lbl, dsts) ->
            let acc = match StringMap.find_opt lbl !ext_branch_map with
              | None -> IntSet.empty
              | Some s -> s in
            let acc' = IntSet.union acc dsts in
            ext_branch_map := StringMap.add lbl acc' !ext_branch_map
          ) branches
        )
  ) kset;

  (* If we observed branching by participant, build internal choice step *)
  if !branch_mode then (
    let lbls = Option.get !labels_ref in
    let peer = Option.get !other_role_ref in
    let branches =
      List.map (fun lbl ->
        let dsts = Option.value ~default:IntSet.empty (StringMap.find_opt lbl !branch_map) in
        let succ_opt =
          if IntSet.is_empty dsts then None
          else (
            let ks = compute_knowledge_set g participant dsts in
            if IntSet.is_empty ks then None else Some ks)
        in
        (lbl, succ_opt)) lbls in
    IntStep (peer, branches)
  ) else if !ext_branch_mode then (
    let peer =
      (match !ext_other_ref with
       | Some r -> r
       | None -> 
           (match !other_ref with
            | Some r -> r
            | None -> raise (Not_projectable "cannot determine peer role for external choice"))) in
    (* Build external choice step using the consistent label set *)
    let lbls = Option.get !ext_labels_ref in
    let branches =
      List.map (fun lbl ->
        let dsts = Option.value ~default:IntSet.empty (StringMap.find_opt lbl !ext_branch_map) in
        let succ_opt =
          if IntSet.is_empty dsts then None
          else (
            let ks = compute_knowledge_set g participant dsts in
            if IntSet.is_empty ks then None else Some ks)
        in
        (lbl, succ_opt)) lbls in
    ExtStep (peer, branches)
  ) else
  match !dir_ref with
  | None -> Stop   (* no outgoing transitions *)
  | Some `Send ->
      let peer = Option.get !other_ref in
      let succ_opt =
        if Option.get !final_ref then None
        else Some (compute_knowledge_set g participant !union_dests)
      in
      SendStep (peer, Option.get !base_ref, succ_opt)
  | Some `Recv ->
      let peer = Option.get !other_ref in
      let succ_opt =
        if Option.get !final_ref then None
        else Some (compute_knowledge_set g participant !union_dests)
      in
      RecvStep (peer, Option.get !base_ref, succ_opt)

(* -------------------------------------------------------------------------- *)
(* Updated step_knowledge                                                      *)
(* -------------------------------------------------------------------------- *)

(** Analyse [current] knowledge set and return next step (send/recv/stop) and
    successor knowledge set, performing all consistency checks described
    earlier.  May raise [Not_projectable]. *)
let step_knowledge
    ~(g : G.graph)
    ~(participant : role)
    ~(current : knowledge)
  : step_result =
  analyse_transitions ~g ~participant current

(* -------------------------------------------------------------------------- *)
(* Debug utilities                                                             *)
(* -------------------------------------------------------------------------- *)

(* Debug helper *)
let debug_initial_knowledge (_:G.graph) (_:role) : unit = ()

(* -------------------------------------------------------------------------- *)
(* Public API                                                                 *)
(* -------------------------------------------------------------------------- *)

(** [project g participant] constructs the local type automaton that captures
    the behaviour of [participant] projected from the global automaton [g].

    Returns [None] when the global protocol is not projectable onto that participant. *)
let project (g : G.graph) (participant : role) : (L.graph, string) result =
  (* Check balance before attempting coinductive projection *)
  (* COMMENTED OUT: Balance check disabled to allow unbalanced types *)
  (*
  if not (Balance.is_balanced g) then
    Error "Global type is not balanced (required for coinductive projection)"
  else
  *)
  try
    let start_kset = compute_initial_knowledge g participant in
    
    (* Debug output *)
    debug_initial_knowledge g participant;

    (* If the participant can never appear, projection yields an empty automaton *)
    if IntSet.is_empty start_kset then
      Ok { L.num_states = 0;
           start_state = None;
           roles = [||];
           kinds = [||] }
    else (
      (* Fresh local state ids *)
      let counter     = ref 0 in
      let fresh_state () =
        let id = !counter in
        if id >= 1000 then raise (Not_projectable "too many local states")
        else (incr counter; id)
      in

      (* Knowledge-set mapping *)
      let state_of_knowledge = ref KnowledgeMap.empty in
      let get_or_alloc_id ks =
        match KnowledgeMap.find_opt ks !state_of_knowledge with
        | Some id -> id
        | None ->
            let id = fresh_state () in
            state_of_knowledge := KnowledgeMap.add ks id !state_of_knowledge;
            id
      in

      (* Reserve arrays with max capacity 1000 *)
      let max_states = 1000 in
      let roles = Array.make max_states participant in
      let kinds = Array.make max_states (L.Snd ("", None)) in (* placeholder *)

      (* Table to track processed knowledge sets *)
      let processed = ref KnowledgeMap.empty in
      let mark_processed ks = processed := KnowledgeMap.add ks () !processed in
      let is_processed ks = KnowledgeMap.mem ks !processed in

      (* Recursive realisation *)
      let rec realise_state ks : int option =
        if IntSet.is_empty ks then None
        else if is_processed ks then KnowledgeMap.find_opt ks !state_of_knowledge
        else (
          match step_knowledge ~g ~participant ~current:ks with
          | Stop -> None
          | SendStep (peer, base, succ_opt) ->
              let id = get_or_alloc_id ks in
              roles.(id) <- peer;
              mark_processed ks;
              let dest_opt = (match succ_opt with
                | None -> None
                | Some ks' -> realise_state ks') in
              kinds.(id) <- L.Snd (base, dest_opt);
              Some id
          | RecvStep (peer, base, succ_opt) ->
              let id = get_or_alloc_id ks in
              roles.(id) <- peer;
              mark_processed ks;
              let dest_opt = (match succ_opt with
                | None -> None
                | Some ks' -> realise_state ks') in
              kinds.(id) <- L.Rcv (base, dest_opt);
              Some id
          | IntStep (peer, branches) ->
              let id = get_or_alloc_id ks in
              roles.(id) <- peer;
              mark_processed ks;
              let branches' =
                List.map (fun (lbl, ks_opt) ->
                  let dest_opt = match ks_opt with
                    | None -> None
                    | Some ks' -> realise_state ks' in
                  (lbl, dest_opt)) branches in
              kinds.(id) <- L.Int branches';
              Some id
          | ExtStep (peer, branches) ->
              let id = get_or_alloc_id ks in
              roles.(id) <- peer;
              mark_processed ks;
              let branches' =
                List.map (fun (lbl, ks_opt) ->
                  let dest_opt = match ks_opt with
                    | None -> None
                    | Some ks' -> realise_state ks' in
                  (lbl, dest_opt)) branches in
              kinds.(id) <- L.Ext branches';
              Some id
        )
      in

      let start_state_opt = realise_state start_kset in

      let num_states = !counter in
      let roles_arr = Array.sub roles 0 num_states in
      let kinds_arr = Array.sub kinds 0 num_states in

      Ok { L.num_states = num_states;
           start_state = start_state_opt;
           roles = roles_arr;
           kinds = kinds_arr }
    )
  with Not_projectable msg -> Error msg




