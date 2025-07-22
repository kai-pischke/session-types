(*
   Global-type synthesis from a family of local automata
   ------------------------------------------------------
   Given one local automaton per participant we attempt to rebuild a
   *global* automaton that realises the same behaviours.  This module
   only provides the *infrastructure* – reach-set analysis and helpers
   for the forthcoming product exploration.  The actual synthesis
   (construction of states / transitions of the global automaton) will
   be added incrementally.

     Notation
   ---------
   • Each participant P ∈ Roles comes with its own local automaton Lᴾ.
   • A *configuration* is a tuple of local-state identifiers, one for
     every participant.
   • A *communication event* is the synchronised pair of a send state in
     participant P and the matching receive state in participant Q that
     share the same base.
   • Two events are *independent* (parallel) when the sets of
     participants that might still interact with them in the future are
     disjoint.  We approximate those future sets by a pre-computed
     reachability analysis below.
*)

open Ast
module LA = Local_automaton
module GA = Automaton

module StringSet = Set.Make (String)
module IntSet    = Set.Make (Int)
module StringMap = Map.Make(String)

(* ------------------------------------------------------------------ *)
(* Input bundle                                                        *)
(* ------------------------------------------------------------------ *)

(* A local automaton together with the identity of its owner role. *)
type local_participant = {
  role : role;           (* the participant owning this automaton *)
  laut : LA.graph;       (* its local behaviour graph             *)
}

(* The full family of locals we are asked to combine. *)
type local_family = local_participant list

(* ------------------------------------------------------------------ *)
(* Role universe & indexing                                            *)
(* ------------------------------------------------------------------ *)

(* All roles that appear either *as owner* or *as peer* anywhere in
   the family.  We give them consecutive indices 0..k-1 so that bitmask
   operations are easy. *)

module RoleIndex = struct
  type t = {
    to_index : int StringMap.t;  (* role -> idx *)
    of_index : string array;     (* idx  -> role *)
  }

  let build (locals : local_family) : t =
    (* collect all roles *)
    let roles_set = ref StringSet.empty in
    List.iter (fun { role = owner; laut } ->
        roles_set := StringSet.add owner !roles_set;
        Array.iter (fun peer -> roles_set := StringSet.add peer !roles_set)
          laut.roles) locals;
    let roles = StringSet.elements !roles_set in
    let to_index = ref StringMap.empty in
    List.iteri (fun idx r -> to_index := StringMap.add r idx !to_index) roles;
    { to_index = !to_index; of_index = Array.of_list roles }

  let index_of t r = StringMap.find r t.to_index
  let role_of  t i = t.of_index.(i)
end

(* For small role sets (<64) we can use an int64 as bitmask *)
module Mask = struct
  type t = Int64.t  (* bit i set iff role i is in the set *)
  let empty = 0L
  let add idx m = Int64.logor m (Int64.shift_left 1L idx)
  let union = Int64.logor
  let inter = Int64.logand
  let is_empty m = (m = 0L)
end

(* ------------------------------------------------------------------ *)
(* Future-partner analysis                                             *)
(* ------------------------------------------------------------------ *)

(* For every state s of every local automaton we compute a bit-mask of
   "reachable":  roles that the owner may still communicate with
   when control is in s (including the immediate peer of the outgoing
   transition, if any).
*)
(* (compute_reach_masks idx lp)[i][q] = participant q reachable *) 
(* from role lp.role from state i of its local automaton *)
let compute_reach_masks (index : RoleIndex.t) (lp : local_participant)
    : Mask.t array =
  (* Local automaton *)
  let l = lp.laut in
  (* Number of states *)
  let n = l.num_states in

  (* create array *)
  let reach = Array.make n Mask.empty in

  (* Helper functions *)
  let add_mask st m = reach.(st) <- Mask.union m reach.(st) in
  let add_role st r = add_mask st (Mask.add (RoleIndex.index_of index r) Mask.empty) in

  (* initialise array *)
  for st = 0 to n - 1 do
    add_role st lp.role; (* add owner *)
    add_role st l.roles.(st); (* add peer *)
  done;

  (* update reachability masks *)
  let changed = ref true in
  while !changed do
    changed := false;
      for st = 0 to n - 1 do
      let old = reach.(st) in
      let succs =
        match l.kinds.(st) with
        | LA.Snd (_base, dst_opt) ->
            (match dst_opt with Some d -> [d] | None -> [])
        | LA.Rcv (_base, dst_opt) ->
            (match dst_opt with Some d -> [d] | None -> [])
        | LA.Int branches | LA.Ext branches ->
            List.filter_map (fun (_, dst_opt) -> dst_opt) branches
      in
      List.iter (fun succ -> add_mask st (reach.(succ))) succs;
      if reach.(st) <> old then changed := true
    done
  done;
  reach

(* Bundle reach masks for the whole family *)
(* reach_tbl[p][i][q] = participant q reachable from role p from state i of its local automaton *)
let build_reach_table (index : RoleIndex.t) (locals : local_family)
    : Mask.t array array =
  (* Array indexed by role index; each entry is the reach-mask array for that role. *)
  let num_roles = Array.length index.of_index in
  let tbl = Array.make num_roles [||] in
  List.iter (fun lp ->
      let idx = RoleIndex.index_of index lp.role in
      tbl.(idx) <- compute_reach_masks index lp)
    locals;
  tbl

(* ------------------------------------------------------------------ *)
(* Event description & independence test                               *)
(* ------------------------------------------------------------------ *)

type event = {
  sender     : role;
  receiver   : role;
  base       : base;
  s_state    : int;   (* state id in sender automaton *)
  r_state    : int;   (* state id in receiver automaton *)
}

(* Hash-table key for events: (sender,receiver,base,s_state,r_state) *)
module EventKey = struct
  type t = event
  let hash ev =
    Hashtbl.hash (ev.sender, ev.receiver, ev.base, ev.s_state, ev.r_state)
  let equal a b =
    a.s_state = b.s_state && a.r_state = b.r_state &&
    String.equal a.sender b.sender && String.equal a.receiver b.receiver &&
    String.equal a.base b.base
end

module EventTbl = Hashtbl.Make(EventKey)

(* Build the full mask (participants + future partners) of an event *)
let mask_of_event (index : RoleIndex.t)
    ~(reach_tbl : Mask.t array array)
    (ev : event) : Mask.t =
  let open Mask in
  let idx_sender = RoleIndex.index_of index ev.sender in
  let idx_receiver = RoleIndex.index_of index ev.receiver in
  let m1 = reach_tbl.(idx_sender).(ev.s_state) in
  let m2 = reach_tbl.(idx_receiver).(ev.r_state) in
  union m1 m2

(* Independence predicate *)
let independent index reach_tbl ev1 ev2 =
  let open Mask in
  is_empty (inter (mask_of_event index ~reach_tbl ev1)
                    (mask_of_event index ~reach_tbl ev2))

(* ------------------------------------------------------------------ *)
(* Placeholder for the forthcoming synthesis                           *)
(* ------------------------------------------------------------------ *)

(* We expose an [initialise] function that computes all static data that
   the real synthesiser will need.                                    *)

type precomputed = {
  roles      : RoleIndex.t;
  reach_tbl  : Mask.t array array;
}

let precompute (locals : local_family) : precomputed =
  let roles = RoleIndex.build locals in
  let reach_tbl = build_reach_table roles locals in
  { roles; reach_tbl }

(* ------------------------------------------------------------------ *)
(* Product-state representation                                        *)
(* ------------------------------------------------------------------ *)

(* For exploration we keep participants in index order 0..k-1 *)

(* configuration[p] = state id of participant p *)
type configuration = int option array  (* length = |roles|; None = terminated *)

(* Helper to get current state id for a role *)
let state_of cfg idx = cfg.(idx)

(* Pointer array of locals in role-index order *)
let build_local_array (roles:RoleIndex.t) (locals:local_family) : local_participant array =
  let empty_la = { LA.num_states = 0; start_state = None; roles = [||]; kinds = [||] } in
  let arr = Array.init (Array.length roles.of_index) (fun _ -> { role=""; laut = empty_la }) in
  List.iter (fun lp ->
    let idx = RoleIndex.index_of roles lp.role in
    arr.(idx) <- lp) locals;
  arr

(* Build initial configuration *)
let initial_config (locals_arr: local_participant array) : configuration =
  Array.map (fun lp -> lp.laut.start_state) locals_arr

(* Enumerate enabled message events in a configuration *)
let enabled_events (roles:RoleIndex.t)
    (locals_arr:local_participant array)
    (cfg:configuration) : event list =
  let events = ref [] in
  Array.iteri (fun idx lp_sender ->
      match state_of cfg idx with
      | None -> ()
      | Some st_s ->
          if st_s < lp_sender.laut.num_states then
            match lp_sender.laut.kinds.(st_s) with
            | LA.Snd (_base as base, _) ->
                let peer_role = lp_sender.laut.roles.(st_s) in
                (* find peer index *)
                (try
                   let peer_idx = RoleIndex.index_of roles peer_role in
                   let lp_recv = locals_arr.(peer_idx) in
                   (match state_of cfg peer_idx with
                    | Some st_r ->
                        if st_r < lp_recv.laut.num_states then
                          (match lp_recv.laut.kinds.(st_r) with
                           | LA.Rcv (_b2, _) when String.equal lp_recv.laut.roles.(st_r) lp_sender.role && String.equal base _b2 ->
                               let ev = { sender=lp_sender.role; receiver=peer_role; base; s_state=st_s; r_state=st_r } in
                               events := ev :: !events
                           | _ -> ())
                    | None -> ())
                with Not_found -> ())
            | _ -> ()
  ) locals_arr;
  List.rev !events 

(* The actual synthesis *)
let synthesise (locals : local_family) : (GA.graph, string) result = 
  (* ------------------------------------------------------------------ *)
  (*  High-level overview                                                *)
  (* ------------------------------------------------------------------ *)
  (***
     We explore the synchronous product of the *local* automata to build a
     *global* message automaton.  Each *communication event* discovered
     during the exploration becomes a *state* in the resulting global graph
     (GA.Msg).  The exploration is done lazily:
       • we start from the initial configuration (tuple of local states)
       • at every configuration we enumerate all enabled events
       • firing an event moves the two involved local automata to their
         respective successor states, yielding a new configuration

     Invariants maintained during the construction
     ---------------------------------------------
     I1  A unique state-identifier is assigned to every *event* tuple
         (sender,receiver,base,s_state,r_state).
     I2  [succs.(sid)] contains exactly the identifiers of *dependent* events
         that may follow the event represented by [sid].  Two events are
         considered dependent when the helper [independent] predicate returns
         [false].  Independent events are left unordered – this realises the
         required *parallel* semantics for the unit-tests (independent events
         become separate start states with no incoming/outgoing edges).

     Post-conditions of [synthesise]
     --------------------------------
     • `Ok g`
         – [g.num_states] is the number of distinct communication events that
           can occur.
         – [g.start_states] contains all events enabled in the initial
           configuration *and* not preceded by a dependent event.
         – For every state `sid` the pair [g.roles.(sid)] is the (sender,receiver)
           of the corresponding event and [g.kinds.(sid)] is
           `GA.Msg (base, succs)` with `succs` satisfying I2.
     • `Error msg` on inconsistent input (currently only when no event is
       enabled initially).
  ***)
  try
    (* Static pre-computations ------------------------------------------------ *)
    let pc = precompute locals in
    let roles_idx   = pc.roles in
    let reach_tbl   = pc.reach_tbl in
    let locals_arr  = build_local_array roles_idx locals in
    let init_cfg    = initial_config locals_arr in

    (* Helpers ---------------------------------------------------------------- *)

    let event_to_sid : int EventTbl.t = EventTbl.create 17 in
    (* reverse lookup: sid -> event *)
    let sid_to_event : event array ref = ref [||] in

    let next_sid    = ref 0 in
    let succs       = ref (Array.make 0 GA.IntSet.empty) in
    let bases       = ref (Array.make 0 "") in
    let roles_arr   = ref (Array.make 0 ("","")) in

    let ensure_capacity () =
      let needed = !next_sid in
      let cur = Array.length !succs in
      if needed >= cur then begin
        let new_len = max 4 (cur * 2) in
        succs     := Array.append !succs     (Array.make (new_len - cur) GA.IntSet.empty);
        bases     := Array.append !bases     (Array.make (new_len - cur) "");
        roles_arr := Array.append !roles_arr (Array.make (new_len - cur) ("",""));
        sid_to_event := Array.append !sid_to_event (Array.make (new_len - cur) {
            sender=""; receiver=""; base=""; s_state=0; r_state=0 })
      end
    in

    let get_sid (ev:event) : int =
      match EventTbl.find_opt event_to_sid ev with
      | Some sid -> sid
      | None ->
          let sid = !next_sid in
          incr next_sid;
          ensure_capacity();
          (* fill metadata *)
          (!bases).(sid)     <- ev.base;
          (!roles_arr).(sid) <- (ev.sender, ev.receiver);
          (!sid_to_event).(sid) <- ev;
          (* store *)
          EventTbl.add event_to_sid ev sid;
          sid
    in

    (* Mutation helpers for successor sets *)
    let add_succ ~from_sid ~to_sid =
      (!succs).(from_sid) <- GA.IntSet.add to_sid (!succs).(from_sid)
    in

    (* Apply an event to a configuration, yielding the successor configuration *)
    let apply_event (cfg:configuration) (ev:event) : configuration =
      let cfg' = Array.copy cfg in
      let sender_idx   = RoleIndex.index_of roles_idx ev.sender in
      let receiver_idx = RoleIndex.index_of roles_idx ev.receiver in
      (* advance sender *)
      (match locals_arr.(sender_idx).laut.kinds.(ev.s_state) with
       | LA.Snd (_b, dst_opt) -> cfg'.(sender_idx) <- dst_opt
       | _ -> ());
      (* advance receiver *)
      (match locals_arr.(receiver_idx).laut.kinds.(ev.r_state) with
       | LA.Rcv (_b, dst_opt) -> cfg'.(receiver_idx) <- dst_opt
       | _ -> ());
      cfg'
    in

    (* BFS/DFS product exploration ------------------------------------------- *)
    let queue : (configuration * int option) Queue.t = Queue.create () in
    let visited : (configuration * int option, unit) Hashtbl.t = Hashtbl.create 97 in

    let push cfg prev_opt =
      if not (Hashtbl.mem visited (cfg, prev_opt)) then begin
        Hashtbl.add visited (cfg, prev_opt) (); Queue.push (cfg, prev_opt) queue
      end
    in

    push init_cfg None;

    let start_states = ref GA.IntSet.empty in

    while not (Queue.is_empty queue) do
      let cfg, prev_opt = Queue.pop queue in
      let evs = enabled_events roles_idx locals_arr cfg in
      List.iter (fun ev ->
        let sid = get_sid ev in
        (* predecessor linkage *)
        (match prev_opt with
         | None -> start_states := GA.IntSet.add sid !start_states  (* initial events *)
         | Some prev_sid ->
             let prev_ev = (!sid_to_event).(prev_sid) in
             if not (independent roles_idx reach_tbl ev prev_ev) then
               add_succ ~from_sid:prev_sid ~to_sid:sid);
        (* successor configuration and further exploration *)
        let cfg' = apply_event cfg ev in
        push cfg' (Some sid)
      ) evs
    done;

    (* Sanity check *)
    if GA.IntSet.is_empty !start_states then
      Error "no enabled events – nothing to synthesise"
    else
      (* Convert collected arrays to the right size *)
      let n = !next_sid in
      let roles_fin = Array.sub !roles_arr 0 n in
      let kinds_fin = Array.init n (fun i -> GA.Msg ((!bases).(i), (!succs).(i))) in
      let graph : GA.graph = {
        num_states   = n;
        start_states = !start_states;
        roles        = roles_fin;
        kinds        = kinds_fin;
      } in
      Ok graph
  with exn ->
    Error (Printexc.to_string exn) 
  