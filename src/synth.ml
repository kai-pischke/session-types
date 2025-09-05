(* open Ast
open Local_automaton
open Reachable
open Automaton

(* event: msg or branch *)
type event = 
  | Msg of int * int
  | Branch of int * int * label

(* cfsm[p] = local automaton p *)
type cfsm = int_graph array 

(* configuration[p] = state of local automaton p *)
type configuration = int option array

(* Lazily assigns consecutive ints (0,1,2,…) to configurations as they appear. *)
let make_configuration_indexer () :
  (configuration -> int) * (int -> configuration) * (unit -> int) =
  let tbl : (configuration, int) Hashtbl.t = Hashtbl.create 251 in
  let rev : (int, configuration) Hashtbl.t = Hashtbl.create 251 in
  let next = ref 0 in
  let id_of (cfg : configuration) : int =
    try Hashtbl.find tbl cfg
    with Not_found ->
      let id = !next in
      incr next;
      let key = Array.copy cfg in
      Hashtbl.add tbl key id;
      Hashtbl.add rev id key;
      id
  in
  let cfg_of (id : int) : configuration =
    (* return a copy to keep internal version immutable *)
    Array.copy (Hashtbl.find rev id)
  in
  let size () = !next in
  (id_of, cfg_of, size)

(* Helper to get current state for a role *)
let state_of cfg idx = cfg.(idx)

(* enabled gammma sigma = set of events that are enabled in gammma at configuration sigma *)
let enabled (gamma : cfsm) (sigma : configuration) : event list =
  (* number of roles *)
  let n = Array.length gamma in
  (* list of enabled events *)
  let acc = ref [] in
  (* for each role p *)
  for p = 0 to n - 1 do
    match state_of sigma p with
    | None -> ()  (* terminated role *)
    | Some s1 ->
        let g1 = gamma.(p) in
        (* if the state is valid *)
        if s1 >= 0 && s1 < g1.num_states then
          let k1 = g1.kinds.(s1) in
          let q = g1.roles.(s1) in             (* peer role *)
          (* if the peer role is valid *)
          if q >= 0 && q < n then
            (* get the local automaton of the peer *)
            let g2 = gamma.(q) in
            (* get the state of the peer *)
            match state_of sigma q with
            | None -> ()  (* peer is terminated *)
            | Some s2 ->
                (* if the peer state is valid *)
                if s2 >= 0 && s2 < g2.num_states then
                  let k2 = g2.kinds.(s2) in
                  if g2.roles.(s2) = p then        (* peer must point back *)
                    match k1, k2 with
                    | Snd (b1, _), Rcv (b2, _) when b1 = b2 ->
                        (* msg: p sends to q *)
                        acc := Msg (p, q) :: !acc
                    | Rcv (b1, _), Snd (b2, _) when b1 = b2 ->
                        (* msg: q sends to p *)
                        acc := Msg (q, p) :: !acc
                    | Int lst1, Ext lst2 ->
                        (* all common labels *)
                        List.iter
                          (fun (lab, _) ->
                             if List.mem_assoc lab lst2 then
                               acc := Branch (p, q, lab) :: !acc)
                          lst1
                    | Ext lst1, Int lst2 -> 
                        (* all common labels *)
                        List.iter
                          (fun (lab, _) ->
                             if List.mem_assoc lab lst2 then
                               acc := Branch (q, p, lab) :: !acc)
                          lst1
                    | _ -> ()
  done;
  !acc

(* step gamma sigma ev = new configuration after event ev in configuration sigma in cfsm gamma *)
let step (gamma : cfsm) (sigma : configuration) (ev : event) : configuration =
  let next_choice lbl choices =
    let rec find = function
      | [] -> invalid_arg "step: label not found (event not enabled)"
      | (l, succ) :: tl -> if l = lbl then succ else find tl
    in
    find choices
  in
  let set_next cfg p succ =
    cfg.(p) <- succ
  in
  let sigma' = Array.copy sigma in
  match ev with
  | Msg (p, q) ->
      let sp = sigma.(p) and sq = sigma.(q) in
      begin match sp, sq with
      | Some sp_val, Some sq_val ->
          begin match gamma.(p).kinds.(sp_val), gamma.(q).kinds.(sq_val) with
          | Snd (_, succ_p), Rcv (_, succ_q)
          | Rcv (_, succ_q), Snd (_, succ_p) ->
              set_next sigma' p succ_p;
              set_next sigma' q succ_q
          | _ -> invalid_arg "step: Msg not enabled"
          end
      | _ -> invalid_arg "step: Msg not enabled"
      end;
      sigma'
  | Branch (p, q, l) ->
      let sp = sigma.(p) and sq = sigma.(q) in
      begin match sp, sq with
      | Some sp_val, Some sq_val ->
          begin match gamma.(p).kinds.(sp_val), gamma.(q).kinds.(sq_val) with
          | Int choices_p, Ext choices_q ->
              let succ_p = next_choice l choices_p in
              let succ_q = next_choice l choices_q in
              set_next sigma' p succ_p;
              set_next sigma' q succ_q
          | Ext choices_q, Int choices_p ->
              let succ_p = next_choice l choices_p in
              let succ_q = next_choice l choices_q in
              set_next sigma' p succ_p;
              set_next sigma' q succ_q
          | _ -> invalid_arg "step: Branch not enabled"
          end
      | _ -> invalid_arg "step: Branch not enabled"
      end;
      sigma'

(* adj.(p).(q) = true iff p ∈ reach[q][sigma[q]]. *)
let build_adj (reach : Mask.t array array) (sigma : int option array) : bool array array =
  let n = Array.length reach in
  Array.init n (fun p ->
    Array.init n (fun q ->
      match sigma.(q) with
      | None -> false  (* terminated role has no reachable states *)
      | Some sq -> Mask.mem p reach.(q).(sq)
    ))

(* Return: comp_of.(v) = component id in [0..comp_count), and comp_count. *)
let components (adj : bool array array) : int array * int =
  let n = Array.length adj in
  let comp_of = Array.make n (-1) in
  let comp_count = ref 0 in
  let q = Queue.create () in
  for s = 0 to n - 1 do
    if comp_of.(s) = -1 then begin
      let cid = !comp_count in
      incr comp_count;
      comp_of.(s) <- cid;
      Queue.clear q; Queue.push s q;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        for v = 0 to n - 1 do
          if adj.(u).(v) && comp_of.(v) = -1 then begin
            comp_of.(v) <- cid;
            Queue.push v q
          end
        done
      done
    end
  done;
  (comp_of, !comp_count)

(* decompose sigma into components *)
let decompose_sigma (sigma : int option array) (adj : bool array array) (id_of : configuration -> int) : IntSet.t =
  let comp_of, comp_count = components adj in
  let n = Array.length sigma in
  let temp_set = ref IntSet.empty in
  for comp = 0 to comp_count - 1 do
    let sigma_comp = Array.make n None in
    for p = 0 to n - 1 do
      if comp_of.(p) = comp then
        sigma_comp.(p) <- sigma.(p)
    done;
    let sigma_comp_id = id_of sigma_comp in
    temp_set := IntSet.add sigma_comp_id !temp_set;
  done;
  !temp_set

(* dynamically resized array of configuration states *)
let make_configuration_array () : state_kind array * ((int * int) array) * (int -> int -> state_kind -> unit) =
  let kinds = ref [||] in
  let roles = ref [||] in

  let ensure_capacity (cfg_id : int) =
    let capacity = Array.length !kinds in
    if cfg_id >= capacity then begin
      let new_capacity = max 4 (capacity * 2) in
      kinds := Array.append !kinds (Array.make (new_capacity - capacity) (Msg ("", IntSet.empty)));
      roles := Array.append !roles (Array.make (new_capacity - capacity) (-1, -1))
    end
  in

  let add_state (cfg_id : int) (roles : int * int) (kind : state_kind) : unit =
    ensure_capacity cfg_id;
    !kinds.(cfg_id) <- kind;
    !roles.(cfg_id) <- roles
  in 
  (!kinds, !roles, add_state)

(* initial_configuration gamma = initial configuration of cfsm gamma *)
let initial_configuration (gamma : cfsm) : configuration =
  Array.map
    (fun g ->
        match g.start_state with
        | Some s when s >= 0 && s < g.num_states -> Some s
        | _ -> None)   (* empty automaton or invalid -> terminated state *)
    gamma

let synth (gamma : cfsm) : graph =
  (* number of roles *)
  let n = Array.length gamma in
  (* map configurations to ints *)
  let id_of, _cfg_of, size = make_configuration_indexer () in 
  (* initial configuration *)
  let sigma = initial_configuration gamma in
  (* precompute reach sets *)
  let reach = Array.init n (fun p -> reachable gamma.(p)) in
  (* adj.(p).(q) = true iff p ∈ reach.(q).(sigma.(q)) *)
  let adj = build_adj reach sigma in
  (* decompose sigma into components *)
  let start_set = decompose_sigma sigma adj id_of in
  (* dynamically resized array of configuration states *)
  let kinds_arr, add_kind = make_configuration_array () in
  
  (* enqueue all start configurations *)
  let q = Queue.create () in
  IntSet.iter (fun sigma_id -> Queue.push sigma_id q) start_set;

  let visited = ref IntSet.empty in

  (* bfs from start_set *)
  while not (Queue.is_empty q) do
      let sigma_id = Queue.pop q in
      if IntSet.mem sigma_id !visited then ()
      else
        visited := IntSet.add sigma_id !visited;
        let sigma = cfg_of sigma_id in
        let enabled_events = enabled gamma sigma in
        match enabled_events with
        | [] -> ()
        | Msg (p, q) :: _ -> 
            let next_sigma = step gamma sigma (Msg (p, q)) in
            let next_sigma_id = id_of next_sigma in
            let new_adj = build_adj reach next_sigma in
            let split_sigma = decompose_sigma next_sigma new_adj id_of in
            add_state sigma_id (p, q) split_sigma;
            IntSet.iter (fun sigma_id -> Queue.push sigma_id q) split_sigma;
        | Branch (p, q, l) :: _ ->
            let branches = List.filter (fun ev -> match ev with Branch (p', q', l') -> p' = p && q' = q && l' = l) enabled_events in
            add_kind sigma_id (p, q) (Branch);
            let split_sigmas = ref [] in
            List.iter (fun (Branch (_, _, l)) ->
              let next_sigma = step gamma sigma ev in
              let next_sigma_id = id_of next_sigma in
              let new_adj = build_adj reach next_sigma in
              let split_sigma = decompose_sigma next_sigma new_adj id_of in
              split_sigmas := (l, split_sigma) :: !split_sigmas;
              IntSet.iter (fun sigma_id -> Queue.push sigma_id q) split_sigma
            ) branches;
            add_state sigma_id (p, q) !split_sigmas
  done;

  (* TODO: Implement the actual synthesis algorithm *)
  (* For now, return an empty automaton to fix the syntax error *)
  {
    num_states = size ();
    start_states = start_set;
    roles = Array.init n (fun p -> p);
    kinds = [||];
  }

   *)