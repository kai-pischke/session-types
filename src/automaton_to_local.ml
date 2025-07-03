open Ast
open Local_automaton

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

(* ------------------------------------------------------------------ *)
(* Strongly-connected components                                      *)
(* ------------------------------------------------------------------ *)

(* Returns for each state its component id and a table telling if that
   component is genuinely recursive (size >1 or self-loop). *)
let compute_sccs (g : graph) : int array * bool array =
  let index = ref 0 in
  let stack = Stack.create () in
  let on_stack = Array.make g.num_states false in
  let indices = Array.make g.num_states (-1) in
  let lowlink = Array.make g.num_states (-1) in

  (* Will grow lazily since we don't know number of comps in advance *)
  let comp_of_state = Array.make g.num_states (-1) in
  let comp_rec = ref [] in               (* (id -> is_recursive) list *)
  let comp_counter = ref 0 in

  let mark_comp nodes =
    let comp_id = !comp_counter in
    incr comp_counter;
    (* Determine if this component is recursive *)
    let is_rec =
      match nodes with
      | [single] -> (
          match g.kinds.(single) with
          | Snd (_, Some dst)
          | Rcv (_, Some dst) when dst = single -> true
          | Int brs | Ext brs when List.exists (fun (_,d) -> d = Some single) brs -> true
          | _ -> false)
      | _ -> true
    in
    List.iter (fun st -> comp_of_state.(st) <- comp_id) nodes;
    comp_rec := (comp_id, is_rec) :: !comp_rec
  in

  let rec strongconnect v =
    indices.(v) <- !index;
    lowlink.(v) <- !index;
    incr index;
    Stack.push v stack; on_stack.(v) <- true;

    let successors =
      match g.kinds.(v) with
      | Snd (_, dst_opt) | Rcv (_, dst_opt) -> (match dst_opt with Some d -> [d] | None -> [])
      | Int brs | Ext brs -> List.filter_map (fun (_,d) -> d) brs
    in
    List.iter (fun w ->
      if indices.(w) = -1 then (
        strongconnect w;
        lowlink.(v) <- min lowlink.(v) lowlink.(w)
      ) else if on_stack.(w) then
        lowlink.(v) <- min lowlink.(v) indices.(w)
    ) successors;

    if lowlink.(v) = indices.(v) then (
      (* Pop *)
      let comp_nodes = ref [] in
      let continue = ref true in
      while !continue do
        let w = Stack.pop stack in
        on_stack.(w) <- false;
        comp_nodes := w :: !comp_nodes;
        if w = v then continue := false
      done;
      mark_comp !comp_nodes)
  in

  for v = 0 to g.num_states - 1 do
    if indices.(v) = -1 then strongconnect v
  done;

  let max_comp = !comp_counter in
  let rec_flags = Array.make max_comp false in
  List.iter (fun (cid, isrec) -> rec_flags.(cid) <- isrec) !comp_rec;
  (comp_of_state, rec_flags)

(* ------------------------------------------------------------------ *)
(* Tarjan SCC to find recursive states                                  *)
(* ------------------------------------------------------------------ *)
let compute_recursive_states (g : graph) : IntSet.t =
  let index = ref 0 in
  let stack = Stack.create () in
  let on_stack = Array.make g.num_states false in
  let indices = Array.make g.num_states (-1) in
  let lowlink = Array.make g.num_states (-1) in
  let rec_components = ref IntSet.empty in
  let add_component comp =
    (* Any SCC with >1 node or self-loop counts as recursive *)
    match comp with
    | [ single ] ->
        (match g.kinds.(single) with
         | Snd (_, Some dst)
         | Rcv (_, Some dst) when dst = single ->
             rec_components := IntSet.add single !rec_components
         | Int brs | Ext brs when List.exists (fun (_,d) -> d = Some single) brs ->
             rec_components := IntSet.add single !rec_components
         | _ -> ())
    | lst -> List.iter (fun n -> rec_components := IntSet.add n !rec_components) lst
  in
  let rec strongconnect v =
    indices.(v) <- !index;
    lowlink.(v) <- !index;
    incr index;
    Stack.push v stack; on_stack.(v) <- true;
    let successors =
      match g.kinds.(v) with
      | Snd (_, dst_opt) | Rcv (_, dst_opt) ->
          (match dst_opt with Some d -> [d] | None -> [])
      | Int brs | Ext brs ->
          List.filter_map (fun (_,dopt) -> dopt) brs
    in
    List.iter (fun w ->
      if indices.(w) = -1 then (
        strongconnect w;
        lowlink.(v) <- min lowlink.(v) lowlink.(w)
      ) else if on_stack.(w) then
        lowlink.(v) <- min lowlink.(v) indices.(w)
    ) successors;
    if lowlink.(v) = indices.(v) then (
      (* Pop stack until v *)
      let comp = ref [] in
      let continue = ref true in
      while !continue do
        let w = Stack.pop stack in
        on_stack.(w) <- false;
        comp := w :: !comp;
        if w = v then continue := false
      done;
      add_component !comp)
  in
  for v = 0 to g.num_states - 1 do
    if indices.(v) = -1 then strongconnect v
  done;
  !rec_components

(* ------------------------------------------------------------------ *)
(* Conversion                                                         *)
(* ------------------------------------------------------------------ *)

let automaton_to_local (g : graph) : int local =
  match g.start_state with
  | None -> LEnd Loc.dummy
  | Some start ->
      let comp_id, comp_is_rec = compute_sccs g in

      let env = ref IntMap.empty in     (* state_id -> int local already built *)
      let comp_rep = ref IntMap.empty in (* comp_id -> representative state *)
      let in_stack = ref IntSet.empty in
      
      let rec convert_state (st : int) : int local =
        (* memoisation *)
        match IntMap.find_opt st !env with
        | Some l -> l
        | None ->
            let comp = comp_id.(st) in
            let is_recursive = comp_is_rec.(comp) in

            let result : int local =
              if is_recursive then (
                match IntMap.find_opt comp !comp_rep with
                | Some rep_state when IntSet.mem st !in_stack ->
                    (* We came back to a state currently being expanded in this component *)
                    let lvar = LVar (rep_state, Loc.dummy) in
                    env := IntMap.add st lvar !env; lvar
                | Some rep_state ->
                    (* Component already has a representative, but this particular state
                       has its own behaviour. We tie the knot with a placeholder to avoid
                       cycles, then build its body. *)
                    let placeholder = LVar (rep_state, Loc.dummy) in
                    env := IntMap.add st placeholder !env;
                    in_stack := IntSet.add st !in_stack;
                    let body = convert_body st in
                    in_stack := IntSet.remove st !in_stack;
                    env := IntMap.add st body !env;
                    body
                | None ->
                    (* First encounter of this component -> create binder *)
                    comp_rep := IntMap.add comp st !comp_rep;
                    let var = st in
                    env := IntMap.add st (LVar (var, Loc.dummy)) !env;
                    in_stack := IntSet.add st !in_stack;
                    let body = convert_body st in
                    in_stack := IntSet.remove st !in_stack;
                    let rec_node = LRec (var, body, Loc.dummy) in
                    env := IntMap.add st rec_node !env;
                    rec_node
              ) else
                convert_body st
            in
            result
      
      and convert_body st : int local =
        match g.kinds.(st) with
        | Snd (base, dst_opt) ->
            let cont = (match dst_opt with Some d -> convert_state d | None -> LEnd Loc.dummy) in
            LSend (g.roles.(st), base, cont, Loc.dummy)
        | Rcv (base, dst_opt) ->
            let cont = (match dst_opt with Some d -> convert_state d | None -> LEnd Loc.dummy) in
            LRecv (g.roles.(st), base, cont, Loc.dummy)
        | Int branches ->
            let brs = List.map (fun (lbl, dopt) ->
              let cont = match dopt with Some d -> convert_state d | None -> LEnd Loc.dummy in
              (lbl, cont)
            ) branches in
            LInt (g.roles.(st), brs, Loc.dummy)
        | Ext branches ->
            let brs = List.map (fun (lbl, dopt) ->
              let cont = match dopt with Some d -> convert_state d | None -> LEnd Loc.dummy in
              (lbl, cont)
            ) branches in
            LExt (g.roles.(st), brs, Loc.dummy)
      in
      convert_state start 