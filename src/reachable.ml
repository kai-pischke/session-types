open Local_automaton

(* compute the reachble participants from a given state *)
(* first to tarjans algorithm to find the strongly connected components *)
(* then traverse vertices in reverse toplogical order and record the reachble participants *)
let reachable (g : int_graph) : Mask.t array =
  let n = g.num_states in
  if n = 0 then [||] else
  (* Build successor lists *)
  let succ =
    Array.init n (fun i ->
      match g.kinds.(i) with
      | Snd (_, Some j) | Rcv (_, Some j) -> [j]
      | Snd (_, None) | Rcv (_, None) -> []
      | Int alts | Ext alts ->
          List.fold_left (fun acc (_, nxt) ->
            match nxt with None -> acc | Some j -> j :: acc) [] alts)
  in
  (* ----- Tarjan SCC ----- *)
  let index  = Array.make n (-1) in
  let low    = Array.make n 0 in
  let onstk  = Array.make n false in
  let stack  = Stack.create () in
  let next_index = ref 0 in
  let comp_id = Array.make n (-1) in
  let comp_count = ref 0 in

  let rec strongconnect v =
    index.(v) <- !next_index;
    low.(v) <- !next_index;
    incr next_index;
    Stack.push v stack;
    onstk.(v) <- true;

    List.iter (fun w ->
      if index.(w) = -1 then (
        strongconnect w;
        low.(v) <- min low.(v) low.(w)
      ) else if onstk.(w) then
        low.(v) <- min low.(v) index.(w)
    ) succ.(v);

    if low.(v) = index.(v) then (
      (* root of an SCC *)
      let rec pop () =
        let w = Stack.pop stack in
        onstk.(w) <- false;
        comp_id.(w) <- !comp_count;
        if w <> v then pop ()
      in
      pop ();
      incr comp_count
    )
  in
  for v = 0 to n-1 do
    if index.(v) = -1 then strongconnect v
  done;

  let nb_comp = !comp_count in

  (* States per component *)
  let comp_states = Array.make nb_comp [] in
  for v = 0 to n-1 do
    let c = comp_id.(v) in
    comp_states.(c) <- v :: comp_states.(c)
  done;

  (* Condensation graph adjacency + indegrees *)
  let comp_adj = Array.make nb_comp [] in
  for v = 0 to n-1 do
    let c1 = comp_id.(v) in
    List.iter (fun w ->
      let c2 = comp_id.(w) in
      if c1 <> c2 then comp_adj.(c1) <- c2 :: comp_adj.(c1)
    ) succ.(v)
  done;
  let indeg = Array.make nb_comp 0 in
  Array.iter (fun outs ->
    List.iter (fun t -> indeg.(t) <- indeg.(t) + 1) outs
  ) comp_adj;

  (* Kahn topological order *)
  let q = Queue.create () in
  for c = 0 to nb_comp - 1 do
    if indeg.(c) = 0 then Queue.push c q
  done;
  let topo = ref [] in
  while not (Queue.is_empty q) do
    let c = Queue.pop q in
    topo := c :: !topo;
    List.iter (fun t ->
      indeg.(t) <- indeg.(t) - 1;
      if indeg.(t) = 0 then Queue.push t q
    ) comp_adj.(c)
  done;
  (* topo currently reverse topological order *)

  (* Base mask of each component (its own roles) *)
  let comp_base = Array.make nb_comp Mask.empty in
  for c = 0 to nb_comp - 1 do
    List.iter (fun v ->
      comp_base.(c) <- Mask.add g.roles.(v) comp_base.(c)
    ) comp_states.(c)
  done;

  (* DP over reverse topological order (so that successors processed first) *)
  let comp_reach = Array.make nb_comp Mask.empty in
  List.iter (fun c ->
    let m = ref comp_base.(c) in
    List.iter (fun t ->
      m := Mask.union !m comp_reach.(t)
    ) comp_adj.(c);
    comp_reach.(c) <- !m
  ) !topo;

  (* Project back to states *)
  Array.init n (fun v -> comp_reach.(comp_id.(v)))
