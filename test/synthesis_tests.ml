(*--------------------------------------------------------------------*)
(*  Unit‑tests for the synthesis module                                *)
(*--------------------------------------------------------------------*)
open Test_helpers

let synthesis_tests =
  let open Synthesis in
  let build_la lt = Local_automaton.of_local lt in
  
  let test_independence_conflict () =
    (* p -> q and r -> q share future partner q, hence not independent *)
    let p_la = build_la (send "q" "Int" mk_end) in
    let q_la = build_la (recv "p" "Int" mk_end) in
    let r_la = build_la (send "q" "Int" mk_end) in
    let locals = [ { Synthesis.role="p"; laut=p_la };
                   { role="q"; laut=q_la };
                   { role="r"; laut=r_la } ]
    in
    let pc = precompute locals in
    let reach_tbl = pc.reach_tbl in
    let idx = pc.roles in
    let ev1 = { sender="p"; receiver="q"; base="Int"; s_state=0; r_state=0 } in
    let ev2 = { sender="r"; receiver="q"; base="Int"; s_state=0; r_state=0 } in
    Alcotest.(check bool) "dependent" false (independent idx reach_tbl ev1 ev2)
  in
  
  let test_independence_ok () =
    (* p->q and r->s are independent *)
    let p_la = build_la (send "q" "Int" mk_end) in
    let q_la = build_la (recv "p" "Int" mk_end) in
    let r_la = build_la (send "s" "Bool" mk_end) in
    let s_la = build_la (recv "r" "Bool" mk_end) in
    let locals = [ { role="p"; laut=p_la };
                   { role="q"; laut=q_la };
                   { role="r"; laut=r_la };
                   { role="s"; laut=s_la } ]
    in
    let pc = precompute locals in
    let idx = pc.roles in
    let reach_tbl = pc.reach_tbl in
    let ev1 = { sender="p"; receiver="q"; base="Int"; s_state=0; r_state=0 } in
    let ev2 = { sender="r"; receiver="s"; base="Bool"; s_state=0; r_state=0 } in
    Alcotest.(check bool) "independent" true (independent idx reach_tbl ev1 ev2)
  in

  (* Simple one-step protocol p -> q [Int] *)
  let test_simple_seq () =
    let p_local = send "q" "Int" mk_end in
    let q_local = recv "p" "Int" mk_end in
    let locals = [ build_part "p" p_local; build_part "q" q_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        Alcotest.(check int) "num_states" 1 g.num_states;
        let start = Automaton.IntSet.elements g.start_states in
        Alcotest.(check (list int)) "start" [0] start;
        (match g.kinds.(0) with
         | Automaton.Msg (base, dsts) ->
             Alcotest.(check string) "base" "Int" base;
             Alcotest.(check (list int)) "succ" [] (Automaton.IntSet.elements dsts)
         | _ -> Alcotest.fail "state0 not Msg");
        (match g.roles.(0) with
         | ("p","q") -> ()
         | _ -> Alcotest.fail "roles wrong")
  in

  (* Two-step ping-pong: p->q Int ; q->p Bool *)
  let test_two_step () =
    let p_local = send "q" "Int" (recv "q" "Bool" mk_end) in
    let q_local = recv "p" "Int" (send "p" "Bool" mk_end) in
    let locals = [ build_part "p" p_local; build_part "q" q_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        Alcotest.(check int) "num_states" 2 g.num_states;
        let expect_msg idx base dst =
          match g.kinds.(idx) with
          | Automaton.Msg (b,d) ->
              Alcotest.(check string) (Printf.sprintf "base %d" idx) base b;
              Alcotest.(check (list int)) "succ" dst (Automaton.IntSet.elements d)
          | _ -> Alcotest.failf "state %d not Msg" idx
        in
        expect_msg 0 "Int" [1];
        expect_msg 1 "Bool" [];
        ()
  in

  (* Simple recursive protocol: p -> q [Int]; t *)
  let test_recursive () =
    let p_local = Ast.LRec (0, send "q" "Int" (Ast.LVar (0, Loc.dummy)), Loc.dummy) in
    let q_local = Ast.LRec (0, recv "p" "Int" (Ast.LVar (0, Loc.dummy)), Loc.dummy) in
    let locals = [ build_part "p" p_local; build_part "q" q_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        Alcotest.(check int) "num_states" 1 g.num_states;
        let start = Automaton.IntSet.elements g.start_states in
        Alcotest.(check (list int)) "start" [0] start;
        (match g.kinds.(0) with
         | Automaton.Msg (base, dsts) ->
             Alcotest.(check string) "base" "Int" base;
             Alcotest.(check (list int)) "succ" [0] (Automaton.IntSet.elements dsts)
         | _ -> Alcotest.fail "state0 not Msg");
        (match g.roles.(0) with
         | ("p","q") -> ()
         | _ -> Alcotest.fail "roles wrong")
  in

  (* Parallel independent protocols: p->q [Int] | r->s [Bool] *)
  let test_parallel_independent () =
    let p_local = send "q" "Int" mk_end in
    let q_local = recv "p" "Int" mk_end in
    let r_local = send "s" "Bool" mk_end in
    let s_local = recv "r" "Bool" mk_end in
    let locals = [ build_part "p" p_local; build_part "q" q_local; 
                   build_part "r" r_local; build_part "s" s_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        (* Should have 2 states *)
        Alcotest.(check int) "num_states" 2 g.num_states;
        let start = Automaton.IntSet.elements g.start_states in
        Alcotest.(check (list int)) "start" [0;1] start;
        (* Check that both messages are enabled from the initial state *)
        (* The synthesizer should choose one of the independent events *)
        (match g.kinds.(0) with
         | Automaton.Msg (base, dsts) ->
             (* Should be either Int or Bool *)
             Alcotest.(check bool) "base is Int or Bool" 
               (base = "Int" || base = "Bool") true;
             (* Should go to end*)
             Alcotest.(check (list int)) "succ" [] (Automaton.IntSet.elements dsts)
         | _ -> Alcotest.fail "state0 not Msg");
        (* Check that the second message is in state 1 *)
        (match g.kinds.(1) with
         | Automaton.Msg (base, dsts) ->
             (* Should be the other base type *)
             Alcotest.(check bool) "base is Int or Bool" 
               (base = "Int" || base = "Bool") true;
             (* Should go to end *)
             Alcotest.(check (list int)) "succ" [] (Automaton.IntSet.elements dsts)
         | _ -> Alcotest.fail "state1 not Msg")
  in

  (* Three independent protocols: p->q Int | r->s Bool | t->u String *)
  let test_parallel_three () =
    let p_local = send "q" "Int" mk_end in
    let q_local = recv "p" "Int" mk_end in
    let r_local = send "s" "Bool" mk_end in
    let s_local = recv "r" "Bool" mk_end in
    let t_local = send "u" "String" mk_end in
    let u_local = recv "t" "String" mk_end in
    let locals = [ build_part "p" p_local; build_part "q" q_local;
                   build_part "r" r_local; build_part "s" s_local;
                   build_part "t" t_local; build_part "u" u_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        (* Expect 3 message states *)
        Alcotest.(check int) "num_states" 3 g.num_states;
        let starts = Automaton.IntSet.elements g.start_states in
        Alcotest.(check int) "three starts" 3 (List.length starts);
        List.iter (fun idx ->
          match g.kinds.(idx) with
          | Automaton.Msg (b,d) ->
              Alcotest.(check bool) "no succ from start" (Automaton.IntSet.is_empty d) true;
              Alcotest.(check bool) "base recognised" (List.mem b ["Int";"Bool";"String"]) true
          | _ -> Alcotest.fail "start not Msg") starts
  in

  (* Nested parallel after a sequential prefix: two independent events r->s and t->u *)
  let test_nested_parallel_two () =
    (* p -> q first *)
    let p_local = send "q" "Int" mk_end in
    (* q receives then sends unit to r and t sequentially, then terminates *)
    let q_local = recv "p" "Int"
        (send "r" "Unit" (send "t" "Unit" mk_end)) in
    (* r waits for q then sends to s *)
    let r_local = recv "q" "Unit" (send "s" "Bool" mk_end) in
    let s_local = recv "r" "Bool" mk_end in
    (* t waits for q then sends to u *)
    let t_local = recv "q" "Unit" (send "u" "String" mk_end) in
    let u_local = recv "t" "String" mk_end in
    let locals = [ build_part "p" p_local; build_part "q" q_local;
                   build_part "r" r_local; build_part "s" s_local;
                   build_part "t" t_local; build_part "u" u_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        (* Expect 5 message states: p->q, q->r, q->t, r->s, t->u *)
        Alcotest.(check int) "num_states" 5 g.num_states
  in

  (* Nested parallel after prefix with three independent events *)
  let test_nested_parallel_three () =
    let p_local = send "q" "Int" mk_end in
    let q_local = recv "p" "Int"
        (send "r" "Unit"
           (send "t" "Unit"
              (send "v" "Unit" mk_end))) in
    let r_local = recv "q" "Unit" (send "s" "Bool" mk_end) in
    let s_local = recv "r" "Bool" mk_end in
    let t_local = recv "q" "Unit" (send "u" "String" mk_end) in
    let u_local = recv "t" "String" mk_end in
    let v_local = recv "q" "Unit" (send "w" "Float" mk_end) in
    let w_local = recv "v" "Float" mk_end in
    let locals = [ build_part "p" p_local; build_part "q" q_local;
                   build_part "r" r_local; build_part "s" s_local;
                   build_part "t" t_local; build_part "u" u_local;
                   build_part "v" v_local; build_part "w" w_local ] in
    match synthesise locals with
    | Error msg -> Alcotest.failf "synth failed: %s" msg
    | Ok g ->
        Alcotest.(check int) "num_states" 7 g.num_states
  in

  [
    Alcotest.test_case "independence conflict" `Quick test_independence_conflict;
    Alcotest.test_case "independence ok" `Quick test_independence_ok;
    Alcotest.test_case "simple sequence" `Quick test_simple_seq;
    Alcotest.test_case "two step" `Quick test_two_step;
    Alcotest.test_case "recursive" `Quick test_recursive;
    Alcotest.test_case "parallel independent" `Quick test_parallel_independent;
    Alcotest.test_case "parallel three" `Quick test_parallel_three;
    Alcotest.test_case "nested parallel two" `Quick test_nested_parallel_two;
    Alcotest.test_case "nested parallel three" `Quick test_nested_parallel_three;
  ]

let suite = ("synthesis", synthesis_tests) 