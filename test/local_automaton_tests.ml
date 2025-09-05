(*--------------------------------------------------------------------*)
(*  Unit‑tests for the local automaton module                          *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let local_automaton_tests =
  let open Local_automaton in
  
  (* Helper to build a local automaton from a local type *)
  let build_la lt = of_local (Encode.encode_local lt) in
  
  [
    test_case "LEnd" `Quick (fun () ->
      let g = build_la (parse_local "end") in
      Alcotest.(check int) "num_states" 0 g.num_states;
      Alcotest.(check (option int)) "start_state" None g.start_state
    );

    test_case "LSend simple" `Quick (fun () ->
      let g = build_la (parse_local "q ! [Int]; end") in
      Alcotest.(check int) "num_states" 1 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Snd ("Int", None) -> ()
       | _ -> fail "Expected Snd with Int and None destination")
    );

    test_case "LRecv simple" `Quick (fun () ->
      let g = build_la (parse_local "q ? [String]; end") in
      Alcotest.(check int) "num_states" 1 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Rcv ("String", None) -> ()
       | _ -> fail "Expected Rcv with String and None destination")
    );

    test_case "LInt simple" `Quick (fun () ->
      let g = build_la (parse_local "q ! {Ok: end, Err: end}") in
      Alcotest.(check int) "num_states" 1 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Int [("Ok", None); ("Err", None)] -> ()
       | _ -> fail "Expected Int with Ok and Err branches")
    );

    test_case "LExt simple" `Quick (fun () ->
      let g = build_la (parse_local "p ? {X: end, Y: end}") in
      Alcotest.(check int) "num_states" 1 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Ext [("X", None); ("Y", None)] -> ()
       | _ -> fail "Expected Ext with X and Y branches")
    );

    test_case "Send with continuation" `Quick (fun () ->
      let g = build_la (parse_local "q ! [Int]; r ? [String]; end") in
      Alcotest.(check int) "num_states" 2 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Snd ("Int", Some 1) -> ()
       | _ -> fail "Expected Snd with Int and destination 1");
      (match g.kinds.(1) with
       | Rcv ("String", None) -> ()
       | _ -> fail "Expected Rcv with String and None destination")
    );

    test_case "Receive with continuation" `Quick (fun () ->
      let g = build_la (parse_local "q ? [Bool]; r ! [Char]; end") in
      Alcotest.(check int) "num_states" 2 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Rcv ("Bool", Some 1) -> ()
       | _ -> fail "Expected Rcv with Bool and destination 1");
      (match g.kinds.(1) with
       | Snd ("Char", None) -> ()
       | _ -> fail "Expected Snd with Char and None destination")
    );

    test_case "Choice with messages" `Quick (fun () ->
      let g = build_la (parse_local "p ! {Ok: q ! [Int]; end, Err: q ? [String]; end}") in
      Alcotest.(check int) "num_states" 3 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Int [("Ok", Some 1); ("Err", Some 2)] -> ()
       | _ -> fail "Expected Int with Ok->1 and Err->2 branches")
    );

    test_case "Recursion with messages" `Quick (fun () ->
      let g = build_la (parse_local "rec t. q ! [Int]; t") in
      Alcotest.(check int) "num_states" 1 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Snd ("Int", Some 0) -> ()
       | _ -> fail "Expected Snd with Int and self-loop")
    );

    test_case "Complex nested choice" `Quick (fun () ->
      let g = build_la (parse_local "p ! {A: q ? [X]; end, B: r ! [Y]; end}") in
      Alcotest.(check int) "num_states" 3 g.num_states;
      Alcotest.(check (option int)) "start_state" (Some 0) g.start_state;
      (match g.kinds.(0) with
       | Int [("A", Some 1); ("B", Some 2)] -> ()
       | _ -> fail "Expected Int with A->1 and B->2 branches")
    );

    test_case "Int-based graph conversion" `Quick (fun () ->
      (* Test the new int-based functions *)
      let empty_g = empty_graph () in
      let empty_ig = empty_int_graph () in
      Alcotest.(check int) "empty graph states" 0 empty_g.num_states;
      Alcotest.(check int) "empty int graph states" 0 empty_ig.num_states;
      
      (* Test conversion functions *)
      let role_to_int = function "p" -> 0 | "q" -> 1 | "r" -> 2 | _ -> 3 in
      let int_to_role = function 0 -> "p" | 1 -> "q" | 2 -> "r" | _ -> "unknown" in
      
      let g = build_la (parse_local "p ! [Int]; end") in
      let ig = graph_to_int_graph g role_to_int in
      let g2 = int_graph_to_graph ig int_to_role in
      
      Alcotest.(check int) "conversion preserves num_states" g.num_states ig.num_states;
      Alcotest.(check int) "conversion preserves num_states back" g.num_states g2.num_states;
      Alcotest.(check (option int)) "conversion preserves start_state" g.start_state ig.start_state;
      
      (* Test direct creation *)
      let ig_direct = of_local_int (Encode.encode_local (parse_local "q ? [String]; end")) role_to_int in
      Alcotest.(check int) "direct int graph states" 1 ig_direct.num_states;
      Alcotest.(check (option int)) "direct int graph start" (Some 0) ig_direct.start_state;
      
      (* Test pretty printing *)
      let _ = string_of_int_graph ig_direct in
      ()
    );
  ]

let suite = ("local-automaton", local_automaton_tests) 