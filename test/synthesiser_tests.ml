(*--------------------------------------------------------------------*)
(*  Unit‑tests for the synthesiser module                              *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers
open Ast
open Local_automaton

let synthesiser_tests =
  let open Synthesiser in
  
  
  (* Helper to create int_graph from local type with role mapping *)
  let build_int_la lt role_to_int = 
    Printf.printf "  build_int_la: encoding...\n%!";
    let encoded = Encode.encode_local lt in
    Printf.printf "  build_int_la: building automaton...\n%!";
    let result = of_local_int encoded role_to_int in
    Printf.printf "  build_int_la: completed\n%!";
    result
  in
  
  (* Helper to create role mapping *)
  let make_role_map roles = 
    let role_array = Array.of_list roles in
    let role_to_int role = 
      match Array.find_index ((=) role) role_array with
      | Some i -> i
      | None -> failwith ("Unknown role: " ^ role)
    in
    (role_array, role_to_int)
  in
  
  (* Helper to check alpha equivalence with default variable equality *)
  let alpha_equiv_global g1 g2 = Alpha_equiv.global (=) g1 g2 in
  
  [
    test_case "Simple message p->q" `Quick (fun () ->
      Printf.printf "\n=== SIMPLE MESSAGE TEST ===\n%!";
      (* Protocol: p -> q [Int]; end *)
      let p_local = send "q" "Int" mk_end in
      let q_local = recv "p" "Int" mk_end in
      Printf.printf "1. Created local types\n%!";
      
      let roles = ["p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      Printf.printf "2. Created role mapping\n%!";
      
      Printf.printf "3a. About to build p_automaton...\n%!";
      let p_automaton = build_int_la p_local role_to_int in
      Printf.printf "3b. Built p_automaton, about to build q_automaton...\n%!";
      let q_automaton = build_int_la q_local role_to_int in
      Printf.printf "3c. Built both automata, about to call synth...\n%!";
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      Printf.printf "3d. Synth completed successfully!\n%!";
      
      (* DEBUG: Print actual vs expected results *)
      let pp_int_var fmt i = Format.fprintf fmt "X%d" i in
      Printf.printf "ACTUAL RESULT: %s\n%!" (Pretty.string_of pp_int_var result);
      let expected = GMsg ("p", "q", "Int", GEnd Loc.dummy, Loc.dummy) in
      Printf.printf "EXPECTED RESULT: %s\n%!" (Pretty.string_of pp_int_var expected);
      Printf.printf "ALPHA EQUIVALENT: %b\n%!" (alpha_equiv_global result expected);
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Simple choice p->q" `Quick (fun () ->
      (* Protocol: (p -> q [Int]; end) | (r -> s [Bool]; end) *)
      let p_local = send "q" "Int" mk_end in
      let q_local = recv "p" "Int" mk_end in
      let r_local = send "s" "Bool" mk_end in
      let s_local = recv "r" "Bool" mk_end in
      
      let roles = ["p"; "q"; "r"; "s"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let r_automaton = build_int_la r_local role_to_int in
      let s_automaton = build_int_la s_local role_to_int in
      let cfsm = [| p_automaton; q_automaton; r_automaton; s_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Result should be parallel composition in some order *)
      let msg1 = GMsg ("p", "q", "Int", GEnd Loc.dummy, Loc.dummy) in
      let msg2 = GMsg ("r", "s", "Bool", GEnd Loc.dummy, Loc.dummy) in
      let expected1 = GPar (msg1, msg2, Loc.dummy) in
      let expected2 = GPar (msg2, msg1, Loc.dummy) in
      
      let matches = alpha_equiv_global result expected1 || alpha_equiv_global result expected2 in
      check bool "parallel composition" true matches
    );

    test_case "Three-way parallel p->q | r->s | t->u" `Quick (fun () ->
      (* Protocol: (p -> q [A]) | (r -> s [B]) | (t -> u [C]) *)
      let p_local = send "q" "A" mk_end in
      let q_local = recv "p" "A" mk_end in
      let r_local = send "s" "B" mk_end in
      let s_local = recv "r" "B" mk_end in
      let t_local = send "u" "C" mk_end in
      let u_local = recv "t" "C" mk_end in
      
      let roles = ["p"; "q"; "r"; "s"; "t"; "u"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let r_automaton = build_int_la r_local role_to_int in
      let s_automaton = build_int_la s_local role_to_int in
      let t_automaton = build_int_la t_local role_to_int in
      let u_automaton = build_int_la u_local role_to_int in
      let cfsm = [| p_automaton; q_automaton; r_automaton; s_automaton; t_automaton; u_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Check that we get some valid parallel structure *)
      let rec has_all_messages g =
        match g with
        | GMsg (p, q, base, _, _) ->
            (p = "p" && q = "q" && base = "A") ||
            (p = "r" && q = "s" && base = "B") ||
            (p = "t" && q = "u" && base = "C")
        | GPar (g1, g2, _) -> has_all_messages g1 && has_all_messages g2
        | GEnd _ -> false
        | _ -> false
      in
      
      check bool "contains all three messages" true (has_all_messages result)
    );

    test_case "Request-response p->q->p" `Quick (fun () ->
      (* Protocol: p -> q [Req]; q -> p [Resp]; end *)
      let p_local = send "q" "Req" (recv "q" "Resp" mk_end) in
      let q_local = recv "p" "Req" (send "p" "Resp" mk_end) in
      
      let roles = ["p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      let expected = GMsg ("p", "q", "Req", 
                          GMsg ("q", "p", "Resp", GEnd Loc.dummy, Loc.dummy), 
                          Loc.dummy) in
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Choice with continuation" `Quick (fun () ->
      (* Protocol: p -> q {login: p -> q [User]; logout: end} *)
      let continue_branch = send "q" "User" mk_end in
      let p_local = Ast.LInt ("q", [("login", continue_branch); ("logout", mk_end)], Loc.dummy) in
      let q_local = Ast.LExt ("p", [("login", recv "p" "User" mk_end); ("logout", mk_end)], Loc.dummy) in
      
      let roles = ["p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      let expected = GBra ("p", "q", [
        ("login", GMsg ("p", "q", "User", GEnd Loc.dummy, Loc.dummy));
        ("logout", GEnd Loc.dummy)
      ], Loc.dummy) in
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Empty protocol" `Quick (fun () ->
      (* All participants terminated from start *)
      let roles = ["p"; "q"] in
      let (role_array, _) = make_role_map roles in
      
      let p_automaton = { num_states = 0; start_state = None; roles = [||]; kinds = [||] } in
      let q_automaton = { num_states = 0; start_state = None; roles = [||]; kinds = [||] } in
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      let expected = GEnd Loc.dummy in
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Single participant protocol" `Quick (fun () ->
      (* Only one participant that immediately terminates *)
      let roles = ["p"] in
      let (role_array, _) = make_role_map roles in
      
      let p_automaton = { num_states = 0; start_state = None; roles = [||]; kinds = [||] } in
      let cfsm = [| p_automaton |] in
      
      let result = synth cfsm role_array in
      let expected = GEnd Loc.dummy in
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Mixed parallel and sequential" `Quick (fun () ->
      (* Protocol: p -> q [Start]; (q -> r [A] | q -> s [B]) *)
      let p_local = send "q" "Start" mk_end in
      let q_local = recv "p" "Start" 
                      (Ast.LInt ("r", [("a", send "r" "A" mk_end)], Loc.dummy)) in
      let r_local = recv "q" "A" mk_end in
      let s_local = mk_end in  (* s doesn't participate in this execution path *)
      
      let roles = ["p"; "q"; "r"; "s"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let r_automaton = build_int_la r_local role_to_int in
      let s_automaton = build_int_la s_local role_to_int in
      let cfsm = [| p_automaton; q_automaton; r_automaton; s_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Should start with p -> q [Start] *)
      let starts_with_message = match result with
        | GMsg ("p", "q", "Start", _, _) -> true
        | _ -> false
      in
      
      check bool "starts with p->q message" true starts_with_message
    );

    test_case "Receiver superset of sender branches" `Quick (fun () ->
      (* Protocol: p offers {login, logout}, q can handle {login, logout, register} *)
      (* This should synthesize successfully - receiver handles superset of what sender offers *)
      let p_local = Ast.LInt ("q", [
        ("login", send "q" "User" mk_end);
        ("logout", mk_end)
      ], Loc.dummy) in
      let q_local = Ast.LExt ("p", [
        ("login", recv "p" "User" mk_end);
        ("logout", mk_end);
        ("register", recv "p" "UserData" mk_end)  (* Extra branch q can handle *)
      ], Loc.dummy) in
      
      let roles = ["p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      (* Expected: only the branches that p actually offers *)
      let expected = GBra ("p", "q", [
        ("login", GMsg ("p", "q", "User", GEnd Loc.dummy, Loc.dummy));
        ("logout", GEnd Loc.dummy)
      ], Loc.dummy) in
      
      check bool "alpha equivalent" true (alpha_equiv_global result expected)
    );

    test_case "Recursive alternating messages" `Quick (fun () ->
      (* Protocol: rec t . r->s : [Unit] ; p->q : [Unit] ; t 
         This is equivalent to: (rec t . r->s : [Unit]; t | rec t . p->q : [Unit] ; t)
         We don't mind which form is synthesized! *)
      
      (* Create recursive local types for independent r->s and p->q loops *)
      let r_local = Ast.LRec ("t", Ast.LSend ("s", "Unit", Ast.LVar ("t", Loc.dummy), Loc.dummy), Loc.dummy) in
      let s_local = Ast.LRec ("t", Ast.LRecv ("r", "Unit", Ast.LVar ("t", Loc.dummy), Loc.dummy), Loc.dummy) in
      let p_local = Ast.LRec ("t", Ast.LSend ("q", "Unit", Ast.LVar ("t", Loc.dummy), Loc.dummy), Loc.dummy) in
      let q_local = Ast.LRec ("t", Ast.LRecv ("p", "Unit", Ast.LVar ("t", Loc.dummy), Loc.dummy), Loc.dummy) in
      
      let roles = ["r"; "s"; "p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let r_automaton = build_int_la r_local role_to_int in
      let s_automaton = build_int_la s_local role_to_int in
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let cfsm = [| r_automaton; s_automaton; p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Check that we get some form of recursive protocol *)
      let is_recursive = match result with
        | GRec (_, _, _) -> true  (* Sequential form: rec t . r->s ; p->q ; t *)
        | GPar (GRec (_, _, _), GRec (_, _, _), _) -> true  (* Parallel form: (rec...) | (rec...) *)
        | _ -> false
      in
      
      (* Also check that it contains both r->s and p->q communications *)
      let contains_rs_and_pq = 
        let rec check_global g =
          match g with
          | GMsg ("r", "s", _, cont, _) -> true, check_global cont |> snd
          | GMsg ("p", "q", _, cont, _) -> check_global cont |> fst, true
          | GMsg (_, _, _, cont, _) -> check_global cont  (* Other messages *)
          | GPar (g1, g2, _) -> 
              let (rs1, pq1) = check_global g1 in
              let (rs2, pq2) = check_global g2 in
              (rs1 || rs2, pq1 || pq2)
          | GBra (_, _, branches, _) ->
              List.fold_left (fun (rs_acc, pq_acc) (_, branch_g) ->
                let (rs_b, pq_b) = check_global branch_g in
                (rs_acc || rs_b, pq_acc || pq_b)
              ) (false, false) branches
          | GRec (_, body, _) -> check_global body
          | GVar (_, _) -> (false, false)
          | GEnd _ -> (false, false)
        in
        let (has_rs, has_pq) = check_global result in
        has_rs && has_pq
      in
      
      check bool "is recursive protocol" true is_recursive;
      check bool "contains both r->s and p->q" true contains_rs_and_pq
    );

    test_case "Book quote protocol - three-party choice" `Quick (fun () ->
      (* Full book quote example: B1 -> S -> B2 with choice coordination *)
      let b1_local = parse_local "s ! [Title]; s ? [Quote]; b2 ! [Quote]; s ? { ok: s ? [Addr]; s ! [Addr]; end, quit: end }" in
      let b2_local = parse_local "b1 ? [Quote]; s ! { ok: end, quit: end }" in
      let s_local = parse_local "b1 ? [Title]; b1 ! [Quote]; b2 ? { ok: b1 ! { ok: b1 ! [Addr]; b1 ? [Addr]; end }, quit: b1 ! { quit: end } }" in
      
      let roles = ["b1"; "b2"; "s"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let b1_automaton = build_int_la b1_local role_to_int in
      let b2_automaton = build_int_la b2_local role_to_int in
      let s_automaton = build_int_la s_local role_to_int in
      let cfsm = [| b1_automaton; b2_automaton; s_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Check that result contains the key elements *)
      let rec check_global_structure g =
        match g with
        | GMsg ("b1", "s", "Title", cont, _) -> check_global_structure cont
        | GMsg ("s", "b1", "Quote", cont, _) -> check_global_structure cont  
        | GMsg ("b1", "b2", "Quote", cont, _) -> check_global_structure cont
        | GBra ("b2", "s", branches, _) -> 
            List.exists (fun (label, _) -> label = "ok" || label = "quit") branches
        | _ -> false
      in
      
      check bool "contains expected protocol structure" true (check_global_structure result)
    );

    test_case "Multi-party choice with coordination" `Quick (fun () ->
      (* Protocol: A -> B -> C, then C makes choice affecting both A and B *)
      let a_local = parse_local "b ! [Start]; c ? { yes: b ! [Proceed]; end, no: end }" in
      let b_local = parse_local "a ? [Start]; c ! [Request]; a ? [Proceed]; end" in  
      let c_local = parse_local "b ? [Request]; a ! { yes: end, no: end }" in
      
      let roles = ["a"; "b"; "c"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let a_automaton = build_int_la a_local role_to_int in
      let b_automaton = build_int_la b_local role_to_int in
      let c_automaton = build_int_la c_local role_to_int in
      let cfsm = [| a_automaton; b_automaton; c_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Should synthesize without parallel splitting at choice point *)
      let contains_choice = 
        let rec check g =
          match g with
          | GBra ("c", "a", _, _) -> true
          | GMsg (_, _, _, cont, _) -> check cont
          | GBra (_, _, branches, _) -> List.exists (fun (_, branch_g) -> check branch_g) branches
          | GPar (g1, g2, _) -> check g1 || check g2
          | _ -> false
        in
        check result
      in
      check bool "contains choice from c to a" true contains_choice
    );

    test_case "Nested choice protocols" `Quick (fun () ->
      (* Protocol with nested choices: outer choice leads to inner choice *)
      let p_local = parse_local "q ! { start: q ? { inner1: end, inner2: end }, quit: end }" in
      let q_local = parse_local "p ? { start: p ! { inner1: end, inner2: end }, quit: end }" in
      
      let roles = ["p"; "q"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let p_automaton = build_int_la p_local role_to_int in
      let q_automaton = build_int_la q_local role_to_int in
      let cfsm = [| p_automaton; q_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Check for nested choice structure *)
      let has_nested_choices =
        let rec count_choices g =
          match g with
          | GBra (_, _, branches, _) -> 
              1 + List.fold_left (fun acc (_, branch_g) -> acc + count_choices branch_g) 0 branches
          | GMsg (_, _, _, cont, _) -> count_choices cont
          | GPar (g1, g2, _) -> count_choices g1 + count_choices g2
          | _ -> 0
        in
        count_choices result >= 2
      in
      check bool "contains nested choices" true has_nested_choices
    );

    test_case "Choice with multiple message exchanges" `Quick (fun () ->
      (* Protocol: message exchange, then choice, then more messages based on choice *)
      let a_local = parse_local "b ! [Init]; b ? { path1: b ! [Data1]; b ? [Ack1]; end, path2: b ! [Data2]; end }" in
      let b_local = parse_local "a ? [Init]; a ! { path1: a ? [Data1]; a ! [Ack1]; end, path2: a ? [Data2]; end }" in
      
      let roles = ["a"; "b"] in
      let (role_array, role_to_int) = make_role_map roles in
      
      let a_automaton = build_int_la a_local role_to_int in
      let b_automaton = build_int_la b_local role_to_int in
      let cfsm = [| a_automaton; b_automaton |] in
      
      let result = synth cfsm role_array in
      
      (* Should have choice with different message patterns in branches *)
      let has_choice_with_messages =
        let rec check g =
          match g with
          | GBra (_, _, branches, _) ->
              List.exists (fun (_, branch_g) ->
                let rec has_msg bg = match bg with
                  | GMsg (_, _, _, _, _) -> true
                  | GBra (_, _, sub_branches, _) -> List.exists (fun (_, sub_g) -> has_msg sub_g) sub_branches
                  | _ -> false
                in has_msg branch_g
              ) branches
          | GMsg (_, _, _, cont, _) -> check cont
          | _ -> false
        in
        check result
      in
      check bool "choice branches contain messages" true has_choice_with_messages
    ); 
  ]

let suite = ("Synthesiser", synthesiser_tests)
