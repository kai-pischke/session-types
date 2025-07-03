(*--------------------------------------------------------------------*)
(*  Unit‑tests for the well‑formedness checker                        *)
(*--------------------------------------------------------------------*)
open Alcotest
open Lexing

(* Small helper to parse a global type from a string *)
let parse_global (src : string) : string Ast.global =
  let lb = from_string src in
  Parser.gfile Lexer.token lb

(* Expect the given thunk to raise [Well_formed.Error].               *)
let expect_error name thunk =
  test_case name `Quick (fun () ->
      match thunk () with
      | () -> fail "Expected Well_formed.Error, but check succeeded"
      | exception Well_formed.Error _ -> ())

(* Positive test: checker should succeed.                             *)
let expect_ok name thunk =
  test_case name `Quick (fun () -> thunk ())

(*--------------------------------------------------------------------*)
(*  Concrete test cases                                               *)
(*--------------------------------------------------------------------*)

let simple_valid () =
  let g = parse_global "a -> b { Ok: end }" in
  Well_formed.check_global g

let message_valid () =
  let g = parse_global "a -> b : [Int]; end" in
  Well_formed.check_global g

let parallel_valid () =
  let g = parse_global "a -> b { X: end } | c -> d { Y: end }" in
  Well_formed.check_global g

let message_parallel_valid () =
  let g = parse_global "a -> b : [String]; end | c -> d : [Int]; end" in
  Well_formed.check_global g

let self_msg_invalid () =
  let g = parse_global "a -> a { Loop: end }" in
  Well_formed.check_global g

let unguarded_rec_invalid () =
  let g = parse_global "rec t. t" in
  Well_formed.check_global g

let nested_unguarded_rec_invalid () =
  let g = parse_global "rec t. rec r . t" in
  Well_formed.check_global g

let overlapping_parallel_invalid () =
  let g = parse_global "a -> b { X: end } | b -> c { Y: end }" in
  Well_formed.check_global g

let unbound_var_invalid () =
  let g = parse_global "a -> b { X: t }" in
  Well_formed.check_global g

(*--------------------------------------------------------------------*)
(*  Test-suite registration                                           *)
(*--------------------------------------------------------------------*)

let wf_tests =
  [ expect_ok    "simple valid"           simple_valid
  ; expect_ok    "message valid"          message_valid
  ; expect_ok    "parallel valid"         parallel_valid
  ; expect_ok    "message parallel valid"  message_parallel_valid
  ; expect_error "self-messaging"         self_msg_invalid
  ; expect_error "unguarded recursion"    unguarded_rec_invalid
  ; expect_error "unguarded nested recursion" nested_unguarded_rec_invalid
  ; expect_error "overlapping parallel"   overlapping_parallel_invalid
  ; expect_error "unbound recursion var"  unbound_var_invalid
  ]

let encode_tests =
  [
    test_case "no rec for end" `Quick (fun () ->
      let g = parse_global "end" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GEnd _ -> ()
      | _ -> fail "Expected just end"
    );

    test_case "rec is preserved if used" `Quick (fun () ->
      let g = parse_global "rec t. a -> b { Ok: t }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _))], _), _) -> ()
      | _ -> fail "Expected rec to be preserved and variable renamed"
    );

    test_case "unused rec is removed" `Quick (fun () ->
      let g = parse_global "rec t. end" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GEnd _ -> ()
      | _ -> fail "Unused rec should be removed"
    );

    test_case "adjacent recs are preserved if used" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t, Err: u }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GRec (1, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _)); ("Err", Ast.GVar (1, _))], _), _), _) -> ()
      | _ -> fail "Both recs should be preserved and renamed"
    );

    test_case "inner unused rec is removed" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _))], _), _) -> ()
      | _ -> fail "Inner unused rec should be removed"
    );

    test_case "variable renaming is consecutive" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t, Err: u }" in
      let g' = Encode.encode g in
      let rec collect acc = function
        | Ast.GEnd _ -> acc
        | Ast.GVar (v, _) -> v :: acc
        | Ast.GRec (v, body, _) -> v :: collect acc body
        | Ast.GMsg (_, _, _, cont, _) -> collect acc cont
        | Ast.GBra (_, _, bs, _) -> List.fold_left (fun a (_, g) -> collect a g) acc bs
        | Ast.GPar (g1, g2, _) -> collect (collect acc g1) g2
      in
      let ids = List.sort_uniq compare (collect [] g') in
      if ids <> [0;1] then fail "Variables should be 0 and 1"
    );
  ]

let automaton_tests =
  let open Automaton in
  
  (* Parse automaton output into a structured format for comparison *)
  let parse_automaton_output output =
    let lines = String.split_on_char '\n' output in
    let rec parse_lines lines states start_states transitions =
      match lines with
      | [] -> (states, start_states, transitions)
      | line :: rest ->
          if String.length line = 0 then
            parse_lines rest states start_states transitions
          else if String.starts_with ~prefix:"States: " line then
            (* Parse states line - just skip this line, we'll get state info from individual state lines *)
            parse_lines rest states start_states transitions
          else if String.starts_with ~prefix:"Start states: " line then
            (* Parse start states line *)
            let start_str = String.sub line 13 (String.length line - 13) in
            let start_ids =
              if start_str = "" then []
              else
                String.split_on_char ',' start_str
                |> List.map String.trim
                |> List.filter (fun s -> s <> "" && String.for_all (fun c -> '0' <= c && c <= '9') s)
                |> List.map int_of_string
            in
            parse_lines rest states start_ids transitions
          else if String.starts_with ~prefix:"state " line then
            let parts = String.split_on_char ' ' line in
            if List.length parts >= 3 && List.nth parts 1 <> "" then
              let state_id = int_of_string (List.nth parts 1) in
              let roles_str = List.nth parts 2 in
              let roles = String.sub roles_str 1 (String.length roles_str - 2) in
              let role_parts = String.split_on_char ',' roles in
              let p = List.hd role_parts in
              let q = List.hd (List.tl role_parts) in
              let new_states = (state_id, (p, q)) :: states in
              parse_lines rest new_states start_states transitions
            else
              parse_lines rest states start_states transitions
          else if String.starts_with ~prefix:"from " line then
            let parts = String.split_on_char ' ' line in
            if List.length parts >= 6 then
              let from_state = int_of_string (List.nth parts 1) in
              let label = List.nth parts 3 in
              let to_states_str = List.nth parts 5 in
              let to_states =
                if to_states_str = "" then []
                else
                  String.split_on_char ',' to_states_str
                  |> List.map String.trim
                  |> List.filter (fun s -> s <> "" && String.for_all (fun c -> '0' <= c && c <= '9') s)
                  |> List.map int_of_string
              in
              let new_transitions = (from_state, label, to_states) :: transitions in
              parse_lines rest states start_states new_transitions
            else
              parse_lines rest states start_states transitions
          else
            parse_lines rest states start_states transitions
    in
    parse_lines lines [] [] []
  in
  
  (* Compare two automata structurally, allowing for state renumbering *)
  let compare_automata expected_str actual_str =
    let (expected_states, expected_start_states, expected_transitions) = 
      parse_automaton_output expected_str in
    let (actual_states, actual_start_states, actual_transitions) = 
      parse_automaton_output actual_str in
    
    (* Check that we have the same number of states *)
    if List.length expected_states != List.length actual_states then
      failwith (Printf.sprintf "Different number of states: expected %d, got %d" 
                  (List.length expected_states) (List.length actual_states));
    
    (* Check that we have the same number of start states *)
    if List.length expected_start_states != List.length actual_start_states then
      failwith (Printf.sprintf "Different number of start states: expected %d, got %d" 
                  (List.length expected_start_states) (List.length actual_start_states));
    
    (* Create a mapping from expected state roles to actual state IDs *)
    let expected_roles = List.map snd expected_states in
    let actual_roles = List.map snd actual_states in
    
    (* Check that all expected roles exist in actual *)
    let missing_roles = List.filter (fun role -> 
      not (List.mem role actual_roles)) expected_roles in
    if missing_roles != [] then
      failwith (Printf.sprintf "Missing roles: %s" 
                  (String.concat ", " (List.map (fun (p,q) -> Printf.sprintf "(%s,%s)" p q) missing_roles)));
    
    (* Create a mapping from expected state ID to actual state ID based on roles *)
    let state_mapping = ref [] in
    let used_actual_states = ref [] in
    
    List.iter (fun (expected_id, expected_role) ->
      let matching_actual = List.find (fun (actual_id, actual_role) ->
        actual_role = expected_role && not (List.mem actual_id !used_actual_states)
      ) actual_states in
      match matching_actual with
      | (actual_id, _) ->
          state_mapping := (expected_id, actual_id) :: !state_mapping;
          used_actual_states := actual_id :: !used_actual_states
      | exception Not_found ->
          failwith (Printf.sprintf "No matching state found for role (%s,%s)" 
                      (fst expected_role) (snd expected_role))
    ) expected_states;
    
    let state_map = List.to_seq !state_mapping |> Hashtbl.of_seq in
    
    (* Check start states mapping *)
    let expected_start_set = List.fold_left (fun acc id ->
      let actual_id = Hashtbl.find state_map id in
      IntSet.add actual_id acc
    ) IntSet.empty expected_start_states in
    
    let actual_start_set = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty actual_start_states in
    
    if not (IntSet.equal expected_start_set actual_start_set) then
      failwith (Printf.sprintf "Start states don't match: expected %s, got %s"
                  (String.concat "," (List.map string_of_int (IntSet.elements expected_start_set)))
                  (String.concat "," (List.map string_of_int (IntSet.elements actual_start_set))));
    
    (* Check transitions mapping *)
    let expected_trans_set = List.fold_left (fun acc (from, label, to_list) ->
      let actual_from = Hashtbl.find state_map from in
      let actual_to_set = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty to_list in
      (actual_from, label, actual_to_set) :: acc
    ) [] expected_transitions in
    
    let actual_trans_set = List.fold_left (fun acc (from, label, to_list) ->
      let actual_to_set = List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty to_list in
      (from, label, actual_to_set) :: acc
    ) [] actual_transitions in
    
    (* Compare transitions *)
    let compare_transition (from1, label1, to_set1) (from2, label2, to_set2) =
      from1 = from2 && label1 = label2 && IntSet.equal to_set1 to_set2
    in
    
    let missing_transitions = List.filter (fun expected_trans ->
      not (List.exists (fun actual_trans -> compare_transition expected_trans actual_trans) actual_trans_set)
    ) expected_trans_set in
    
    if missing_transitions != [] then
      failwith (Printf.sprintf "Missing transitions: %s"
                  (String.concat ", " (List.map (fun (from, label, to_set) ->
                    Printf.sprintf "from %d --%s--> %s" from label 
                      (String.concat "," (List.map string_of_int (IntSet.elements to_set)))
                  ) missing_transitions)));
    
    let extra_transitions = List.filter (fun actual_trans ->
      not (List.exists (fun expected_trans -> compare_transition expected_trans actual_trans) expected_trans_set)
    ) actual_trans_set in
    
    if extra_transitions != [] then
      failwith (Printf.sprintf "Extra transitions: %s"
                  (String.concat ", " (List.map (fun (from, label, to_set) ->
                    Printf.sprintf "from %d --%s--> %s" from label 
                      (String.concat "," (List.map string_of_int (IntSet.elements to_set)))
                  ) extra_transitions)));
    
    (* All checks passed *)
    ()
  in
  
  let test_case_graph name src expected_str =
    test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let aut = of_global g' in
      let pretty = string_of_graph aut in
      compare_automata expected_str pretty
    )
  in
  [
    test_case_graph
      "GEnd"
      "end"
      "States: \nStart states: \n"
  ;
    test_case_graph
      "GMsg simple"
      "rec t. a -> b { Ok: t }"
      "States: 0\nStart states: 0\nstate 0 (a,b)\nfrom 0 --label Ok--> 0\n"
  ;
    test_case_graph
      "GBra simple"
      "rec t. a -> b { Ok: end, Err: t }"
      "States: 0\nStart states: 0\nstate 0 (a,b)\nfrom 0 --label Ok--> \nfrom 0 --label Err--> 0\n"
  ;
    test_case_graph
      "Three in parallel"
      "a -> b { Ok: end } | c -> d { X: end } | e -> f { Y: end }"
      "States: 0, 1, 2\nStart states: 0, 1, 2\nstate 0 (a,b)\nstate 1 (c,d)\nstate 2 (e,f)\nfrom 0 --label Ok--> \nfrom 1 --label X--> \nfrom 2 --label Y--> \n"
  ;
    test_case_graph
      "Nested parallel"
      "a -> b { Ok: c -> d { X: end } | e -> f { Y: end } }"
      "States: 0, 1, 2\nStart states: 0\nstate 0 (a,b)\nstate 1 (c,d)\nstate 2 (e,f)\nfrom 0 --label Ok--> 1,2\nfrom 1 --label X--> \nfrom 2 --label Y--> \n"
  ;
    test_case_graph
      "Parallel with recursion both sides"
      "(rec t. a -> b { Ok: t }) | (rec u. c -> d { X: u, Y: end })"
      "States: 0, 1\nStart states: 0, 1\nstate 0 (a,b)\nstate 1 (c,d)\nfrom 0 --label Ok--> 0\nfrom 1 --label X--> 1\nfrom 1 --label Y--> \n"
  ;
    test_case_graph
      "Branch successor present"
      "rec t. a -> b { X: c -> d { Z: end }, Y : t }"
      "States: 0, 1\nStart states: 0\nstate 0 (a,b)\nstate 1 (c,d)\nfrom 0 --label X--> 1\nfrom 0 --label Y--> 0\nfrom 1 --label Z--> \n"
  ;
    test_case_graph
      "Message simple"
      "a -> b : [Int]; end"
      "States: 0\nStart states: 0\nstate 0 (a,b)\nfrom 0 --base Int--> \n"
  ;
    test_case_graph
      "Message with continuation"
      "a -> b : [String]; c -> d : [Int]; end"
      "States: 0, 1\nStart states: 0\nstate 0 (a,b)\nstate 1 (c,d)\nfrom 0 --base String--> 1\nfrom 1 --base Int--> \n"
  ;
    test_case_graph
      "Message in recursion"
      "rec t. a -> b : [Bool]; t"
      "States: 0\nStart states: 0\nstate 0 (a,b)\nfrom 0 --base Bool--> 0\n"
  ]

let balance_tests =
  let open Balance in
  let check_balanced name src expected =
    test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let aut = Automaton.of_global g' in
      let res = is_balanced aut in
      Alcotest.(check bool) "balanced?" expected res
    )
  in
  [ check_balanced "rec loop balanced" "rec t. a -> b { X: t }" true
  ; check_balanced "double rec loop balanced" "rec t. a -> b { X: a -> b { Y: t } }" true
  ; check_balanced "parallel balanced" "a -> b { Ok: end } | c -> d { X: end }" true
  ; check_balanced "unbalanced branch" "rec t. a -> b { X: c -> d { Z: end }, Y : t }" false
  ; check_balanced "unbalanced recursion branch" "rec t. a -> b { X: a -> b { Z: end }, Y : c -> d { A: t } }" false
  ; check_balanced "balanced nested rec || rec" "(rec t. a -> b { X: t }) | (rec u. c -> d { Y: u })" true
  ; check_balanced "balanced rec around parallel" "rec t. (a -> b { X: end } | c -> d { Y: end })" true
  ; check_balanced "balanced side stops" "(rec t. a -> b { X: t }) | (c -> d { Y: end })" true
  ; check_balanced "unbalanced deep mix" "(rec t. a -> b { X: (c -> d { Z: end } | e -> f { W: t }), Y: t }) | (g -> h { U: end })" false
  ]

(* Local type parser tests *)
let parse_local (src : string) : string Ast.local =
  let lb = from_string src in
  Parser.lfile Lexer.token lb

let local_type_tests =
  let open Ast in
  [
    test_case "internal choice" `Quick (fun () ->
      let l = parse_local "p ! {L1: end, L2: end}" in
      match l with
      | LInt ("p", [("L1", LEnd _); ("L2", LEnd _)], _) -> ()
      | _ -> fail "Did not parse internal choice"
    );
    test_case "external choice" `Quick (fun () ->
      let l = parse_local "p ? {L1: end, L2: end}" in
      match l with
      | LExt ("p", [("L1", LEnd _); ("L2", LEnd _)], _) -> ()
      | _ -> fail "Did not parse external choice"
    );
    test_case "send message" `Quick (fun () ->
      let l = parse_local "p![Int]; end" in
      match l with
      | LSend ("p", "Int", LEnd _, _) -> ()
      | _ -> fail "Did not parse send"
    );
    test_case "receive message" `Quick (fun () ->
      let l = parse_local "p?[Int]; end" in
      match l with
      | LRecv ("p", "Int", LEnd _, _) -> ()
      | _ -> fail "Did not parse receive"
    );
    test_case "send with continuation" `Quick (fun () ->
      let l = parse_local "p![String]; q?[Int]; end" in
      match l with
      | LSend ("p", "String", LRecv ("q", "Int", LEnd _, _), _) -> ()
      | _ -> fail "Did not parse send with continuation"
    );
    test_case "receive with continuation" `Quick (fun () ->
      let l = parse_local "p?[Bool]; q![Float]; end" in
      match l with
      | LRecv ("p", "Bool", LSend ("q", "Float", LEnd _, _), _) -> ()
      | _ -> fail "Did not parse receive with continuation"
    );
    test_case "choice with messages" `Quick (fun () ->
      let l = parse_local "p ! {Ok: q![Int]; end, Err: q?[String]; end}" in
      match l with
      | LInt ("p", [("Ok", LSend ("q", "Int", LEnd _, _)); ("Err", LRecv ("q", "String", LEnd _, _))], _) -> ()
      | _ -> fail "Did not parse choice with messages"
    );
    test_case "recursion with messages" `Quick (fun () ->
      let l = parse_local "rec t. p![Int]; t" in
      match l with
      | LRec ("t", LSend ("p", "Int", LVar ("t", _), _), _) -> ()
      | _ -> fail "Did not parse recursion with messages"
    );
  ]

(* Global type explicit message test *)
let global_msg_test =
  test_case "explicit message" `Quick (fun () ->
    let g = parse_global "a -> b : [Int]; end" in
    match g with
    | GMsg ("a", "b", "Int", GEnd _, _) -> ()
    | _ -> fail "Did not parse explicit message"
  )

(* Local automaton tests *)
let local_automaton_tests =
  let open Local_automaton in
  let test_case_graph name src expected_str =
    test_case name `Quick (fun () ->
      let l = parse_local src in
      let l' = Encode.encode_local l in
      let aut = of_local l' in
      let pretty = string_of_graph aut in
      Alcotest.(check string) "local automaton pretty" expected_str pretty
    )
  in
  [
    test_case_graph
      "LEnd"
      "end"
      "States: \nStart state: none\n"
  ;
    test_case_graph
      "LSend simple"
      "rec t. p![Int]; t"
      "States: 0\nStart state: 0\nstate 0 (p)\nfrom 0 --snd Int--> 0\n"
  ;
    test_case_graph
      "LRecv simple"
      "rec t. p?[String]; t"
      "States: 0\nStart state: 0\nstate 0 (p)\nfrom 0 --rcv String--> 0\n"
  ;
    test_case_graph
      "LInt simple"
      "rec t. p ! {Ok: end, Err: t}"
      "States: 0\nStart state: 0\nstate 0 (p)\nfrom 0 --int Ok--> end\nfrom 0 --int Err--> 0\n"
  ;
    test_case_graph
      "LExt simple"
      "rec t. p ? {Ok: end, Err: t}"
      "States: 0\nStart state: 0\nstate 0 (p)\nfrom 0 --ext Ok--> end\nfrom 0 --ext Err--> 0\n"
  ;
    test_case_graph
      "Send with continuation"
      "p![Int]; q?[String]; end"
      "States: 0, 1\nStart state: 0\nstate 0 (p)\nstate 1 (q)\nfrom 0 --snd Int--> 1\nfrom 1 --rcv String--> end\n"
  ;
    test_case_graph
      "Receive with continuation"
      "p?[Bool]; q![Float]; end"
      "States: 0, 1\nStart state: 0\nstate 0 (p)\nstate 1 (q)\nfrom 0 --rcv Bool--> 1\nfrom 1 --snd Float--> end\n"
  ;
    test_case_graph
      "Choice with messages"
      "p ! {Ok: q![Int]; end, Err: q?[String]; end}"
      "States: 0, 1, 2\nStart state: 0\nstate 0 (p)\nstate 1 (q)\nstate 2 (q)\nfrom 0 --int Ok--> 1\nfrom 0 --int Err--> 2\nfrom 1 --snd Int--> end\nfrom 2 --rcv String--> end\n"
  ;
    test_case_graph
      "Recursion with messages"
      "rec t. p![Int]; q?[String]; t"
      "States: 0, 1\nStart state: 0\nstate 0 (p)\nstate 1 (q)\nfrom 0 --snd Int--> 1\nfrom 1 --rcv String--> 0\n"
  ;
    test_case_graph
      "Complex nested choice"
      "rec t. p ! {A: q ? {X: end, Y: t}, B: r![Bool]; t}"
      "States: 0, 1, 2\nStart state: 0\nstate 0 (p)\nstate 1 (q)\nstate 2 (r)\nfrom 0 --int A--> 1\nfrom 0 --int B--> 2\nfrom 1 --ext X--> end\nfrom 1 --ext Y--> 0\nfrom 2 --snd Bool--> 0\n"
  ]

(* Live tests *)
let live_tests =
  let open Live in
  
  (* Helper to build a minimal local automaton with a single Send action *)
  let single_send_graph ~peer ~msg =
    let open Local_automaton in
    {
      num_states = 1;
      start_state = Some 0;
      roles = [| peer |];            (* partner role stored here *)
      kinds = [| Snd (msg, None) |];
    }
  in

  let single_recv_graph ~peer ~msg =
    let open Local_automaton in
    {
      num_states = 1;
      start_state = Some 0;
      roles = [| peer |];            (* partner role stored here *)
      kinds = [| Rcv (msg, None) |];
    }
  in

  let empty_graph =
    let open Local_automaton in
    {
      num_states = 0;
      start_state = None;
      roles = [||];
      kinds = [||];
    }
  in

  let participants = ["p"] in
  let automata_send = [| single_send_graph ~peer:"q" ~msg:"Int" |] in
  let automata_empty = [| empty_graph |] in

  [
    test_case "no violation" `Quick (fun () ->
      let state = [|0|] in
      let fp = { states=[state]; observations=[] } in
      let res = is_counterwitness ~participants ~automata:automata_empty (FinitePath fp) in
      Alcotest.(check bool) "not counterwitness" false res
    );
    
    test_case "simple violation" `Quick (fun () ->
      let state1 = [|0|] in
      let state2 = [|0|] in  (* same state, but we add no observations *)
      let fp = { states=[state1; state2]; observations=[ActionSet.empty] } in
      let res = is_counterwitness ~participants ~automata:automata_send (FinitePath fp) in
      Alcotest.(check bool) "counterwitness detected" true res
    );

    (* Third, more complex test: two states, one observation covers the barb *)
    test_case "covered by observation" `Quick (fun () ->
      let state1 = [|0|] in              (* participant p ready to send *)
      let state2 = [|1|] in              (* participant p has terminated: invalid state index -> empty barb *)
      let obs = ActionSet.of_list [
        Send ("p", "q", "Int")
      ] in
      let fp = { states=[state1; state2]; observations=[obs] } in
      let res = is_counterwitness ~participants ~automata:automata_send (FinitePath fp) in
      Alcotest.(check bool) "no counterwitness when covered" false res
    );

    (* Fourth: non-trivial infinite counterwitness *)
    test_case "infinite violation" `Quick (fun () ->
      (* Two participants: p wants to send, q wants to receive *)
      let participants2 = ["p"; "q"] in

      let single_send_graph ~peer ~msg =
        let open Local_automaton in
        {
          num_states = 1;
          start_state = Some 0;
          roles = [| peer |];
          kinds = [| Snd (msg, None) |];
        }
      in
      let single_recv_graph ~peer ~msg =
        let open Local_automaton in
        {
          num_states = 1;
          start_state = Some 0;
          roles = [| peer |];
          kinds = [| Rcv (msg, None) |];
        }
      in

      let aut_p = single_send_graph ~peer:"q" ~msg:"Int" in
      let aut_q = single_recv_graph ~peer:"p" ~msg:"Int" in
      let automata2 = [| aut_p; aut_q |] in

      let state0 = [|0; 0|] in
      let obs_send = ActionSet.of_list [ Send ("p", "q", "Int") ] in

      let ip = {
        prefix_states = [];
        prefix_observations = [];
        cycle_states = [state0];
        cycle_observations = [obs_send];
      } in
      let res = is_counterwitness ~participants:participants2 ~automata:automata2 (InfinitePath ip) in
      Alcotest.(check bool) "counterwitness infinite" true res
    );

    (* Fifth: infinite path that is NOT a violation because union covers barb *)
    test_case "infinite covered" `Quick (fun () ->
      let participants2 = ["p"; "q"] in
      let aut_p = single_send_graph ~peer:"q" ~msg:"Int" in
      let aut_q = single_recv_graph ~peer:"p" ~msg:"Int" in
      let automata2 = [| aut_p; aut_q |] in
      let state0 = [|0; 0|] in
      let obs_full = ActionSet.of_list [
        Send ("p", "q", "Int");
        Receive ("q", "p", "Int")
      ] in
      let ip = {
        prefix_states = [];
        prefix_observations = [];
        cycle_states = [state0];
        cycle_observations = [obs_full];
      } in
      let res = is_counterwitness ~participants:participants2 ~automata:automata2 (InfinitePath ip) in
      Alcotest.(check bool) "no counterwitness when covered (infinite)" false res
    );

    (* Sixth: prefix violation with benign cycle *)
    test_case "infinite prefix violation" `Quick (fun () ->
      let participants2 = ["p"; "q"] in
      let aut_p = single_send_graph ~peer:"q" ~msg:"Int" in
      let aut_q = single_recv_graph ~peer:"p" ~msg:"Int" in
      let automata2 = [| aut_p; aut_q |] in
      let state0 = [|0; 0|] in                (* prefix state *)
      let state1 = [|0; 0|] in                (* cycle state *)
      let obs_empty = ActionSet.empty in
      let obs_send_only = ActionSet.of_list [ Send ("p", "q", "Int") ] in
      let ip = {
        prefix_states = [state0];
        prefix_observations = [obs_empty];
        cycle_states = [state1];
        cycle_observations = [obs_send_only];
      } in
      let res = is_counterwitness ~participants:participants2 ~automata:automata2 (InfinitePath ip) in
      Alcotest.(check bool) "counterwitness prefix" true res
    );

    (* Seventh: cycle violation with covered prefix *)
    test_case "infinite cycle violation" `Quick (fun () ->
      let participants2 = ["p"; "q"] in
      let aut_p = single_send_graph ~peer:"q" ~msg:"Int" in
      let aut_q = single_recv_graph ~peer:"p" ~msg:"Int" in
      let automata2 = [| aut_p; aut_q |] in
      let state0 = [|0; 0|] in                (* prefix state covered *)
      let state1 = [|0; 0|] in                (* cycle state not fully covered *)
      let obs_full = ActionSet.of_list [
        Send ("p", "q", "Int");
        Receive ("q", "p", "Int")
      ] in
      let obs_send_only = ActionSet.of_list [ Send ("p", "q", "Int") ] in
      let ip = {
        prefix_states = [state0];
        prefix_observations = [obs_full];
        cycle_states = [state1];
        cycle_observations = [obs_send_only];
      } in
      let res = is_counterwitness ~participants:participants2 ~automata:automata2 (InfinitePath ip) in
      Alcotest.(check bool) "counterwitness cycle" true res
    );

    (* Eighth: multi-state cycle with union covering barb (no violation) *)
    test_case "infinite covered multi-state" `Quick (fun () ->
      let participants2 = ["p"; "q"] in
      let aut_p = single_send_graph ~peer:"q" ~msg:"Int" in
      let aut_q = single_recv_graph ~peer:"p" ~msg:"Int" in
      let automata2 = [| aut_p; aut_q |] in
      let st0 = [|0; 0|] in      (* barb send+recv *)
      let st1 = [|0; 0|] in
      let obs_send_only = ActionSet.of_list [ Send ("p", "q", "Int") ] in
      let obs_recv_only = ActionSet.of_list [ Receive ("q", "p", "Int") ] in
      let ip = {
        prefix_states = [];
        prefix_observations = [];
        cycle_states = [st0; st1];
        cycle_observations = [obs_send_only; obs_recv_only];
      } in
      let res = is_counterwitness ~participants:participants2 ~automata:automata2 (InfinitePath ip) in
      Alcotest.(check bool) "no counterwitness multi-state" false res
    );

    (* Ninth: internal choice violation *)
    test_case "internal choice violation" `Quick (fun () ->
      let participants = ["p"] in
      let single_int_graph ~peer ~label =
        let open Local_automaton in
        {
          num_states = 1;
          start_state = Some 0;
          roles = [| peer |];
          kinds = [| Int [ (label, None) ] |];
        }
      in
      let aut_p = single_int_graph ~peer:"q" ~label:"Ok" in
      let automata = [| aut_p |] in
      let state0 = [|0|] in
      let state1 = [|0|] in
      (* No observation provided, missing the Int action *)
      let fp = { states=[state0; state1]; observations=[ActionSet.empty] } in
      let res = is_counterwitness ~participants ~automata (FinitePath fp) in
      Alcotest.(check bool) "counterwitness Int" true res
    );
  ]

(* Automaton to global type tests *)
let automaton_to_global_tests =
  let open Ast in
  let open Automaton in
  let mk_graph ~roles ~kinds ~start_states =
    { num_states = Array.length roles;
      start_states;
      roles;
      kinds;
    }
  in
  [
    Alcotest.test_case "GEnd" `Quick (fun () ->
      let g = mk_graph ~roles:[||] ~kinds:[||] ~start_states:IntSet.empty in
      let gt = Automaton_to_global.automaton_to_global g in
      match gt with
      | GEnd _ -> ()
      | _ -> Alcotest.fail "Expected GEnd"
    );
    Alcotest.test_case "GMsg simple" `Quick (fun () ->
      let roles = [| ("p", "q") |] in
      let kinds = [| Msg ("Int", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in
      Format.eprintf "ACTUAL: %s@." (Pretty.string_of Format.pp_print_int gt);
      (match gt with
      | GEnd _ -> Alcotest.fail "Should not be GEnd"
      | GMsg (p, q, "Int", GEnd _, _) when p = "p" && q = "q" -> ()
      | _ -> Alcotest.fail "Expected GMsg(p,q,Int,GEnd)")
    );
    Alcotest.test_case "GBra simple" `Quick (fun () ->
      let roles = [| ("a", "b"); ("a", "b"); ("a", "b") |] in
      let kinds = [| Bra [ ("L", IntSet.singleton 1); ("R", IntSet.singleton 2) ];
                     Msg ("Int", IntSet.empty);
                     Msg ("Bool", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in
      (match gt with
      | GBra ("a", "b", [ ("L", GMsg ("a", "b", "Int", GEnd _, _));
                            ("R", GMsg ("a", "b", "Bool", GEnd _, _)) ], _) -> ()
      | _ -> Alcotest.fail "Expected GBra with two branches")
    );
    Alcotest.test_case "GPar simple" `Quick (fun () ->
      let roles = [| ("p", "q"); ("r", "s") |] in
      let kinds = [| Msg ("Int", IntSet.empty); Msg ("Bool", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.of_list [0;1]) in
      let gt = Automaton_to_global.automaton_to_global g in
      let g1 = GMsg ("p", "q", "Int", GEnd Loc.dummy, Loc.dummy) in
      let g2 = GMsg ("r", "s", "Bool", GEnd Loc.dummy, Loc.dummy) in
      let expected = GPar (g1, g2, Loc.dummy) in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GPar simple: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "GPar nested" `Quick (fun () ->
      (* Create a more complex automaton with nested parallel structure *)
      let roles = [| ("a", "b"); ("c", "d"); ("e", "f"); ("g", "h") |] in
      let kinds = [| Msg ("Int", IntSet.singleton 1); 
                     Msg ("Bool", IntSet.singleton 2);
                     Msg ("String", IntSet.empty);
                     Msg ("Float", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.of_list [0;2]) in
      let gt = Automaton_to_global.automaton_to_global g in
      let g1 = GMsg ("a", "b", "Int", GMsg ("c", "d", "Bool", GMsg ("e", "f", "String", GEnd Loc.dummy, Loc.dummy), Loc.dummy), Loc.dummy) in
      let g2 = GMsg ("e", "f", "String", GEnd Loc.dummy, Loc.dummy) in
      let expected = GPar (g1, g2, Loc.dummy) in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GPar nested: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "GRec simple" `Quick (fun () ->
      (* Create automaton with recursion *)
      let roles = [| ("p", "q") |] in
      let kinds = [| Msg ("Int", IntSet.singleton 0) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in
      (* Collect recursion branches *)
      let check_rec = function
        | GRec (id, GMsg ("p","q","Int", GVar (id',_), _), _) when id = id' -> true
        | _ -> false
      in
      if not (check_rec gt) then Alcotest.fail "Recursive structure unexpected"
    );
    Alcotest.test_case "GBra with recursion" `Quick (fun () ->
      (* Create branching automaton with recursion in one branch *)
      let roles = [| ("a", "b"); ("a", "b"); ("a", "b") |] in
      let kinds = [| Bra [ ("Ok", IntSet.singleton 1); ("Err", IntSet.singleton 2) ];
                     Msg ("Int", IntSet.singleton 0);  (* recursive *)
                     Msg ("Bool", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in
      (* Compare normalised actual and expected global types directly *)
      let expected =
        GRec (0, GBra ("a", "b", [
          ("Ok", GMsg ("a", "b", "Int", GVar (0, Loc.dummy), Loc.dummy));
          ("Err", GMsg ("a", "b", "Bool", GEnd Loc.dummy, Loc.dummy))
        ], Loc.dummy), Loc.dummy)
      in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GBra with recursion: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "GPar with recursion" `Quick (fun () ->
      (* Create parallel automaton where one side has recursion *)
      let roles = [| ("p", "q"); ("r", "s"); ("r", "s") |] in
      let kinds = [| Msg ("Int", IntSet.empty);
                     Msg ("Bool", IntSet.singleton 2);  (* recursive *)
                     Msg ("String", IntSet.singleton 1) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.of_list [0;1]) in
      let gt = Automaton_to_global.automaton_to_global g in
      let g1 = GMsg ("p", "q", "Int", GEnd Loc.dummy, Loc.dummy) in
      let g2 = GRec (0, GMsg ("r", "s", "Bool", GMsg ("r", "s", "String", GVar (0, Loc.dummy), Loc.dummy), Loc.dummy), Loc.dummy) in
      let expected = GPar (g1, g2, Loc.dummy) in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GPar with recursion: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "Complex nested structure" `Quick (fun () ->
      (* Create a complex automaton with multiple features *)
      let roles = [| ("a", "b"); ("c", "d"); ("e", "f"); ("g", "h"); ("i", "j") |] in
      let kinds = [| Bra [ ("X", IntSet.singleton 1); ("Y", IntSet.singleton 2) ];
                     Msg ("Int", IntSet.singleton 3);
                     Msg ("Bool", IntSet.singleton 0);  (* recursive *)
                     Msg ("String", IntSet.empty);
                     Msg ("Float", IntSet.empty) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in
      let expected =
        GRec (0, GBra ("a", "b", [
          ("X", GMsg ("c", "d", "Int", GMsg ("g", "h", "String", GEnd Loc.dummy, Loc.dummy), Loc.dummy));
          ("Y", GMsg ("e", "f", "Bool", GVar (0, Loc.dummy), Loc.dummy))
        ], Loc.dummy), Loc.dummy)
      in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "Complex nested structure: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "GPar both sides recursive" `Quick (fun () ->
      (* Parallel where each side is its own recursion loop, no free vars *)
      let roles = [|
        ("p","q"); (* 0 *)
        ("p","q"); (* 1 *)
        ("r","s"); (* 2 *)
        ("r","s"); (* 3 *)
      |] in
      let kinds = [|
        Msg ("Int", IntSet.singleton 1);            (* 0 -> 1 *)
        Msg ("Bool", IntSet.singleton 0);           (* 1 -> 0 loop *)
        Msg ("String", IntSet.singleton 3);        (* 2 -> 3 *)
        Msg ("Float", IntSet.singleton 2);         (* 3 -> 2 loop *)
      |] in
      let starts = IntSet.of_list [0;2] in
      let g = mk_graph ~roles ~kinds ~start_states:starts in
      let gt = Automaton_to_global.automaton_to_global g in

      (* expected global type *)
      let side1 =
        GRec (0,
          GMsg ("p","q","Int",
            GMsg ("p","q","Bool", GVar (0, Loc.dummy), Loc.dummy),
          Loc.dummy),
        Loc.dummy)
      in
      let side2 =
        GRec (1,
          GMsg ("r","s","String",
            GMsg ("r","s","Float", GVar (1, Loc.dummy), Loc.dummy),
          Loc.dummy),
        Loc.dummy)
      in
      let expected = GPar (side1, side2, Loc.dummy) in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GPar closed recs: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "GPar three closed recs" `Quick (fun () ->
      let roles =
        [| ("a","b"); ("c","d"); ("e","f") |] in
      let kinds =
        [| Msg ("Int",  IntSet.singleton 0);
           Msg ("Bool", IntSet.singleton 1);
           Msg ("String", IntSet.singleton 2) |] in
      (* each state loops to itself; all three start in parallel *)
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.of_list [0;1;2]) in
      let gt = Automaton_to_global.automaton_to_global g in

      let loop r1 r2 base var_id =
        GRec (var_id,
          GMsg (r1, r2, base, GVar (var_id, Loc.dummy), Loc.dummy),
        Loc.dummy)
      in
      let side1 = loop "a" "b" "Int" 0 in
      let side2 = loop "c" "d" "Bool" 1 in
      let side3 = loop "e" "f" "String" 2 in
      let expected = GPar (side1, GPar (side2, side3, Loc.dummy), Loc.dummy) in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "GPar three recs: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
    Alcotest.test_case "Nested parallel inside message" `Quick (fun () ->
      (* state0 sends Int then splits into two self-loop recs *)
      let roles =
        [| ("x","y"); ("p","q"); ("r","s") |] in
      let kinds =
        [| Msg ("Int", IntSet.of_list [1;2]);              (* parallel after message *)
           Msg ("Bool", IntSet.singleton 1);               (* loop side1 *)
           Msg ("String", IntSet.singleton 2)              (* loop side2 *) |] in
      let g = mk_graph ~roles ~kinds ~start_states:(IntSet.singleton 0) in
      let gt = Automaton_to_global.automaton_to_global g in

      let side1 = GRec (0, GMsg ("p","q","Bool", GVar (0, Loc.dummy), Loc.dummy), Loc.dummy) in
      let side2 = GRec (1, GMsg ("r","s","String", GVar (1, Loc.dummy), Loc.dummy), Loc.dummy) in
      let expected =
        GMsg ("x","y","Int", GPar (side1, side2, Loc.dummy), Loc.dummy)
      in
      if not (Alpha_equiv.global (=) gt expected) then
        Alcotest.failf "Nested parallel: expected %s but got %s"
          (Pretty.string_of Format.pp_print_int expected)
          (Pretty.string_of Format.pp_print_int gt)
    );
  ]

(* ------------------------------------------------------------------ *)
(* Automaton to local type tests                                       *)
(* ------------------------------------------------------------------ *)
let automaton_to_local_tests =
  let open Ast in
  let open Local_automaton in
  let mk_graph ~roles ~kinds ~start_state =
    { num_states = Array.length roles;
      start_state;
      roles;
      kinds }
  in
  [
    Alcotest.test_case "LSend simple" `Quick (fun () ->
      let roles = [| "p" |] in
      let kinds = [| Snd ("Int", None) |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let expected = LSend ("p","Int", LEnd Loc.dummy, Loc.dummy) in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "LSend simple: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );

    Alcotest.test_case "Internal choice" `Quick (fun () ->
      let roles = [| "q"; "q"; "q" |] in
      let kinds = [|
        Int [ ("L", Some 1); ("R", Some 2) ];
        Snd ("Int", None);
        Snd ("Bool", None) |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let expected =
        LInt ("q", [
          ("L", LSend ("q","Int", LEnd Loc.dummy, Loc.dummy));
          ("R", LSend ("q","Bool", LEnd Loc.dummy, Loc.dummy)) ], Loc.dummy)
      in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "LInt choice: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );

    Alcotest.test_case "Recursion loop" `Quick (fun () ->
      let roles = [| "p"; "p" |] in
      let kinds = [| Snd ("Int", Some 1); Snd ("Bool", Some 0) |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let expected =
        LRec (0,
          LSend ("p","Int",
            LSend ("p","Bool", LVar (0, Loc.dummy), Loc.dummy),
          Loc.dummy),
        Loc.dummy)
      in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "Local recursion: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );

    (* External choice where one branch terminates, the other loops *)
    Alcotest.test_case "External choice mixed" `Quick (fun () ->
      let roles = [| "q"; "q"; "q" |] in
      let kinds = [|
        Ext [ ("A", Some 1); ("B", Some 2) ];
        Snd ("Int", None);                         (* ends *)
        Snd ("Bool", Some 2)                       (* loops *)
      |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let expected =
        LExt ("q", [
          ("A", LSend ("q","Int", LEnd Loc.dummy, Loc.dummy));
          ("B", LRec (0, LSend ("q","Bool", LVar (0, Loc.dummy), Loc.dummy), Loc.dummy))
        ], Loc.dummy)
      in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "Ext choice mixed: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );

    (* Mutual recursion of two states send / recv *)
    Alcotest.test_case "Mutual send/recv recursion" `Quick (fun () ->
      let roles = [| "p"; "p" |] in
      let kinds = [|
        Snd ("Int", Some 1);
        Rcv ("Bool", Some 0)
      |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let expected =
        LRec (0,
          LSend ("p","Int",
            LRecv ("p","Bool", LVar (0, Loc.dummy), Loc.dummy),
          Loc.dummy),
        Loc.dummy)
      in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "Mutual recursion: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );

    (* Nested recursion introduced via choice branch *)
    Alcotest.test_case "Nested rec via choice" `Quick (fun () ->
      let roles = [| "r"; "r"; "r" |] in
      let kinds = [|
        Int [ ("L", Some 1); ("R", Some 2) ];
        Snd ("Int", Some 0);          (* loops to 0 -> forms SCC of 0,1 *)
        Snd ("Bool", Some 2)          (* self-loop -> its own rec *)
      |] in
      let g = mk_graph ~roles ~kinds ~start_state:(Some 0) in
      let lt = Automaton_to_local.automaton_to_local g in
      let outer_body =
        LInt ("r", [
          ("L", LSend ("r","Int", LVar (0, Loc.dummy), Loc.dummy));
          ("R", LRec (1, LSend ("r","Bool", LVar (1, Loc.dummy), Loc.dummy), Loc.dummy))
        ], Loc.dummy) in
      let expected = LRec (0, outer_body, Loc.dummy) in
      if not (Alpha_equiv.local (=) lt expected) then
        Alcotest.failf "Nested rec via choice: expected %s but got %s"
          (Pretty.string_of_local Format.pp_print_int expected)
          (Pretty.string_of_local Format.pp_print_int lt)
    );
  ]


(* ------------------------------------------------------------------ *)
(* Projection tests                                                   *)
(* ------------------------------------------------------------------ *)

let projection_tests =
  let open Local_automaton in

  (* Helper functions *)
  let expect_error g role =
    match Projection.project g role with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "Projection unexpectedly succeeded"
  in

  (* Legacy alias for tests that still expect option *)
  let project g r = match Projection.project g r with Ok l -> Some l | Error _ -> None in

  let mk_expected_send role base =
    { num_states = 1;
      start_state = Some 0;
      roles = [| role |];
      kinds = [| Snd (base, None) |] }
  in
  let mk_expected_recv role base =
    { num_states = 1;
      start_state = Some 0;
      roles = [| role |];
      kinds = [| Rcv (base, None) |] }
  in

  let test_send () =
    let g_str = "a -> b : [Int]; end" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "a" with
    | None -> Alcotest.fail "Projection returned None for sender"
    | Some l ->
        let expected = mk_expected_send "b" "Int" in
        Alcotest.(check int) "num states" expected.num_states l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        Alcotest.(check string) "role" expected.roles.(0) l.roles.(0);
        (match l.kinds.(0) with
         | Snd (b, None) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "Expected send kind")
  in

  let test_recv () =
    let g_str = "a -> b : [Int]; end" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "b" with
    | None -> Alcotest.fail "Projection returned None for receiver"
    | Some l ->
        let expected = mk_expected_recv "a" "Int" in
        Alcotest.(check int) "num states" expected.num_states l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        Alcotest.(check string) "role" expected.roles.(0) l.roles.(0);
        (match l.kinds.(0) with
         | Rcv (b, None) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "Expected recv kind")
  in

  let test_absent_participant () =
    let g_str = "a -> b : [Int]; end" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "c" with
    | None -> Alcotest.fail "Projection unexpectedly failed for absent role"
    | Some l ->
        Alcotest.(check int) "num states" 0 l.num_states;
        (match l.start_state with None -> () | _ -> Alcotest.fail "expected no start state")
  in

  let test_recursive_send () =
    let g_str = "rec t. a -> b : [Int]; t" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "a" with
    | None -> Alcotest.fail "Projection returned None for recursive sender"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Snd (b, Some 0) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "expected looping send")
  in

  let test_uninvolved_recursion () =
    let g_str = "rec t. a -> b : [Int]; t" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "c" with
    | None -> Alcotest.fail "Projection failed for uninvolved participant in recursion"
    | Some l ->
        Alcotest.(check int) "num states" 0 l.num_states;
        (match l.start_state with None -> () | _ -> Alcotest.fail "expected no start state")
  in

  let test_mixed_participant () =
    let g_str = "a -> b : [Int]; b -> c : [Bool]; end" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "b" with
    | None -> Alcotest.fail "Projection returned None for mixed participant"
    | Some l ->
        (* Should have 2 states: one for receiving Int, one for sending Bool *)
        Alcotest.(check int) "num states" 2 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (* Check first state (receive) *)
        (match l.kinds.(0) with
         | Rcv (b, Some 1) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "Expected receive kind in first state");
        (* Check second state (send) *)
        (match l.kinds.(1) with
         | Snd (b, None) -> Alcotest.(check string) "base" "Bool" b
         | _ -> Alcotest.fail "Expected send kind in second state")
  in

  let test_projected_behavior () =
    let g_str = "a -> b : [Int]; b -> c : [Bool]; c -> a : [String]; end" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "b" with
    | None -> Alcotest.fail "Projection returned None for projected behavior"
    | Some l ->
        (* Should have 2 states: one for receiving Int, one for sending Bool *)
        (* The c -> a transition should be completely ignored by b's projection *)
        Alcotest.(check int) "num states" 2 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (* Check first state (receive from a) *)
        (match l.kinds.(0) with
         | Rcv (b, Some 1) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "Expected receive kind in first state");
        (* Check second state (send to c) *)
        (match l.kinds.(1) with
         | Snd (b, None) -> Alcotest.(check string) "base" "Bool" b
         | _ -> Alcotest.fail "Expected send kind in second state")
  in

  let test_branching_knowledge () =
    let g_str = "r -> s { L: s -> p : [Int]; end, R: s -> p : [Int]; end }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for branching knowledge set"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Rcv (b, None) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "expected receive kind")
  in

  let test_complex_branching () =
    let g_str = "r -> s { L: rec t. s -> p : [Int]; t, R: rec t. s -> p : [Int]; s -> p : [Int]; t }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for complex branching"
    | Some l ->
        Alcotest.(check int) "num states" 2 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        let check_state idx expected_dest =
          match l.kinds.(idx) with
          | Local_automaton.Rcv (b, dest_opt) ->
              Alcotest.(check string) (Printf.sprintf "base %d" idx) "Int" b;
              (match dest_opt with
               | Some d -> Alcotest.(check int) (Printf.sprintf "dest %d" idx) expected_dest d
               | None -> Alcotest.fail (Printf.sprintf "state %d should have dest" idx))
          | _ -> Alcotest.fail (Printf.sprintf "state %d expected receive" idx)
        in
        check_state 0 1;
        check_state 1 0
  in

  let test_branching_recursion_loop () =
    let g_str = "rec t1. r -> s { L: s -> p : [Int]; t1, R: rec t2. s -> p : [Int]; t2 }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for branching recursion loop"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Rcv (b, Some 0) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "expected looping receive")
  in

  let test_parallel_unrelated () =
    let g_str = "(a -> p : [Int]; end) | (r -> s : [Bool]; end)" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for parallel unrelated"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Rcv (b, None) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "expected receive kind")
  in

  let test_parallel_recursion () =
    let g_str = "(rec t. a -> p : [Int]; t) | (r -> s : [Bool]; end)" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for parallel recursion"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Rcv (b, Some 0) -> Alcotest.(check string) "base" "Int" b
         | _ -> Alcotest.fail "expected looping receive")
  in

  let test_unproj_send_recv_conflict () =
    let g_str = "r -> s { L: p -> q : [Int]; end, R: q -> p : [Int]; end }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    expect_error g_aut "p"
  in

  let test_unproj_base_conflict () =
    let g_str = "r -> s { L: p -> q : [Int]; end, R: p -> q : [Bool]; end }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    expect_error g_aut "p"
  in

  (* New test: participant is sender in branching – should yield internal choice *)
  let test_internal_choice_sender () =
    let g_str = "p -> q { L: end, R: end }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for internal sender branching"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.start_state with Some 0 -> () | _ -> Alcotest.fail "wrong start state");
        (match l.kinds.(0) with
         | Local_automaton.Int branches ->
             let labels = List.map fst branches |> List.sort String.compare in
             Alcotest.(check (list string)) "labels" ["L"; "R"] labels;
             List.iter (fun (_, dest_opt) ->
               match dest_opt with
               | None -> ()
               | Some _ -> Alcotest.fail "branch dest should be None") branches
         | _ -> Alcotest.fail "expected internal choice")
  in

  (* Complex test: combines recursion, parallel composition, uninvolved transitions, and branching *)
  let test_complex_mixed_scenario () =
    let g_str = "(rec t. p -> q { L: r -> s : [Int]; t, R: end }) | (a -> b : [Bool]; c -> d : [String]; end)" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection returned None for complex mixed scenario"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        let start_idx = match l.start_state with Some i -> i | _ -> Alcotest.fail "wrong start state" in
        Printf.eprintf "DEBUG: local automaton structure:\n";
        for i = 0 to l.num_states - 1 do
          Printf.eprintf "DEBUG: local state %d role=%s kind=" i l.roles.(i);
          (match l.kinds.(i) with
           | Local_automaton.Int branches ->
               Printf.eprintf "Int([%s])" (String.concat ";" (List.map (fun (lbl, d) -> Printf.sprintf "%s->%s" lbl (match d with Some j -> string_of_int j | None -> "end")) branches))
           | Local_automaton.Rcv (b, d) -> Printf.eprintf "Rcv(%s,%s)" b (match d with Some j -> string_of_int j | None -> "end")
           | Local_automaton.Snd (b, d) -> Printf.eprintf "Snd(%s,%s)" b (match d with Some j -> string_of_int j | None -> "end")
           | Local_automaton.Ext _ -> Printf.eprintf "Ext..."
          );
          Printf.eprintf "\n";
        done;
        (match l.kinds.(start_idx) with
         | Local_automaton.Int branches ->
             let labels = List.map fst branches |> List.sort String.compare in
             Alcotest.(check (list string)) "labels" ["L"; "R"] labels;
             let l_branch = List.find (fun (lbl, _) -> lbl = "L") branches in
             (match l_branch with
              | ("L", Some idx) when idx = start_idx -> ()
              | _ -> Alcotest.fail "L branch should loop to start state");
             let r_branch = List.find (fun (lbl, _) -> lbl = "R") branches in
             (match r_branch with
              | ("R", None) -> ()
              | _ -> Alcotest.fail "R branch should lead to end")
         | _ -> Alcotest.fail "expected internal choice in first state")
  in

  let test_receive_branching_ok () =
    let g_str = "r -> s { L: s -> p { A: end }, R: s -> p { B: end } }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    match project g_aut "p" with
    | None -> Alcotest.fail "Projection failed for receive branching ok"
    | Some l ->
        Alcotest.(check int) "num states" 1 l.num_states;
        (match l.kinds.(0) with
         | Local_automaton.Ext branches ->
             let labels = List.map fst branches |> List.sort String.compare in
             Alcotest.(check (list string)) "labels" ["A";"B"] labels
         | _ -> Alcotest.fail "expected external choice")
  in

  let test_receive_branching_conflict () =
    let g_str = "r -> s { L: p -> s { A: end }, R: p -> s { B: end } }" in
    let g_enc = Encode.encode (parse_global g_str) in
    let g_aut = Automaton.of_global g_enc in
    expect_error g_aut "p"
  in

  [ Alcotest.test_case "sender projection" `Quick test_send;
    Alcotest.test_case "receiver projection" `Quick test_recv;
    Alcotest.test_case "absent participant" `Quick test_absent_participant;
    Alcotest.test_case "recursive sender" `Quick test_recursive_send;
    Alcotest.test_case "uninvolved recursion" `Quick test_uninvolved_recursion;
    Alcotest.test_case "mixed participant" `Quick test_mixed_participant;
    Alcotest.test_case "projected behavior" `Quick test_projected_behavior;
    Alcotest.test_case "branching knowledge" `Quick test_branching_knowledge;
    Alcotest.test_case "complex branching" `Quick test_complex_branching;
    Alcotest.test_case "branching recursion loop" `Quick test_branching_recursion_loop;
    Alcotest.test_case "parallel unrelated" `Quick test_parallel_unrelated;
    Alcotest.test_case "parallel recursion" `Quick test_parallel_recursion;
    Alcotest.test_case "unprojectable send/recv conflict" `Quick test_unproj_send_recv_conflict;
    Alcotest.test_case "unprojectable base conflict" `Quick test_unproj_base_conflict;
    Alcotest.test_case "internal sender branching" `Quick test_internal_choice_sender;
    Alcotest.test_case "complex mixed scenario" `Quick test_complex_mixed_scenario;
    Alcotest.test_case "receive branching ok" `Quick test_receive_branching_ok;
    Alcotest.test_case "receive branching conflict" `Quick test_receive_branching_conflict ]

let () =
  Alcotest.run "Global-type utilities"
    [ "well-formedness", wf_tests
    ; "encode",          encode_tests
    ; "automaton",       automaton_tests
    ; "balance",         balance_tests
    ; "local-types",     local_type_tests
    ; "global-msg",      [global_msg_test]
    ; "local-automaton", local_automaton_tests
    ; "live",            live_tests
    ; "automaton-to-global", automaton_to_global_tests
    ; "automaton-to-local",  automaton_to_local_tests
    ; "projection",          projection_tests
    ]
