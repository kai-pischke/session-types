(*--------------------------------------------------------------------*)
(*  Unit‑tests for the automaton module                                *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

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

let suite = ("automaton", automaton_tests) 