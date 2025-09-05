(*--------------------------------------------------------------------*)
(*  Unit‑tests for the connected module                                 *)
(*--------------------------------------------------------------------*)
open Alcotest

module Connected_tests = struct
  open Connected
  open Local_automaton

  (* Helper to create a simple automaton for testing *)
  let make_simple_automaton num_states start_state roles kinds =
    { num_states; start_state; roles; kinds }

  (* Helper to create automaton that sends to specific role *)
  let make_send_automaton target_role =
    make_simple_automaton 1 (Some 0) 
      [| target_role |] 
      [| Snd ("test", None) |]

  (* Helper to create automaton that receives from specific role *)
  let make_recv_automaton target_role =
    make_simple_automaton 1 (Some 0) 
      [| target_role |] 
      [| Rcv ("test", None) |]

  (* Helper to create automaton with internal choice *)
  let make_choice_automaton target_role =
    make_simple_automaton 1 (Some 0) 
      [| target_role |] 
      [| Int [("ok", None); ("err", None)] |]

  (* Helper to create empty automaton (terminated) *)
  let make_empty_automaton () =
    make_simple_automaton 0 None [| |] [| |]

  (* Helper to create chain automaton: state 0 -> state 1 with different roles *)
  let make_chain_automaton role1 role2 =
    make_simple_automaton 2 (Some 0) 
      [| role1; role2 |] 
      [| Snd ("msg1", Some 1); Snd ("msg2", None) |]

  (* Test helper to check if two configurations are equivalent *)
  let config_equal config1 config2 =
    Array.length config1 = Array.length config2 &&
    Array.for_all2 (fun a b -> a = b) config1 config2

  (* Test helper to check if a list of configurations forms a proper partition *)
  let is_valid_partition original_config result_configs =
    let n = Array.length original_config in
    let covered = Array.make n false in
    
    (* Check each result config *)
    List.for_all (fun config ->
      (* Check that config has right length *)
      Array.length config = n &&
      (* Check that non-None values match original and mark as covered *)
      Array.mapi (fun i state ->
        match state with
        | None -> true
        | Some _ -> 
            if covered.(i) then false (* already covered by another config *)
            else (
              covered.(i) <- true;
              state = original_config.(i)
            )
      ) config |> Array.for_all (fun x -> x)
    ) result_configs &&
    (* Check that all non-None positions in original are covered *)
    Array.mapi (fun i state ->
      match state with
      | None -> true
      | Some _ -> covered.(i)
    ) original_config |> Array.for_all (fun x -> x)

  let tests = [
    test_case "Single participant - trivial case" `Quick (fun () ->
      let cfsm = [| make_empty_automaton () |] in
      let config = [| None |] in
      let result = split cfsm config in
      check int "number of components" 0 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Two disconnected participants" `Quick (fun () ->
      let cfsm = [| make_empty_automaton (); make_empty_automaton () |] in
      let config = [| None; None |] in
      let result = split cfsm config in
      check int "number of components" 0 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Two participants - one sends to other" `Quick (fun () ->
      (* Participant 0 sends to participant 1 *)
      let cfsm = [| make_send_automaton 1; make_recv_automaton 0 |] in
      let config = [| Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "valid partition" true (is_valid_partition config result);
      (* Both participants should be in the same component *)
      match result with
      | [config'] -> 
          check bool "both participants present" true 
            (config'.(0) = Some 0 && config'.(1) = Some 0)
      | _ -> fail "Expected exactly one component"
    );

    test_case "Three participants - linear chain" `Quick (fun () ->
      (* 0 -> 1 -> 2 (chain communication) *)
      let cfsm = [| 
        make_send_automaton 1;    (* 0 sends to 1 *)
        make_chain_automaton 0 2; (* 1 receives from 0, sends to 2 *)
        make_recv_automaton 1     (* 2 receives from 1 *)
      |] in
      let config = [| Some 0; Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Four participants - two separate pairs" `Quick (fun () ->
      (* (0 <-> 1) and (2 <-> 3) are separate *)
      let cfsm = [| 
        make_send_automaton 1;  (* 0 sends to 1 *)
        make_recv_automaton 0;  (* 1 receives from 0 *)
        make_send_automaton 3;  (* 2 sends to 3 *)
        make_recv_automaton 2   (* 3 receives from 2 *)
      |] in
      let config = [| Some 0; Some 0; Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 2 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Mixed terminated and active participants" `Quick (fun () ->
      let cfsm = [| 
        make_empty_automaton ();  (* 0 terminated *)
        make_send_automaton 2;    (* 1 sends to 2 *)
        make_recv_automaton 1;    (* 2 receives from 1 *)
        make_empty_automaton ()   (* 3 terminated *)
      |] in
      let config = [| None; Some 0; Some 0; None |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Internal choice creates connections" `Quick (fun () ->
      (* Participant 0 has internal choice involving participant 1 *)
      let cfsm = [| 
        make_choice_automaton 1;  (* 0 has choice involving 1 *)
        make_recv_automaton 0     (* 1 can receive from 0 *)
      |] in
      let config = [| Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Empty configuration" `Quick (fun () ->
      let cfsm = [| make_empty_automaton (); make_empty_automaton () |] in
      let config = [| None; None |] in
      let result = split cfsm config in
      check int "number of components" 0 (List.length result);
      check bool "valid partition" true (is_valid_partition config result);
      (* Each component should contain one participant with None state *)
      List.iter (fun comp ->
        let active_count = Array.fold_left (fun acc state ->
          match state with None -> acc | Some _ -> acc + 1
        ) 0 comp in
        check int "active participants per component" 0 active_count
      ) result
    );

    test_case "Large disconnected group" `Quick (fun () ->
      (* 6 participants, all disconnected *)
      let cfsm = Array.init 6 (fun _ -> make_empty_automaton ()) in
      let config = Array.make 6 None in
      let result = split cfsm config in
      check int "number of components" 0 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Complex connectivity pattern" `Quick (fun () ->
      (* Complex pattern: 0-1-2 connected, 3-4 connected *)
      let cfsm = [| 
        make_send_automaton 1;    (* 0 -> 1 *)
        make_chain_automaton 0 2; (* 1 <-> 0, 1 -> 2 *)
        make_recv_automaton 1;    (* 2 <- 1 *)
        make_send_automaton 4;    (* 3 -> 4 *)
        make_recv_automaton 3;    (* 4 <- 3 *)
        make_empty_automaton ()   (* 5 alone *)
      |] in
      let config = [| Some 0; Some 0; Some 0; Some 0; Some 0; None |] in
      let result = split cfsm config in
      check int "number of components" 2 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Bidirectional communication" `Quick (fun () ->
      (* Test that both directions of communication are detected *)
      let bidirectional_auto role1 role2 = 
        make_simple_automaton 2 (Some 0)
          [| role1; role2 |]
          [| Snd ("msg", Some 1); Rcv ("resp", None) |] in
      
      let cfsm = [| 
        bidirectional_auto 1 0;  (* 0 sends to 1, then receives from 1 *)
        bidirectional_auto 0 1   (* 1 receives from 0, then sends to 0 *)
      |] in
      let config = [| Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "valid partition" true (is_valid_partition config result)
    );

    test_case "Choice connectivity - internal choice" `Quick (fun () ->
      (* Test that internal choice (Int) creates connectivity with target role *)
      let choice_auto _role target_role = 
        make_simple_automaton 2 (Some 0)
          [| target_role; -1 |]  (* State 0 targets role, state 1 is dummy *)
          [| Int [("choice1", Some 1); ("choice2", None)]; Int [] |] in
      
      let cfsm = [| 
        choice_auto 0 1;  (* Participant 0 makes choice for participant 1 *)
        choice_auto 1 0   (* Participant 1 makes choice for participant 0 *)
      |] in
      let config = [| Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "both participants connected" true 
        (List.exists (fun comp -> 
          let active_count = Array.fold_left (fun acc state_opt -> 
            match state_opt with Some _ -> acc + 1 | None -> acc
          ) 0 comp in
          active_count = 2
        ) result)
    );

    test_case "Choice connectivity - external choice" `Quick (fun () ->
      (* Test that external choice (Ext) creates connectivity with target role *)
      let ext_choice_auto _role target_role = 
        make_simple_automaton 2 (Some 0)
          [| target_role; -1 |]
          [| Ext [("option1", Some 1); ("option2", None)]; Ext [] |] in
      
      let cfsm = [| 
        ext_choice_auto 0 1;  (* Participant 0 waits for choice from participant 1 *)
        ext_choice_auto 1 0   (* Participant 1 waits for choice from participant 0 *)
      |] in
      let config = [| Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "both participants connected" true 
        (List.exists (fun comp -> 
          let active_count = Array.fold_left (fun acc state_opt -> 
            match state_opt with Some _ -> acc + 1 | None -> acc
          ) 0 comp in
          active_count = 2
        ) result)
    );

    test_case "Mixed message and choice connectivity" `Quick (fun () ->
      (* Test scenario: p sends message to q, then q makes choice for r *)
      let p_auto = make_simple_automaton 1 (Some 0) [| 1 |] [| Snd ("data", None) |] in
      let q_auto = make_simple_automaton 2 (Some 0) [| 0; 2 |] [| Rcv ("data", Some 1); Int [("yes", None); ("no", None)] |] in
      let r_auto = make_simple_automaton 1 (Some 0) [| 1 |] [| Ext [("yes", None); ("no", None)] |] in
      
      let cfsm = [| p_auto; q_auto; r_auto |] in
      let config = [| Some 0; Some 0; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "all three participants connected" true 
        (List.exists (fun comp -> 
          let active_count = Array.fold_left (fun acc state_opt -> 
            match state_opt with Some _ -> acc + 1 | None -> acc
          ) 0 comp in
          active_count = 3
        ) result)
    );

    test_case "Three-party choice bug regression" `Quick (fun () ->
      (* Regression test for the specific bug: after message exchange, 
         choice states should maintain connectivity *)
      
      (* Simulate the book quote scenario at config [0:1; 1:2; 2:3] *)
      let b2_auto = make_simple_automaton 2 (Some 1) [| -1; 1 |] [| Rcv ("quote", Some 1); Int [("ok", None); ("quit", None)] |] in
      let s_auto = make_simple_automaton 3 (Some 2) [| -1; -1; 0 |] [| Rcv ("title", Some 1); Snd ("quote", Some 2); Ext [("ok", None); ("quit", None)] |] in  
      let b1_auto = make_simple_automaton 4 (Some 3) [| -1; -1; -1; 1 |] [| Snd ("title", Some 1); Rcv ("quote", Some 2); Snd ("quote", Some 3); Ext [("ok", None); ("quit", None)] |] in
      
      let cfsm = [| b2_auto; s_auto; b1_auto |] in
      let config = [| Some 1; Some 2; Some 3 |] in  (* The critical configuration *)
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      check bool "all participants still connected" true 
        (List.exists (fun comp -> 
          let active_count = Array.fold_left (fun acc state_opt -> 
            match state_opt with Some _ -> acc + 1 | None -> acc
          ) 0 comp in
          active_count = 3
        ) result)
    );

    test_case "Choice with terminated participant" `Quick (fun () ->
      (* Test that terminated participants are correctly excluded *)
      let active_choice_auto = make_simple_automaton 2 (Some 0) [| 1; -1 |] [| Int [("choice", Some 1)]; Int [] |] in
      let terminated_auto = make_simple_automaton 1 None [| -1 |] [| Ext [] |] in
      let target_auto = make_simple_automaton 1 (Some 0) [| 0 |] [| Ext [("choice", None)] |] in
      
      let cfsm = [| active_choice_auto; terminated_auto; target_auto |] in
      let config = [| Some 0; None; Some 0 |] in
      let result = split cfsm config in
      check int "number of components" 1 (List.length result);
      (* Only participant 0 and 2 should be connected, 1 is terminated *)
      check bool "correct participants connected" true 
        (List.exists (fun comp -> 
          let active_count = Array.fold_left (fun acc state_opt -> 
            match state_opt with Some _ -> acc + 1 | None -> acc
          ) 0 comp in
          active_count = 2 && comp.(0) <> None && comp.(2) <> None && comp.(1) = None
        ) result)
    );
  ]
end

let suite = ("Connected", Connected_tests.tests)
