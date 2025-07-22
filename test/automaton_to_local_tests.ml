(*--------------------------------------------------------------------*)
(*  Unit‑tests for automaton to local conversion                       *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let automaton_to_local_tests =
  let open Projection in
  
  (* Helper to test automaton to local conversion *)
  let test_conversion name src role =
    test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let aut = Automaton.of_global g' in
      match project aut role with
      | Ok result ->
          let pretty = Local_automaton.string_of_graph result in
          (* For now, just check that conversion succeeds *)
          Alcotest.(check bool) "conversion succeeds" true (pretty <> "")
      | Error msg ->
          Alcotest.failf "projection failed: %s" msg
    )
  in
  
  [ test_conversion "LSend simple" "a -> b : [Int]; end" "a"
  ; test_conversion "Internal choice" "a -> b { Ok: end, Err: end }" "a"
  ; test_conversion "Recursion loop" "rec t. a -> b { Ok: t }" "a"
  ; test_conversion "External choice mixed" "a -> b { Ok: end, Err: a -> c { X: end } }" "a"
  ; test_conversion "Mutual send/recv recursion" "rec t. a -> b { Ok: b -> a { Ack: t } }" "a"
  ; test_conversion "Nested rec via choice" "rec t. a -> b { Ok: c -> d { X: t }, Err: end }" "a"
  ]

let suite = ("automaton-to-local", automaton_to_local_tests) 