(*--------------------------------------------------------------------*)
(*  Main test runner - imports all test suites                         *)
(*--------------------------------------------------------------------*)

let suites =
  [ Well_formedness_tests.suite
  ; Encode_tests.suite
  ; Automaton_tests.suite
  ; Balance_tests.suite
  ; Local_type_tests.suite
  ; Global_msg_tests.suite
  ; Local_automaton_tests.suite
  (* ; Live_tests.suite *)
  ; Automaton_to_global_tests.suite
  ; Automaton_to_local_tests.suite
  ; Projection_tests.suite
  ; Synthesis_tests.suite
  ]

let () = Alcotest.run "Global-type utilities" suites
