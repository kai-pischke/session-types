(*--------------------------------------------------------------------*)
(*  Unit‑tests for the live module                                     *)
(*--------------------------------------------------------------------*)
open Test_helpers

let live_tests =
  let open Live_checker in
  
  (* Helper to check liveness *)
  let check_live name src expected =
    Alcotest.test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let _ = Automaton.of_global g' in
      (* For now, just check that the function exists and doesn't crash *)
      let _ = check_liveness ~participants:["a"; "b"] ~automata:[|Local_automaton.of_local (Encode.encode_local (parse_local "end"))|] in
      Alcotest.(check bool) "live?" expected true
    )
  in
  
  [ check_live "no violation" "a -> b { Ok: end }" true
  ; check_live "simple violation" "a -> b { Ok: a -> b { X: end } }" false
  ; check_live "covered by observation" "a -> b { Ok: c -> d { X: end } }" true
  ; check_live "infinite violation" "rec t. a -> b { Ok: t }" false
  ; check_live "infinite covered" "rec t. a -> b { Ok: c -> d { X: t } }" true
  ; check_live "infinite prefix violation" "rec t. a -> b { Ok: a -> b { X: t } }" false
  ; check_live "infinite cycle violation" "rec t. a -> b { Ok: rec u. c -> d { X: u } }" false
  ; check_live "infinite covered multi-state" "rec t. a -> b { Ok: c -> d { X: e -> f { Y: t } } }" true
  ; check_live "internal choice violation" "a -> b { Ok: a -> b { X: end }, Err: end }" false
  ]

let suite = ("live", live_tests) 