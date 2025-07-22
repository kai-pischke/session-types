(*--------------------------------------------------------------------*)
(*  Unit‑tests for automaton to global conversion                      *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let automaton_to_global_tests =
  let open Automaton_to_global in
  
  (* Helper to test automaton to global conversion *)
  let test_conversion name src =
    test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let aut = Automaton.of_global g' in
      let result = automaton_to_global aut in
      let pretty = Pretty.string_of (fun fmt i -> Format.fprintf fmt "%d" i) result in
      (* For now, just check that conversion succeeds *)
      Alcotest.(check bool) "conversion succeeds" true (pretty <> "")
    )
  in
  
  [ test_conversion "GEnd" "end"
  ; test_conversion "GMsg simple" "a -> b : [Int]; end"
  ; test_conversion "GBra simple" "a -> b { Ok: end, Err: end }"
  ; test_conversion "GPar simple" "a -> b { Ok: end } | c -> d { X: end }"
  ; test_conversion "GPar nested" "a -> b { Ok: c -> d { X: end } | e -> f { Y: end } }"
  ; test_conversion "GRec simple" "rec t. a -> b { Ok: t }"
  ; test_conversion "GBra with recursion" "rec t. a -> b { Ok: end, Err: t }"
  ; test_conversion "GPar with recursion" "(rec t. a -> b { Ok: t }) | (c -> d { X: end })"
  ; test_conversion "GPar both sides recursive" "(rec t. a -> b { Ok: t }) | (rec u. c -> d { X: u })"
  ; test_conversion "Complex nested structure" "rec t. a -> b { Ok: c -> d { X: e -> f { Y: t } } }"
  ]

let suite = ("automaton-to-global", automaton_to_global_tests) 