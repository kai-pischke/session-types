(*--------------------------------------------------------------------*)
(*  Unit‑tests for the balance module                                  *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

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

let suite = ("balance", balance_tests) 