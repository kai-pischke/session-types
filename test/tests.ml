(*--------------------------------------------------------------------*)
(*  Unit‑tests for the well‑formedness checker                        *)
(*--------------------------------------------------------------------*)
open Alcotest
open Lexing

(* Small helper to parse a global type from a string *)
let parse_global (src : string) : Ast.global =
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

let parallel_valid () =
  let g = parse_global "a -> b { X: end } | c -> d { Y: end }" in
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

let tests =
  [ expect_ok    "simple valid"           simple_valid
  ; expect_ok    "parallel valid"         parallel_valid
  ; expect_error "self‑messaging"         self_msg_invalid
  ; expect_error "unguarded recursion"    unguarded_rec_invalid
  ; expect_error "unguarded nested recursion" nested_unguarded_rec_invalid
  ; expect_error "overlapping parallel"   overlapping_parallel_invalid
  ; expect_error "unbound recursion var"  unbound_var_invalid
  ]

let () =
  Alcotest.run "Well‑formedness" [ "global", tests ]
