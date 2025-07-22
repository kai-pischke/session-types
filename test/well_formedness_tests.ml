(*--------------------------------------------------------------------*)
(*  Unit‑tests for the well‑formedness checker                        *)
(*--------------------------------------------------------------------*)
open Test_helpers

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

let suite = ("well-formedness", wf_tests) 