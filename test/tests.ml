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

module IS = Set.Make (Int)

(* Collect the set of every id occurring in a global type *)
let rec collect_ids acc : int Ast.global -> IS.t = function
  | GEnd _                        -> acc
  | GVar (v,_)                    -> IS.add v acc
  | GRec (v,body,_)               -> collect_ids (IS.add v acc) body
  | GMsg (_,_,_,cont,_)           -> collect_ids acc cont
  | GBra (_,_,brs,_)              ->
      List.fold_left (fun s (_,g)-> collect_ids s g) acc brs
  | GPar (g1,g2,_)                -> collect_ids (collect_ids acc g1) g2

(* Assert that a set of integers is exactly {0,…,n-1}. *)
let assert_consecutive ids =
  let as_list = IS.elements ids in
  let rec check i = function
    | []        -> ()
    | x::xs when x = i -> check (i+1) xs
    | _         -> fail "Variables are not consecutive starting at 0"
  in
  check 0 as_list

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

let rec_end_is_zero () =
  let g   = parse_global "rec t. end" in
  let g'  = Encode.encode g in
  match g' with
  | GRec (0, GEnd _, _) -> ()
  | _ -> fail "Expected [rec 0. end] after encoding"

let fused_adjacent_recs () =
  let g   = parse_global "rec x. rec y. end" in
  let g'  = Encode.encode g in
  match g' with
  | GRec (0, GEnd _, _) -> ()
  | _ -> fail "Adjacent [rec]s should have been fused into one"

let ids_are_consecutive () =
  let g   = parse_global
      "a -> b { X: end } | rec t. c -> d { Y: t }" in
  let g'  = Encode.encode g in
  let ids = collect_ids IS.empty g' in
  assert_consecutive ids

let hard_encode () =
  (* A single, deliberately baroque global-type source.                *)
  let g = parse_global {|
    rec x.
       a -> b { L1:
            rec y.
              c -> d { L2: y
                     , L3: rec y. end }    
          }
      | rec z.
          e -> f { L4: z
                 , L5: x }   
      | b -> a { L6: rec t.
                     g -> h { L7: t }
                   | rec u. t   
               }
  |} in

  let g' = Encode.encode g in

  (* Pretty-print both versions to stdout for debugging *)
  Format.printf "@.Original: %s@."
    (Pretty.string_of Format.pp_print_string g);
  Format.printf "@.Encoded : %s@."
    (Pretty.string_of Format.pp_print_int g');

  (* same assertions as before *)
  let ids = collect_ids IS.empty g' in
  assert_consecutive ids
;;

(*--------------------------------------------------------------------*)
(*  Test-suite registration                                           *)
(*--------------------------------------------------------------------*)

let wf_tests =
  [ expect_ok    "simple valid"           simple_valid
  ; expect_ok    "parallel valid"         parallel_valid
  ; expect_error "self-messaging"         self_msg_invalid
  ; expect_error "unguarded recursion"    unguarded_rec_invalid
  ; expect_error "unguarded nested recursion" nested_unguarded_rec_invalid
  ; expect_error "overlapping parallel"   overlapping_parallel_invalid
  ; expect_error "unbound recursion var"  unbound_var_invalid
  ]

let encode_tests =
  [ expect_ok "encode: rec-end becomes 0"   rec_end_is_zero
  ; expect_ok "encode: rec-rec fuses"       fused_adjacent_recs
  ; expect_ok "encode: ids consecutive"     ids_are_consecutive
  ; expect_ok "encode: hard test passes"    hard_encode
  ]

let automaton_tests =
  let open Automaton in
  let test_case_graph name src expected_str =
    test_case name `Quick (fun () ->
      let g = parse_global src in
      let g' = Encode.encode g in
      let aut = of_global g' in
      let pretty = string_of_graph aut in
      Alcotest.(check string) "automaton pretty" expected_str pretty
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
      "Nested recursion in parallel branch"
      "rec t. a -> b { Ok: rec u. c -> d { X: u, Y: t } | e -> f { Z: end } }"
      "States: 0, 1, 2, 3\nStart states: 1\nstate 0 (_,_)\nstate 1 (a,b)\nstate 2 (c,d)\nstate 3 (e,f)\nfrom 1 --label Ok--> 2,3\nfrom 2 --label X--> 0\nfrom 2 --label Y--> 1\nfrom 3 --label Z--> \n"
  ;
    test_case_graph
      "Branch successor present"
      "rec t. a -> b { X: c -> d { Z: end }, Y : t }"
      "States: 0, 1, 2\nStart states: 0\nstate 0 (a,b)\nstate 1 (c,d)\nstate 2 (a,b)\nfrom 0 --label X--> 1\nfrom 0 --label Y--> 2\nfrom 1 --label Z--> \nfrom 2 --label X--> 1\nfrom 2 --label Y--> 2\n"
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
  ; check_balanced "unbalanced terminating branch" "rec t. a -> b { X: c -> d { Z: end }, Y : t }" false
  ]

let () =
  Alcotest.run "Global-type utilities"
    [ "well-formedness", wf_tests
    ; "encode",          encode_tests
    ; "automaton",       automaton_tests
    ; "balance",         balance_tests
    ]
