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
  ; expect_error "self‑messaging"         self_msg_invalid
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

let () =
  Alcotest.run "Global-type utilities"
    [ "well-formedness", wf_tests
    ; "encode",          encode_tests
    ]
