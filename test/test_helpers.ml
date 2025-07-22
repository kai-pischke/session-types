(*--------------------------------------------------------------------*)
(*  Common test utilities and helpers                                  *)
(*--------------------------------------------------------------------*)
open Alcotest
open Lexing

(* ─── Generic parsers ─────────────────────────────────────────────── *)
let parse_global (src : string) : string Ast.global =
  let lb = from_string src in
  Parser.gfile Lexer.token lb

let parse_local (src : string) : string Ast.local =
  let lb = from_string src in
  Parser.lfile Lexer.token lb

(* ─── Alcotest helpers ────────────────────────────────────────────── *)
(* Expect the given thunk to raise [Well_formed.Error].               *)
let expect_error name thunk =
  test_case name `Quick (fun () ->
      match thunk () with
      | () -> fail "Expected Well_formed.Error, but check succeeded"
      | exception Well_formed.Error _ -> ())

(* Positive test: checker should succeed.                             *)
let expect_ok name thunk =
  test_case name `Quick (fun () -> thunk ())

(* ─── Convenience constructors for local types ────────────────────── *)
let send peer base cont = Ast.LSend (peer, base, cont, Loc.dummy)
let recv peer base cont = Ast.LRecv (peer, base, cont, Loc.dummy)
let mk_end = Ast.LEnd Loc.dummy

(* ─── Synthesis helpers ────────────────────────────────────────────── *)
let build_part role local =
  { Synthesis.role = role; laut = Local_automaton.of_local local }