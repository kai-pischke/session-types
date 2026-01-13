
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SEMI
    | RPAREN
    | REC
    | RBRACK
    | RBRACE
    | QUEST
    | LPAREN
    | LBRACK
    | LBRACE
    | IDENT of (
# 5 "src/parser.mly"
       (string)
# 24 "src/parser.ml"
  )
    | EOF
    | END
    | DOT
    | COMMA
    | COLON
    | BAR
    | BANG
    | ARROW
  
end

include MenhirBasics

# 1 "src/parser.mly"
  
open Ast

# 43 "src/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_gfile) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: gfile. *)

  | MenhirState01 : (('s, _menhir_box_gfile) _menhir_cell1_REC, _menhir_box_gfile) _menhir_state
    (** State 01.
        Stack shape : REC.
        Start symbol: gfile. *)

  | MenhirState04 : ((('s, _menhir_box_gfile) _menhir_cell1_REC, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 04.
        Stack shape : REC ident.
        Start symbol: gfile. *)

  | MenhirState05 : (('s, _menhir_box_gfile) _menhir_cell1_LPAREN, _menhir_box_gfile) _menhir_state
    (** State 05.
        Stack shape : LPAREN.
        Start symbol: gfile. *)

  | MenhirState08 : (('s, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 08.
        Stack shape : ident.
        Start symbol: gfile. *)

  | MenhirState10 : ((('s, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 10.
        Stack shape : ident ident.
        Start symbol: gfile. *)

  | MenhirState13 : (('s, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 13.
        Stack shape : ident.
        Start symbol: gfile. *)

  | MenhirState15 : (('s, _menhir_box_gfile) _menhir_cell1_global_type, _menhir_box_gfile) _menhir_state
    (** State 15.
        Stack shape : global_type.
        Start symbol: gfile. *)

  | MenhirState18 : (('s, _menhir_box_gfile) _menhir_cell1_g_pair, _menhir_box_gfile) _menhir_state
    (** State 18.
        Stack shape : g_pair.
        Start symbol: gfile. *)

  | MenhirState23 : ((('s, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 23.
        Stack shape : ident ident.
        Start symbol: gfile. *)

  | MenhirState26 : (((('s, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_state
    (** State 26.
        Stack shape : ident ident ident.
        Start symbol: gfile. *)

  | MenhirState34 : ('s, _menhir_box_lfile) _menhir_state
    (** State 34.
        Stack shape : .
        Start symbol: lfile. *)

  | MenhirState35 : (('s, _menhir_box_lfile) _menhir_cell1_REC, _menhir_box_lfile) _menhir_state
    (** State 35.
        Stack shape : REC.
        Start symbol: lfile. *)

  | MenhirState37 : ((('s, _menhir_box_lfile) _menhir_cell1_REC, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 37.
        Stack shape : REC ident.
        Start symbol: lfile. *)

  | MenhirState38 : (('s, _menhir_box_lfile) _menhir_cell1_LPAREN, _menhir_box_lfile) _menhir_state
    (** State 38.
        Stack shape : LPAREN.
        Start symbol: lfile. *)

  | MenhirState44 : (('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 44.
        Stack shape : ident.
        Start symbol: lfile. *)

  | MenhirState47 : ((('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 47.
        Stack shape : ident ident.
        Start symbol: lfile. *)

  | MenhirState49 : (('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 49.
        Stack shape : ident.
        Start symbol: lfile. *)

  | MenhirState52 : (('s, _menhir_box_lfile) _menhir_cell1_l_pair, _menhir_box_lfile) _menhir_state
    (** State 52.
        Stack shape : l_pair.
        Start symbol: lfile. *)

  | MenhirState55 : (('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 55.
        Stack shape : ident.
        Start symbol: lfile. *)

  | MenhirState60 : (('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 60.
        Stack shape : ident.
        Start symbol: lfile. *)

  | MenhirState63 : ((('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 63.
        Stack shape : ident ident.
        Start symbol: lfile. *)

  | MenhirState65 : (('s, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_state
    (** State 65.
        Stack shape : ident.
        Start symbol: lfile. *)


and ('s, 'r) _menhir_cell1_g_pair = 
  | MenhirCell1_g_pair of 's * ('s, 'r) _menhir_state * (string * string Ast.global)

and ('s, 'r) _menhir_cell1_global_type = 
  | MenhirCell1_global_type of 's * ('s, 'r) _menhir_state * (string Ast.global)

and ('s, 'r) _menhir_cell1_ident = 
  | MenhirCell1_ident of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_l_pair = 
  | MenhirCell1_l_pair of 's * ('s, 'r) _menhir_state * (string * string Ast.local)

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and _menhir_box_lfile = 
  | MenhirBox_lfile of (string Ast.local) [@@unboxed]

and _menhir_box_gfile = 
  | MenhirBox_gfile of (string Ast.global) [@@unboxed]

let _menhir_action_02 =
  fun _1 ->
    (
# 37 "src/parser.mly"
                                          ( _1 )
# 191 "src/parser.ml"
     : ((string * string Ast.global) list))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 40 "src/parser.mly"
                                          ( (_1, _3) )
# 199 "src/parser.ml"
     : (string * string Ast.global))

let _menhir_action_04 =
  fun _1 ->
    (
# 25 "src/parser.mly"
                                          ( _1 )
# 207 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_05 =
  fun () ->
    (
# 28 "src/parser.mly"
                                          ( GEnd Loc.dummy )
# 215 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_06 =
  fun _1 ->
    (
# 29 "src/parser.mly"
                                          ( GVar (_1, Loc.dummy) )
# 223 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_07 =
  fun _2 _4 ->
    (
# 30 "src/parser.mly"
                                          ( GRec (_2, _4, Loc.dummy) )
# 231 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_08 =
  fun _1 _3 _5 ->
    (
# 31 "src/parser.mly"
                                          ( GBra (_1, _3, _5, Loc.dummy) )
# 239 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 32 "src/parser.mly"
                                          ( GPar (_1, _3, Loc.dummy) )
# 247 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_10 =
  fun _1 _3 _6 _9 ->
    (
# 33 "src/parser.mly"
                                                               ( GMsg (_1, _3, _6, _9, Loc.dummy) )
# 255 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_11 =
  fun _2 ->
    (
# 34 "src/parser.mly"
                                          ( _2 )
# 263 "src/parser.ml"
     : (string Ast.global))

let _menhir_action_12 =
  fun _1 ->
    (
# 21 "src/parser.mly"
                                          ( _1 )
# 271 "src/parser.ml"
     : (string))

let _menhir_action_13 =
  fun _1 ->
    (
# 57 "src/parser.mly"
                                          ( _1 )
# 279 "src/parser.ml"
     : ((string * string Ast.local) list))

let _menhir_action_14 =
  fun _1 _3 ->
    (
# 60 "src/parser.mly"
                                          ( (_1, _3) )
# 287 "src/parser.ml"
     : (string * string Ast.local))

let _menhir_action_15 =
  fun _1 ->
    (
# 44 "src/parser.mly"
                                          ( _1 )
# 295 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_16 =
  fun () ->
    (
# 47 "src/parser.mly"
                                          ( LEnd Loc.dummy )
# 303 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_17 =
  fun _1 ->
    (
# 48 "src/parser.mly"
                                          ( LVar (_1, Loc.dummy) )
# 311 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_18 =
  fun _2 _4 ->
    (
# 49 "src/parser.mly"
                                          ( LRec (_2, _4, Loc.dummy) )
# 319 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_19 =
  fun _1 _4 ->
    (
# 50 "src/parser.mly"
                                          ( LExt (_1, _4, Loc.dummy) )
# 327 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_20 =
  fun _1 _4 ->
    (
# 51 "src/parser.mly"
                                          ( LInt (_1, _4, Loc.dummy) )
# 335 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_21 =
  fun _1 _4 _7 ->
    (
# 52 "src/parser.mly"
                                                  ( LRecv (_1, _4, _7, Loc.dummy) )
# 343 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_22 =
  fun _1 _4 _7 ->
    (
# 53 "src/parser.mly"
                                                  ( LSend (_1, _4, _7, Loc.dummy) )
# 351 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_23 =
  fun _2 ->
    (
# 54 "src/parser.mly"
                                          ( _2 )
# 359 "src/parser.ml"
     : (string Ast.local))

let _menhir_action_24 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 367 "src/parser.ml"
     : ((string * string Ast.global) list))

let _menhir_action_25 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 375 "src/parser.ml"
     : ((string * string Ast.global) list))

let _menhir_action_26 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 383 "src/parser.ml"
     : ((string * string Ast.local) list))

let _menhir_action_27 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 391 "src/parser.ml"
     : ((string * string Ast.local) list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARROW ->
        "ARROW"
    | BANG ->
        "BANG"
    | BAR ->
        "BAR"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DOT ->
        "DOT"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | IDENT _ ->
        "IDENT"
    | LBRACE ->
        "LBRACE"
    | LBRACK ->
        "LBRACK"
    | LPAREN ->
        "LPAREN"
    | QUEST ->
        "QUEST"
    | RBRACE ->
        "RBRACE"
    | RBRACK ->
        "RBRACK"
    | REC ->
        "REC"
    | RPAREN ->
        "RPAREN"
    | SEMI ->
        "SEMI"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_69 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_15 _1 in
          MenhirBox_lfile _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState01 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_12 _1 in
      _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState60 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState65 ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState49 ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState52 ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState44 ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState63 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState55 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState38 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState26 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_61 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _menhir_s = MenhirState63 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REC ->
                  _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | END ->
                  _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_35 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState35 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState38 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | END ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_39 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_16 () in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_local_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState34 ->
          _menhir_run_69 _menhir_stack _v _tok
      | MenhirState37 ->
          _menhir_run_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState63 ->
          _menhir_run_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState55 ->
          _menhir_run_56 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState47 ->
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState38 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_68 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_REC, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_18 _2 _4 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_64 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_22 _1 _4 _7 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_56 : type  ttv_stack. (ttv_stack, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_14 _1 _3 in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_l_pair (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState52 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACE ->
          let x = _v in
          let _v = _menhir_action_26 x in
          _menhir_goto_separated_nonempty_list_COMMA_l_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_l_pair_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState52 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState65 ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState49 ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_53 : type  ttv_stack. (ttv_stack, _menhir_box_lfile) _menhir_cell1_l_pair -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_l_pair (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_27 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_l_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_50 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _1 = _v in
      let _v = _menhir_action_13 _1 in
      _menhir_goto_l_lab _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_l_lab : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState65 ->
          _menhir_run_66 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState49 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_66 : type  ttv_stack. (ttv_stack, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_20 _1 _4 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_57 : type  ttv_stack. (ttv_stack, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_19 _1 _4 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_48 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_ident, _menhir_box_lfile) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_21 _1 _4 _7 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_40 : type  ttv_stack. (ttv_stack, _menhir_box_lfile) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_23 _2 in
          _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_54 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState55 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REC ->
              _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | END ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_45 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _menhir_s = MenhirState47 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REC ->
                  _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | END ->
                  _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_42 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUEST ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACK ->
              let _menhir_s = MenhirState44 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | LBRACE ->
              let _menhir_s = MenhirState49 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | BANG ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACK ->
              let _menhir_s = MenhirState60 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | LBRACE ->
              let _menhir_s = MenhirState65 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA | EOF | RBRACE | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_17 _1 in
          _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. ((ttv_stack, _menhir_box_lfile) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_lfile) _menhir_state -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_s = MenhirState37 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REC ->
              _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | END ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. (((ttv_stack, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _menhir_s = MenhirState26 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REC ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | END ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState05 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | END ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_global_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState26 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_31 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          MenhirBox_gfile _v
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_gfile) _menhir_cell1_global_type -> _ -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState15 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | END ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_30 : type  ttv_stack. (((ttv_stack, _menhir_box_gfile) _menhir_cell1_REC, _menhir_box_gfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | RBRACE | RPAREN ->
          let MenhirCell1_ident (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_07 _2 _4 in
          _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_28 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_11 _2 in
          _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_27 : type  ttv_stack. ((((ttv_stack, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | RBRACE | RPAREN ->
          let MenhirCell1_ident (_menhir_stack, _, _6) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _9 = _v in
          let _v = _menhir_action_10 _1 _3 _6 _9 in
          _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_global_type as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | RBRACE | RPAREN ->
          let MenhirCell1_global_type (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_09 _1 _3 in
          _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_global_type (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_03 _1 _3 in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_g_pair (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState18 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | RBRACE ->
              let x = _v in
              let _v = _menhir_action_24 x in
              _menhir_goto_separated_nonempty_list_COMMA_g_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_g_pair_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_gfile) _menhir_cell1_g_pair -> _ -> _ -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_g_pair (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_25 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_g_pair_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_ident, _menhir_box_gfile) _menhir_cell1_ident -> _ -> _ -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_08 _1 _3 _5 in
      _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState13 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REC ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | END ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_s = MenhirState10 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACK ->
              let _menhir_s = MenhirState23 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState08 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | BAR | COMMA | EOF | RBRACE | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_goto_global_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ((ttv_stack, _menhir_box_gfile) _menhir_cell1_REC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_gfile) _menhir_state -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_s = MenhirState04 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REC ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | END ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_gfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | END ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_34 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_lfile =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState34 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | END ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let lfile =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_lfile v = _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let gfile =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_gfile v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
