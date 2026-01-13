
(* The type of tokens. *)

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
  | IDENT of (string)
  | EOF
  | END
  | DOT
  | COMMA
  | COLON
  | BAR
  | BANG
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val lfile: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string Ast.local)

val gfile: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string Ast.global)
