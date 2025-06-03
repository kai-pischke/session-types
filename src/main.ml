(*--------------------------------------------------------------------*)
(*  Session-type checker – command-line front-end                      *)
(*--------------------------------------------------------------------*)

open Printf
open Lexing

(*--------------------------------------------------------------------*)
(*  Pretty-printing source locations                                   *)
(*--------------------------------------------------------------------*)
let string_of_position p =
  sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol + 1)

let string_of_loc (loc : Loc.t) =
  if loc == Loc.dummy then "<unknown>"
  else
    let sp, ep = loc.start_pos, loc.end_pos in
    if sp.pos_fname = ep.pos_fname && sp.pos_lnum = ep.pos_lnum then
      sprintf "%s-%d"
        (string_of_position sp)
        (ep.pos_cnum - sp.pos_bol + 1)
    else
      sprintf "%s–%s" (string_of_position sp) (string_of_position ep)

(*--------------------------------------------------------------------*)
(*  Parsing                                                            *)
(*--------------------------------------------------------------------*)
let parse_file ~local fname =
  let ic  = open_in fname in
  let lb  = from_channel ic in
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = fname };
  try
    let result =
      if local
      then `Local  (Parser.lfile Lexer.token lb)
      else `Global (Parser.gfile Lexer.token lb)
    in
    close_in ic; result
  with
  | Parser.Error ->
      let p = lb.lex_curr_p in
      eprintf "Syntax error at %s\n%!" (string_of_position p);
      close_in ic; exit 1

(*--------------------------------------------------------------------*)
(*  Main                                                               *)
(*--------------------------------------------------------------------*)
let () =
  let local = ref false in
  let file  = ref "" in
  let speclist =
    [ "--local", Arg.Set local,
        " Parse the input as a local type (default: global)" ]
  in
  Arg.parse speclist (fun f -> file := f) "stc [--local] <file>";
  if !file = "" then (eprintf "No input file given.\n%!"; exit 1);

  match parse_file ~local:!local !file with
  | `Local _lt ->
      printf "✓ Parsed a local session type.\n%!"

  | `Global gt -> (
      try
        Well_formed.check_global gt;
        printf "✓ Well-formed global session type.\n%!"
      with
      | Well_formed.Error (loc, msg) ->
          eprintf "Well-formedness error at %s:\n  %s\n%!"
            (string_of_loc loc) msg;
          exit 1 )
