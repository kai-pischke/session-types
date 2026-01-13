(** Source-code locations (same structure as Lexing.position ranges). *)
type t = { start_pos : Lexing.position; end_pos : Lexing.position }

let dummy =
  let p = Lexing.dummy_pos in
  { start_pos = p; end_pos = p }

let of_lex (sp, ep) = { start_pos = sp; end_pos = ep }

