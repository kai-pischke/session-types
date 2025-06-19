open Printf

let parse_global (src : string) : string Ast.global =
  let lb = Lexing.from_string src in
  Parser.gfile Lexer.token lb

let print_automaton src =
  let g = parse_global src in
  let g' = Encode.encode g in
  Printf.printf "Encoded global: %s\n" (Pretty.string_of Format.pp_print_int g');
  let aut = Automaton.of_global g' in
  print_endline (Automaton.string_of_graph aut)

let () =
  let cases = [
    ("GPar legal", "rec t. (a -> b { Ok: t }) | (c -> d { X: end })");
    ("Three in parallel", "a -> b { Ok: end } | c -> d { X: end } | e -> f { Y: end }");
    ("Nested parallel", "a -> b { Ok: c -> d { X: end } | e -> f { Y: end } }");
    ("Parallel with recursion both sides", "rec t. (a -> b { Ok: t }) | (rec u. c -> d { X: u, Y: end })");
    ("Nested recursion in parallel branch", "rec t. a -> b { Ok: rec u. c -> d { X: u, Y: t } | e -> f { Z: end } }");
  ] in
  List.iter (fun (name, src) ->
    printf "--- %s ---\n" name;
    print_automaton src;
    print_endline ""
  ) cases 