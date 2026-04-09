(* Round-trip test: parse local type string, pretty-print, parse again, pretty-print again.
   The two printed strings must be identical. *)

let pp_str fmt s = Format.pp_print_string fmt s

let parse_local_string s =
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.lfile Lexer.token lexbuf)
  with _ -> Error "parse error"

let print_local l = Pretty.string_of_local pp_str l

let roundtrip label input =
  match parse_local_string input with
  | Error e -> Printf.printf "FAIL [%s]: parse error on input: %s (%s)\n" label input e
  | Ok ast1 ->
      let s1 = print_local ast1 in
      match parse_local_string s1 with
      | Error e -> Printf.printf "FAIL [%s]: re-parse error on: %s (%s)\n" label s1 e
      | Ok ast2 ->
          let s2 = print_local ast2 in
          if s1 = s2 then
            Printf.printf "OK   [%s]: %s\n" label s1
          else
            Printf.printf "FAIL [%s]: first=%s  second=%s\n" label s1 s2

let parse_global_string s =
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.gfile Lexer.token lexbuf)
  with _ -> Error "parse error"

let print_global g = Pretty.string_of pp_str g

let roundtrip_global label input =
  match parse_global_string input with
  | Error e -> Printf.printf "FAIL [%s]: parse error on input: %s (%s)\n" label input e
  | Ok ast1 ->
      let s1 = print_global ast1 in
      match parse_global_string s1 with
      | Error e -> Printf.printf "FAIL [%s]: re-parse error on: %s (%s)\n" label s1 e
      | Ok ast2 ->
          let s2 = print_global ast2 in
          if s1 = s2 then
            Printf.printf "OK   [%s]: %s\n" label s1
          else
            Printf.printf "FAIL [%s]: first=%s  second=%s\n" label s1 s2

let () =
  Printf.printf "=== LOCAL TYPE ROUND-TRIP TESTS ===\n";
  (* Basic messages *)
  roundtrip "send" "b ! [Msg]; end";
  roundtrip "recv" "b ? [Msg]; end";
  (* Branching *)
  roundtrip "int-choice" "b ! { l1: end, l2: end }";
  roundtrip "ext-choice" "b ? { l1: end, l2: end }";
  (* Nested *)
  roundtrip "send+int" "b ! [Msg]; a ! { l1: end, l2: end }";
  roundtrip "recv+ext" "b ? [Msg]; a ? { l1: end, l2: end }";
  roundtrip "send+ext" "b ! [Msg]; a ? { l1: end, l2: b ? [X]; end }";
  roundtrip "recv+int" "b ? [Msg]; a ! { l1: end, l2: b ! [X]; end }";
  (* Recursion *)
  roundtrip "rec-send" "rec t. b ! [Msg]; t";
  roundtrip "rec-int" "rec t. b ! { l1: t, l2: end }";
  roundtrip "rec-ext" "rec t. b ? { l1: t, l2: end }";
  (* Multi-branch *)
  roundtrip "3-branch-int" "b ! { l1: end, l2: end, l3: end }";
  roundtrip "3-branch-ext" "b ? { l1: end, l2: end, l3: end }";
  (* Complex nested *)
  roundtrip "nested-choice" "a ! { l1: b ? { m1: end, m2: end }, l2: end }";
  roundtrip "deep" "rec t. a ! [X]; b ? [Y]; c ! { go: t, stop: end }";

  Printf.printf "\n=== GLOBAL TYPE ROUND-TRIP TESTS ===\n";
  roundtrip_global "msg" "a -> b : [Msg]; end";
  roundtrip_global "bra" "a -> b { l1: end, l2: end }";
  roundtrip_global "rec" "rec t. a -> b { l1: t, l2: end }";
  roundtrip_global "msg+bra" "a -> b : [Msg]; b -> a { l1: end, l2: end }";
  roundtrip_global "3-branch" "a -> b { l1: end, l2: end, l3: end }";
  roundtrip_global "nested" "a -> b { l1: b -> c : [X]; end, l2: end }";

  Printf.printf "\nDone.\n"
