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

let parse_string ~local src =
  let lb = from_string src in
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = "<cmdline>" };
  try
    if local
    then `Local (Parser.lfile Lexer.token lb)
    else `Global (Parser.gfile Lexer.token lb)
  with
  | Parser.Error ->
      let p = lb.lex_curr_p in
      eprintf "Syntax error at %s\n%!" (string_of_position p);
      exit 1

(*--------------------------------------------------------------------*)
(*  Main                                                               *)
(*--------------------------------------------------------------------*)
let () =
  let local = ref false in
  let synth = ref false in
  let files : string list ref = ref [] in
  let value = ref None in
  let speclist =
    [ "--local", Arg.Set local,
        " Parse the input as a local type (default: global)";
      "--synth", Arg.Set synth,
        " Synthesize a global type from multiple local files";
      "-v", Arg.String (fun s -> value := Some s),
        " Parse the given string as a type (instead of a file)";
      "--value", Arg.String (fun s -> value := Some s),
        " Parse the given string as a type (instead of a file)";
    ]
  in
  Arg.parse speclist (fun f -> files := !files @ [f]) "stc [options] <file(s)>";

  (* Helper: expand directories into contained regular files *)
  let rec expand_paths acc = function
    | [] -> List.rev acc
    | p::ps ->
        (try
           if Sys.is_directory p then (
             let entries = Sys.readdir p |> Array.to_list in
             let full = List.map (Filename.concat p) entries in
             expand_paths acc (full @ ps)
           ) else expand_paths (p::acc) ps
         with Sys_error _ -> expand_paths acc ps)

  in

  (* Synthesis mode -------------------------------------------------- *)
  if !synth then (
    if !files = [] then (eprintf "No local input files given for synthesis.\n%!"; exit 1);
    let file_list = expand_paths [] !files in
    (* Parse each file as local type, encode, build automata *)
    let parts =
      List.filter_map (fun fname ->
        if Filename.extension fname = "" || Filename.extension fname = ".st" then
          match parse_file ~local:true fname with
          | `Local lt ->
              let lt_enc = Encode.encode_local lt in
              let role = Filename.chop_extension (Filename.basename fname) in
              let la = Local_automaton.of_local lt_enc in
              Some { Synthesis.role = role; laut = la }
          | _ -> None
        else None) file_list
    in
    match Synthesis.synthesise parts with
    | Error msg -> eprintf "Synthesis error: %s\n%!" msg; exit 1
    | Ok g ->
        (* Convert automaton to global type *)
        let g_global = Automaton_to_global.automaton_to_global g in
        let text = Pretty.string_of (fun fmt v -> Format.fprintf fmt "%d" v) g_global in
        printf "Synthesised global type:\n%s\n%!" text;
        exit 0
  );

  (* Regular single-input mode --------------------------------------- *)
  let file = match !files with
    | [f] -> f
    | [] -> ""
    | _ -> (eprintf "Multiple files given without --synth flag.\n%!"; exit 1)

  in

  if !value = None && file = "" then (
    eprintf "No input given. Use -v <type> or provide a file.\n%!"; exit 1);

  let input = match !value with
    | Some v -> `String v
    | None -> `File file
  in

  let parsed = match input with
    | `String src -> parse_string ~local:!local src
    | `File fname -> parse_file ~local:!local fname
  in

  match parsed with
  | `Local lt ->
      printf "✓ Parsed a local session type.\n%!";
      let la = Local_automaton.of_local (Encode.encode_local lt) in
      printf "Local automaton:\n";
      printf "num_states: %d\n" la.num_states;
      printf "start_state: %s\n" (match la.start_state with Some i -> string_of_int i | None -> "None");
      for i = 0 to la.num_states - 1 do
        printf "state %d: role=%s, " i la.roles.(i);
        begin match la.kinds.(i) with
        | Local_automaton.Snd (base, dst) ->
            printf "Snd(%s, %s)\n" base (match dst with Some j -> string_of_int j | None -> "end")
        | Local_automaton.Rcv (base, dst) ->
            printf "Rcv(%s, %s)\n" base (match dst with Some j -> string_of_int j | None -> "end")
        | Local_automaton.Int branches ->
            printf "Int([%s])\n" (String.concat ";" (List.map (fun (lbl, dst) ->
              Printf.sprintf "%s->%s" lbl (match dst with Some j -> string_of_int j | None -> "end")) branches))
        | Local_automaton.Ext branches ->
            printf "Ext([%s])\n" (String.concat ";" (List.map (fun (lbl, dst) ->
              Printf.sprintf "%s->%s" lbl (match dst with Some j -> string_of_int j | None -> "end")) branches))
        end
      done
  | `Global gt -> (
      try
        Well_formed.check_global gt;
        printf "✓ Well-formed global session type.\n%!";
        
        (* Encode and create automaton *)
        let gt' = Encode.encode gt in
        let aut = Automaton.of_global gt' in
        
        (* Check balance *)
        let balanced = Balance.is_balanced aut in
        printf "Balance: %s\n" (if balanced then "balanced" else "unbalanced");

        if balanced then (
          (* Determine participants *)
          let participants = ref Balance.RoleSet.empty in
          for i = 0 to aut.num_states - 1 do
            let (s,r) = aut.roles.(i) in
            participants := Balance.RoleSet.add s !participants;
            participants := Balance.RoleSet.add r !participants;
          done;
          printf "\nProjections:\n";
          Balance.RoleSet.iter (fun p ->
            match Projection.project aut p with
            | Error msg ->
                printf "- %s: not projectable (%s)\n" p msg
            | Ok loc_aut -> (
                match Automaton_to_local.automaton_to_local loc_aut with
                | local_ast ->
                    let text = Pretty.string_of_local (fun fmt v -> Format.fprintf fmt "%d" v) local_ast in
                    printf "- %s : %s\n" p text )
          ) !participants
        )
      with
      | Well_formed.Error (loc, msg) ->
          eprintf "Well-formedness error at %s:\n  %s\n%!"
            (string_of_loc loc) msg;
          exit 1 )
