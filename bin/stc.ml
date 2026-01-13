open Printf

let usage () =
  eprintf "Usage: stc <command> [...]\n";
  eprintf "  parse-global <file>\n";
  eprintf "  project <file> [<role>] [--coinductive|-c|--inductive|-i] [--full|-f|--plain|-p] [--out-dir <dir>] [--emit-global <dot|json> <path>] [--emit-locals <dot|json> <dir>] (shortcuts: -cf, -cp, -if, -ip)\n";
  eprintf "  check <file>\n";
  eprintf "  synth <local-dir> [--out <file>] [--emit-global <dot|json> <path>] [--emit-locals <dot|json> <dir>]\n";
  eprintf "  case-studies [--path <dir>] [--no-types]\n";
  eprintf "  automaton-global <file> [--format dot|json] [--out|-o <path>]\n";
  eprintf "  automaton-local <file> [--format dot|json] [--out|-o <path>]\n";
  exit 1

let exit_error msg =
  eprintf "Error: %s\n" msg;
  exit 1

let or_fail = function
  | Ok v -> v
  | Error msg -> exit_error msg

let pp_str fmt s = Format.pp_print_string fmt s
let pp_int_var fmt i = Format.fprintf fmt "t%d" i

(* Commands --------------------------------------------------------- *)

let cmd_parse_global file =
  let g = or_fail (Case_studies.parse_global_file file) in
  printf "%s\n" (Pretty.string_of pp_str g)

type project_algorithm = Coinductive | Inductive
type merge_variant = Full | Plain

type graph_format = Dot | Json
type emit_target = {
  fmt : graph_format;
  path : string;
}

let parse_graph_format = function
  | "dot" -> Dot
  | "json" -> Json
  | f -> exit_error ("Unknown format: " ^ f)

let emit_string ?out s =
  match out with
  | None -> print_endline s
  | Some path ->
      let oc = open_out path in
      Stdlib.output_string oc s;
      close_out oc

let write_file path contents =
  let oc = open_out path in
  Stdlib.output_string oc contents;
  close_out oc

let mkdir_p dir =
  let rec aux d =
    if d = "" || d = Filename.dir_sep || d = "." then ()
    else if Sys.file_exists d then
      if Sys.is_directory d then () else exit_error (d ^ " exists and is not a directory")
    else (
      aux (Filename.dirname d);
      Unix.mkdir d 0o755
    )
  in
  aux dir

let string_of_int_local l = Pretty.string_of_local (fun fmt v -> Format.fprintf fmt "t%d" v) l
let string_of_str_local l = Pretty.string_of_local Format.pp_print_string l

type project_emit_opts = {
  out_dir : string option;
  emit_global : emit_target option;
  emit_locals : emit_target option;
}

let cmd_project alg variant file role_opt emit_opts =
  let g = or_fail (Case_studies.parse_global_file file) in
  let roles =
    match role_opt with
    | Some r -> [ r ]
    | None ->
        Well_formed.roles_of_global g
        |> Well_formed.StringSet.elements
  in
  let g_int = Normalise.encode g in
  let global_aut = Automaton.of_global g_int in
  let project_one role =
    match alg, variant with
    | Coinductive, Full ->
        begin match Coinductive.project global_aut role with
        | Ok local_aut ->
            let l = Automaton_to_local.automaton_to_local local_aut in
            (string_of_int_local l, local_aut)
        | Error msg -> exit_error msg
        end
    | Coinductive, Plain ->
        begin match Projection_strict.project global_aut role with
        | Ok local_aut ->
            let l = Automaton_to_local.automaton_to_local local_aut in
            (string_of_int_local l, local_aut)
        | Error msg -> exit_error msg
        end
    | Inductive, Full ->
        begin match Inductive.project ~mode:Inductive.Full g role with
        | Ok l ->
            let enc = Normalise.encode_local l in
            let aut = Local_automaton.of_local enc in
            (string_of_str_local l, aut)
        | Error msg -> exit_error msg
        end
    | Inductive, Plain ->
        begin match Inductive.project ~mode:Inductive.Plain g role with
        | Ok l ->
            let enc = Normalise.encode_local l in
            let aut = Local_automaton.of_local enc in
            (string_of_str_local l, aut)
        | Error msg -> exit_error msg
        end
  in
  let results = List.map (fun r -> (r, project_one r)) roles in
  (* Print to stdout if no out_dir *)
  (match emit_opts.out_dir with
   | None ->
       List.iter (fun (r, (txt, _)) ->
         printf "--- %s ---\n%s\n" r txt
       ) results
   | Some dir ->
       mkdir_p dir;
       List.iter (fun (r, (txt, _)) ->
         let path = Filename.concat dir (r ^ ".st") in
         write_file path txt
       ) results);
  (match emit_opts.emit_global with
   | None -> ()
   | Some {fmt; path} ->
       (match fmt with
        | Dot -> write_file path (Automaton.dot_of_graph global_aut)
        | Json -> write_file path (Automaton.json_of_graph global_aut)));
  (match emit_opts.emit_locals with
   | None -> ()
   | Some {fmt; path} ->
       mkdir_p path;
       List.iter (fun (r, (_, aut)) ->
         let fname = Filename.concat path (r ^ (match fmt with Dot -> ".dot" | Json -> ".json")) in
         let rendered = match fmt with
           | Dot -> Local_automaton.dot_of_graph aut
           | Json -> Local_automaton.json_of_graph aut
         in
         write_file fname rendered
       ) results)

let cmd_check file =
  let g = or_fail (Case_studies.parse_global_file file) in
  (try Well_formed.check_global g; printf "Well-formed: yes\n"
   with Well_formed.Error (_, msg) -> printf "Well-formed: no (%s)\n" msg; exit 1);
  let aut_balanced =
    let enc = Normalise.encode g in
    let aut = Automaton.of_global enc in
    Balance.is_balanced aut
  in
  printf "Balanced: %s\n" (if aut_balanced then "yes" else "no");
  if not aut_balanced then exit 1

type synth_emit_opts = {
  out_file : string option;
  emit_global : emit_target option;
  emit_locals : emit_target option;
}

let cmd_synth opts dir =
  let locals = or_fail (Case_studies.parse_local_files dir) in
  try
    let roles = List.map fst locals in
    let role_array = Array.of_list roles in
    let role_to_int role =
      match Array.find_index ((=) role) role_array with
      | Some i -> i
      | None -> failwith ("Unknown role: " ^ role)
    in
    let encoded_locals = List.map (fun (_, l) -> Normalise.encode_local l) locals in
    let local_auts = List.map Local_automaton.of_local encoded_locals in
    let cfsm = Array.of_list (List.map (fun (_, l) -> Local_automaton.of_local_int (Normalise.encode_local l) role_to_int) locals) in
    let global = Synthesis_alg.synth cfsm role_array in
    let global_str = Pretty.string_of pp_int_var global in
    (match opts.out_file with
     | None -> printf "Synthesised global:\n%s\n" global_str
     | Some path -> write_file path global_str);
    (match opts.emit_global with
     | None -> ()
     | Some {fmt; path} ->
         let aut = Automaton.of_global global in
         let rendered = match fmt with
           | Dot -> Automaton.dot_of_graph aut
           | Json -> Automaton.json_of_graph aut
         in
         write_file path rendered);
    (match opts.emit_locals with
     | None -> ()
     | Some {fmt; path} ->
         mkdir_p path;
         List.iter2 (fun role aut ->
           let fname = Filename.concat path (role ^ (match fmt with Dot -> ".dot" | Json -> ".json")) in
           let rendered = match fmt with
             | Dot -> Local_automaton.dot_of_graph aut
             | Json -> Local_automaton.json_of_graph aut
           in
           write_file fname rendered
         ) roles local_auts)
  with _ -> exit_error "Synthesis failed"

let cmd_case_studies ?(print_types=true) base_path =
  Case_studies.run ~print_types base_path

let cmd_automaton_global fmt out file =
  let g = or_fail (Case_studies.parse_global_file file) in
  let g_int = Normalise.encode g in
  let aut = Automaton.of_global g_int in
  let rendered =
    match fmt with
    | Dot -> Automaton.dot_of_graph aut
    | Json -> Automaton.json_of_graph aut
  in
  emit_string ?out rendered

let cmd_automaton_local fmt out file =
  let l = or_fail (Case_studies.parse_local_file file) in
  let l_int = Normalise.encode_local l in
  let aut = Local_automaton.of_local l_int in
  let rendered =
    match fmt with
    | Dot -> Local_automaton.dot_of_graph aut
    | Json -> Local_automaton.json_of_graph aut
  in
  emit_string ?out rendered

(* Argument parsing ------------------------------------------------- *)

let parse_project_args args =
  let rec aux alg variant out_dir emit_global emit_locals file role_opt = function
    | [] ->
        (match file with
         | Some f -> (alg, variant, out_dir, emit_global, emit_locals, f, role_opt)
         | _ -> usage ())
    | ("--coinductive" | "-c") :: tl ->
        aux Coinductive variant out_dir emit_global emit_locals file role_opt tl
    | ("--inductive" | "-i") :: tl ->
        aux Inductive variant out_dir emit_global emit_locals file role_opt tl
    | ("--full" | "-f") :: tl ->
        aux alg Full out_dir emit_global emit_locals file role_opt tl
    | ("--plain" | "-p") :: tl ->
        aux alg Plain out_dir emit_global emit_locals file role_opt tl
    | "-if" :: tl ->
        aux Inductive Full out_dir emit_global emit_locals file role_opt tl
    | "-ip" :: tl ->
        aux Inductive Plain out_dir emit_global emit_locals file role_opt tl
    | "-cf" :: tl ->
        aux Coinductive Full out_dir emit_global emit_locals file role_opt tl
    | "-cp" :: tl ->
        aux Coinductive Plain out_dir emit_global emit_locals file role_opt tl
    | "--out-dir" :: d :: tl ->
        aux alg variant (Some d) emit_global emit_locals file role_opt tl
    | "--emit-global" :: fmt :: path :: tl ->
        aux alg variant out_dir (Some {fmt=parse_graph_format fmt; path}) emit_locals file role_opt tl
    | "--emit-locals" :: fmt :: dir :: tl ->
        aux alg variant out_dir emit_global (Some {fmt=parse_graph_format fmt; path=dir}) file role_opt tl
    | arg :: tl when file = None ->
        aux alg variant out_dir emit_global emit_locals (Some arg) role_opt tl
    | arg :: tl when role_opt = None ->
        aux alg variant out_dir emit_global emit_locals file (Some arg) tl
    | arg :: _ ->
        exit_error ("Unexpected argument: " ^ arg)
  in
  aux Coinductive Full None None None None None args

let parse_case_study_args args =
  let rec aux path print_types = function
    | [] -> (path, print_types)
    | "--path" :: p :: tl -> aux (Some p) print_types tl
    | "--no-types" :: tl -> aux path false tl
    | arg :: _ -> exit_error ("Unexpected argument: " ^ arg)
  in
  let path_opt, print_types = aux None true args in
  (Option.value path_opt ~default:"case studies", print_types)

let parse_graph_args args =
  let rec aux fmt out file = function
    | [] ->
        (match file with
         | Some f -> (fmt, out, f)
         | None -> usage ())
    | "--format" :: f :: tl ->
        aux (parse_graph_format f) out file tl
    | "--out" :: p :: tl | "-o" :: p :: tl ->
        aux fmt (Some p) file tl
    | arg :: tl when file = None ->
        aux fmt out (Some arg) tl
    | arg :: _ -> exit_error ("Unexpected argument: " ^ arg)
  in
  aux Dot None None args

let parse_synth_args args =
  let rec aux out emit_global emit_locals dir_opt = function
    | [] ->
        (match dir_opt with
         | Some d -> (out, emit_global, emit_locals, d)
         | None -> usage ())
    | "--out" :: p :: tl ->
        aux (Some p) emit_global emit_locals dir_opt tl
    | "--emit-global" :: fmt :: path :: tl ->
        aux out (Some {fmt=parse_graph_format fmt; path}) emit_locals dir_opt tl
    | "--emit-locals" :: fmt :: dir :: tl ->
        aux out emit_global (Some {fmt=parse_graph_format fmt; path=dir}) dir_opt tl
    | arg :: tl when dir_opt = None ->
        aux out emit_global emit_locals (Some arg) tl
    | arg :: _ -> exit_error ("Unexpected argument: " ^ arg)
  in
  aux None None None None args

let () =
  match Array.to_list Sys.argv with
  | [] -> usage ()
  | [_] -> usage ()
  | _ :: cmd :: args ->
      (match cmd with
       | "parse-global" ->
           (match args with
            | [file] -> cmd_parse_global file
            | _ -> usage ())
       | "project" ->
           let alg, variant, out_dir, emit_global, emit_locals, file, role_opt = parse_project_args args in
           cmd_project alg variant file role_opt {out_dir; emit_global; emit_locals}
       | "check" ->
           (match args with
            | [file] -> cmd_check file
            | _ -> usage ())
       | "synth" ->
           let out, emit_global, emit_locals, dir = parse_synth_args args in
           cmd_synth {out_file=out; emit_global; emit_locals} dir
       | "case-studies" ->
           let path, print_types = parse_case_study_args args in
           cmd_case_studies ~print_types path
       | "automaton-global" ->
           let fmt, out, file = parse_graph_args args in
           cmd_automaton_global fmt out file
       | "automaton-local" ->
           let fmt, out, file = parse_graph_args args in
           cmd_automaton_local fmt out file
       | _ -> usage ())
