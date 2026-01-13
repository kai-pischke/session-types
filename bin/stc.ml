open Printf

let usage () =
  eprintf "Usage: stc <command> [...]\n";
  eprintf "  parse-global <file>\n";
  eprintf "  project <file> [<role>] [--coinductive|-c|--inductive|-i] [--full|-f|--plain|-p] (shortcuts: -cf, -cp, -if, -ip)\n";
  eprintf "  check <file>\n";
  eprintf "  synth <local-dir>\n";
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

let parse_graph_format = function
  | "dot" -> Dot
  | "json" -> Json
  | f -> exit_error ("Unknown format: " ^ f)

let cmd_project alg variant file role_opt =
  let g = or_fail (Case_studies.parse_global_file file) in
  let roles =
    match role_opt with
    | Some r -> [ r ]
    | None ->
        Well_formed.roles_of_global g
        |> Well_formed.StringSet.elements
  in
  let project_one role =
    match alg, variant with
    | Coinductive, Full ->
        let g_int = Normalise.encode g in
        let aut = Automaton.of_global g_int in
        begin match Coinductive.project aut role with
        | Ok local_aut ->
            let l = Automaton_to_local.automaton_to_local local_aut in
            Pretty.string_of_local pp_int_var l
        | Error msg -> exit_error msg
        end
    | Coinductive, Plain ->
        let g_int = Normalise.encode g in
        let aut = Automaton.of_global g_int in
        begin match Projection_strict.project aut role with
        | Ok local_aut ->
            let l = Automaton_to_local.automaton_to_local local_aut in
            Pretty.string_of_local pp_int_var l
        | Error msg -> exit_error msg
        end
    | Inductive, Full ->
        begin match Inductive.project ~mode:Inductive.Full g role with
        | Ok l -> Pretty.string_of_local pp_str l
        | Error msg -> exit_error msg
        end
    | Inductive, Plain ->
        begin match Inductive.project ~mode:Inductive.Plain g role with
        | Ok l -> Pretty.string_of_local pp_str l
        | Error msg -> exit_error msg
        end
  in
  List.iter (fun r ->
    printf "--- %s ---\n%s\n" r (project_one r)
  ) roles

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

let cmd_synth dir =
  let locals = or_fail (Case_studies.parse_local_files dir) in
  match Case_studies.test_synthesis locals with
  | None -> exit_error "Synthesis failed"
  | Some (size, global) ->
      printf "Synthesised global (size=%d):\n%s\n" size (Pretty.string_of pp_int_var global)

let cmd_case_studies ?(print_types=true) base_path =
  Case_studies.run ~print_types base_path

let output_string ?out s =
  match out with
  | None -> print_endline s
  | Some path ->
      let oc = open_out path in
      output_string oc s;
      close_out oc

let cmd_automaton_global fmt out file =
  let g = or_fail (Case_studies.parse_global_file file) in
  let g_int = Normalise.encode g in
  let aut = Automaton.of_global g_int in
  let rendered =
    match fmt with
    | Dot -> Automaton.dot_of_graph aut
    | Json -> Automaton.json_of_graph aut
  in
  output_string ?out rendered

let cmd_automaton_local fmt out file =
  let l = or_fail (Case_studies.parse_local_file file) in
  let l_int = Normalise.encode_local l in
  let aut = Local_automaton.of_local l_int in
  let rendered =
    match fmt with
    | Dot -> Local_automaton.dot_of_graph aut
    | Json -> Local_automaton.json_of_graph aut
  in
  output_string ?out rendered

(* Argument parsing ------------------------------------------------- *)

let parse_project_args args =
  let rec aux alg variant file role_opt = function
    | [] ->
        (match file with
         | Some f -> (alg, variant, f, role_opt)
         | _ -> usage ())
    | ("--coinductive" | "-c") :: tl ->
        aux Coinductive variant file role_opt tl
    | ("--inductive" | "-i") :: tl ->
        aux Inductive variant file role_opt tl
    | ("--full" | "-f") :: tl ->
        aux alg Full file role_opt tl
    | ("--plain" | "-p") :: tl ->
        aux alg Plain file role_opt tl
    | "-if" :: tl ->
        aux Inductive Full file role_opt tl
    | "-ip" :: tl ->
        aux Inductive Plain file role_opt tl
    | "-cf" :: tl ->
        aux Coinductive Full file role_opt tl
    | "-cp" :: tl ->
        aux Coinductive Plain file role_opt tl
    | arg :: tl when file = None ->
        aux alg variant (Some arg) role_opt tl
    | arg :: tl when role_opt = None ->
        aux alg variant file (Some arg) tl
    | arg :: _ ->
        exit_error ("Unexpected argument: " ^ arg)
  in
  aux Coinductive Full None None args

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
           let alg, variant, file, role_opt = parse_project_args args in
           cmd_project alg variant file role_opt
       | "check" ->
           (match args with
            | [file] -> cmd_check file
            | _ -> usage ())
       | "synth" ->
           (match args with
            | [dir] -> cmd_synth dir
            | _ -> usage ())
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
