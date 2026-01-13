open Ast
open Printf
open Lexing

(* Feature detection ------------------------------------------------ *)

let rec has_mu_binders_global (g : 'v global) : bool =
  match g with
  | GEnd _ | GVar _ -> false
  | GRec _ -> true
  | GMsg (_, _, _, continuation, _) -> has_mu_binders_global continuation
  | GBra (_, _, branches, _) ->
      List.exists (fun (_, branch) -> has_mu_binders_global branch) branches
  | GPar (left, right, _) -> has_mu_binders_global left || has_mu_binders_global right

let rec has_parallel_composition_global (g : 'v global) : bool =
  match g with
  | GEnd _ | GVar _ -> false
  | GRec (_, body, _) -> has_parallel_composition_global body
  | GMsg (_, _, _, continuation, _) -> has_parallel_composition_global continuation
  | GBra (_, _, branches, _) ->
      List.exists (fun (_, branch) -> has_parallel_composition_global branch) branches
  | GPar _ -> true

let rec has_mu_binders_local (l : 'v local) : bool =
  match l with
  | LEnd _ | LVar _ -> false
  | LRec _ -> true
  | LInt (_, branches, _) | LExt (_, branches, _) ->
      List.exists (fun (_, branch) -> has_mu_binders_local branch) branches
  | LRecv (_, _, continuation, _) -> has_mu_binders_local continuation
  | LSend (_, _, continuation, _) -> has_mu_binders_local continuation

(* Parsing helpers -------------------------------------------------- *)

let parse_global_file (filepath : string) : (string global, string) result =
  try
    let ic = open_in filepath in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let lexbuf = Lexing.from_string content in
    try Ok (Parser.gfile Lexer.token lexbuf)
    with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        Error (sprintf "Parse error at line %d" pos.pos_lnum)
    | e -> Error (Printexc.to_string e)
  with
  | Sys_error msg -> Error ("File error: " ^ msg)
  | e -> Error (Printexc.to_string e)

let parse_local_files (directory_path : string) : ((role * string local) list, string) result =
  try
    let files = Sys.readdir directory_path in
    let file_list = Array.to_list files in
    let st_files = List.filter (fun f -> Filename.extension f = ".st") file_list in
    let parsed_files = List.map (fun filename ->
      let filepath = Filename.concat directory_path filename in
      let role = Filename.chop_extension filename in
      let ic = open_in filepath in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let lexbuf = Lexing.from_string content in
      try (role, Parser.lfile Lexer.token lexbuf)
      with
      | Parser.Error ->
          let pos = lexbuf.lex_curr_p in
          failwith (sprintf "Parse error in %s at line %d" filename pos.pos_lnum)
      | e -> failwith (sprintf "Error parsing %s: %s" filename (Printexc.to_string e))
    ) st_files in
    Ok parsed_files
  with
  | Sys_error msg -> Error ("Directory error: " ^ msg)
  | Failure msg -> Error msg
  | e -> Error (Printexc.to_string e)

let parse_local_file (filepath : string) : (string local, string) result =
  try
    let ic = open_in filepath in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let lexbuf = Lexing.from_string content in
    try Ok (Parser.lfile Lexer.token lexbuf)
    with
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        Error (sprintf "Parse error at line %d" pos.pos_lnum)
    | e -> Error (Printexc.to_string e)
  with
  | Sys_error msg -> Error ("File error: " ^ msg)
  | e -> Error (Printexc.to_string e)

(* Projection / synthesis helpers ----------------------------------- *)

let test_projectability (global_type : string global) (roles : role list)
  : bool * int option * bool * bool * bool =
  let coinductive_ok, cf_size =
    try
      let gt_encoded = Normalise.encode global_type in
      let aut = Automaton.of_global gt_encoded in
      let projections = List.map (fun participant ->
        match Coinductive.project aut participant with
        | Ok local_aut ->
            let local_type = Automaton_to_local.automaton_to_local local_aut in
            Some (Size.size_local local_type)
        | Error _ -> None
      ) roles in
      if List.for_all Option.is_some projections then
        let total = List.fold_left (fun acc opt -> acc + Option.get opt) 0 projections in
        (true, Some total)
      else
        (false, None)
    with _ -> (false, None)
  in
  let coinductive_plain_ok =
    try
      let gt_encoded = Normalise.encode global_type in
      let aut = Automaton.of_global gt_encoded in
      List.for_all (fun participant ->
        match Projection_strict.project aut participant with
        | Ok _ -> true
        | Error _ -> false
      ) roles
    with _ -> false
  in
  let inductive_full_ok =
    List.for_all (fun participant ->
      match Inductive.project ~mode:Inductive.Full global_type participant with
      | Ok _ -> true
      | Error _ -> false
    ) roles
  in
  let inductive_plain_ok =
    List.for_all (fun participant ->
      match Inductive.project ~mode:Inductive.Plain global_type participant with
      | Ok _ -> true
      | Error _ -> false
    ) roles
  in
  (coinductive_ok, cf_size, coinductive_plain_ok, inductive_full_ok, inductive_plain_ok)

let test_synthesis (local_types : (role * string local) list) : (int * int global) option =
  try
    let roles = List.map fst local_types in
    let role_array = Array.of_list roles in
    let role_to_int role =
      match Array.find_index ((=) role) role_array with
      | Some i -> i
      | None -> failwith ("Unknown role: " ^ role)
    in
    let automata = List.map (fun (_, local_ast) ->
      let encoded = Normalise.encode_local local_ast in
      Local_automaton.of_local_int encoded role_to_int
    ) local_types in
    let cfsm = Array.of_list automata in
    let global_result = Synthesis_alg.synth cfsm role_array in
    let size = Size.size_global global_result in
    Some (size, global_result)
  with _ -> None

(* Printing --------------------------------------------------------- *)

type global_summary = string * string global * int * int * bool * int option * bool * bool * bool * bool * bool * bool
type local_summary  = string * int * int list * (int * int global) option * bool

let print_global_analysis_table (data : global_summary list) : unit =
  printf "\n=== GLOBAL SESSION TYPES ===\n";
  printf "%-18s | %2s | %4s | %6s | %-2s | %-2s | %-2s | %-2s | %-2s | %-2s | %-3s\n"
    "Name" "#P" "|G|" "|Δ|" "μ" "||" "IP" "IF" "CP" "CF" "Bal";
  printf "%s\n" (String.make 82 '-');
  List.iter (fun (name, _, orig_size, num_participants, cf_ok, cf_size_opt, cp_ok, if_ok, ip_ok, has_mu, has_par, balanced) ->
    let mu_symbol = if has_mu then "✓" else "✗" in
    let par_symbol = if has_par then "✓" else "✗" in
    let bal_symbol = if balanced then "✓" else "✗" in
    let cf_symbol = if cf_ok then "✓" else "✗" in
    let cp_symbol = if cp_ok then "✓" else "✗" in
    let if_symbol = if if_ok then "✓" else "✗" in
    let ip_symbol = if ip_ok then "✓" else "✗" in
    let delta_size_str = match cf_size_opt with Some s -> string_of_int s | None -> "✗" in
    printf "%-18s | %2d | %4d | %5s | %-2s | %-4s | %-4s | %-4s | %-4s | %-4s | %-3s\n"
      (if String.length name > 18 then String.sub name 0 16 ^ ".." else name)
      num_participants orig_size delta_size_str mu_symbol par_symbol ip_symbol if_symbol cp_symbol cf_symbol bal_symbol
  ) data;
  printf "\n"

let print_local_analysis_table (data : local_summary list) : unit =
  printf "\n=== LOCAL SESSION TYPES ===\n";
  printf "%-18s | %2s | %7s | %6s | %-2s\n" "Name" "#P" "|Δ|" "|G|" "μ";
  printf "%s\n" (String.make 50 '-');
  List.iter (fun (name, num_roles, sizes, synth_result_opt, has_mu) ->
    let mu_symbol = if has_mu then "✓" else "✗" in
    let total_size = List.fold_left (+) 0 sizes in
    let synth_str = match synth_result_opt with
      | Some (sz, _) -> string_of_int sz
      | None -> "✗"
    in
    printf "%-18s | %2d | %6d | %6s | %-2s\n"
      (if String.length name > 18 then String.sub name 0 16 ^ ".." else name)
      num_roles total_size synth_str mu_symbol
  ) data;
  printf "\n"

let print_synthesised_types (data : local_summary list) : unit =
  printf "\n=== SYNTHESISED GLOBAL TYPES ===\n\n";
  let pp_int fmt i = Format.fprintf fmt "t%d" i in
  List.iter (fun (name, _, _, synth_result_opt, _) ->
    match synth_result_opt with
    | Some (size, global_type) ->
        printf "--- %s ---\n" name;
        if size > 100 then
          printf "(Type too large to display: size=%d)\n\n" size
        else
          printf "%s\n\n" (Pretty.string_of pp_int global_type)
    | None ->
        printf "--- %s ---\n" name;
        printf "Synthesis failed\n\n"
  ) data

(* Driver ----------------------------------------------------------- *)

let analyse_case_studies (base_path : string) : (global_summary list * local_summary list, string) result =
  if not (Sys.file_exists base_path && Sys.is_directory base_path) then
    Error (sprintf "Case studies directory not found: %s" base_path)
  else
    let global_path = Filename.concat base_path "global" in
    let local_path  = Filename.concat base_path "local"  in
    let global_data = ref [] in
    let local_data  = ref [] in

    (* globals *)
    if Sys.file_exists global_path && Sys.is_directory global_path then (
      let files = Sys.readdir global_path |> Array.to_list in
      let global_files = List.filter (fun f -> Filename.extension f = ".global") files in
      List.iter (fun filename ->
        let filepath = Filename.concat global_path filename in
        let name = Filename.chop_extension filename in
        match parse_global_file filepath with
        | Ok global_type ->
            let orig_size = Size.size_global global_type in
            let has_mu = has_mu_binders_global global_type in
            let has_par = has_parallel_composition_global global_type in
            let roles_set = Well_formed.roles_of_global global_type in
            let role_list = Well_formed.StringSet.elements roles_set in
            let num_participants = List.length role_list in
            let (cf_ok, cf_size, cp_ok, if_ok, ip_ok) = test_projectability global_type role_list in
            let balanced =
              let gt_enc = Normalise.encode global_type in
              let aut = Automaton.of_global gt_enc in
              Balance.is_balanced aut
            in
            global_data := (name, global_type, orig_size, num_participants, cf_ok, cf_size, cp_ok, if_ok, ip_ok, has_mu, has_par, balanced) :: !global_data;
            printf "Processed global: %s (size=%d, μ=%b, par=%b)\n" name orig_size has_mu has_par
        | Error msg ->
            printf "Failed to parse global %s: %s\n" name msg
      ) global_files
    ) else (
      printf "Global case studies directory not found: %s\n" global_path
    );

    (* locals *)
    if Sys.file_exists local_path && Sys.is_directory local_path then (
      let subdirs = Sys.readdir local_path |> Array.to_list in
      let local_dirs = List.filter (fun d ->
        Sys.is_directory (Filename.concat local_path d)) subdirs in
      List.iter (fun dirname ->
        let dirpath = Filename.concat local_path dirname in
        match parse_local_files dirpath with
        | Ok local_types ->
            let num_roles = List.length local_types in
            let sizes = List.map (fun (_, lt) -> Size.size_local lt) local_types in
            let has_mu = List.exists (fun (_, lt) -> has_mu_binders_local lt) local_types in
            let synth_result_opt = test_synthesis local_types in
            local_data := (dirname, num_roles, sizes, synth_result_opt, has_mu) :: !local_data;
            printf "Processed local: %s (%d participants, μ=%b)\n" dirname num_roles has_mu
        | Error msg ->
            printf "Failed to parse local %s: %s\n" dirname msg
      ) local_dirs
    ) else (
      printf "Local case studies directory not found: %s\n" local_path
    );
    Ok (List.rev !global_data, List.rev !local_data)

let run ?(print_types=true) base_path : unit =
  match analyse_case_studies base_path with
  | Error msg ->
      eprintf "Error: %s\n" msg;
      exit 1
  | Ok (globals, locals) ->
      print_global_analysis_table globals;
      print_local_analysis_table locals;
      if print_types then print_synthesised_types locals
