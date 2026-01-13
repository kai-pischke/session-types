open Ast

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(* Type-safe representation of destinations during building *)
type destination =
  | State of int                    (* Direct state reference *)
  | Variable of int                 (* Variable reference to be resolved *)

(* Intermediate state kind during building *)
type intermediate_state_kind =
  | Msg of base * destination list    (* base + successor list *)
  | Bra of (label * destination list) list   (* branching on labels *)

(* Final state kind after resolution *)
type state_kind =
  | Msg of base * IntSet.t            (* base + successor set *)
  | Bra of (label * IntSet.t) list    (* branching on labels *)

(* The automaton/graph: *)
type graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_states : IntSet.t;             (* S_0: set of start states        *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : (role * role) array;  (* id ↦ (p,q)                       *)
  kinds        : state_kind array;     (* id ↦ behaviour (Msg | Bra)      *)
}

(* Helper function to convert destination list to IntSet *)
let destinations_to_intset (dests : destination list) (var_map : int IntMap.t) : IntSet.t =
  List.fold_left (fun acc dest ->
    match dest with
    | State id -> IntSet.add id acc
    | Variable var_id ->
        (match IntMap.find_opt var_id var_map with
         | Some state_id -> IntSet.add state_id acc
         | None -> acc)  (* Variable not found, skip *)
  ) IntSet.empty dests

(* Convert intermediate state kind to final state kind *)
let resolve_state_kind (kind : intermediate_state_kind) (var_map : int IntMap.t) : state_kind =
  match kind with
  | Msg (base, dests) ->
      let resolved_dests = destinations_to_intset dests var_map in
      Msg (base, resolved_dests)
  | Bra branches ->
      let resolved_branches = List.map (fun (label, dests) ->
        let resolved_dests = destinations_to_intset dests var_map in
        (label, resolved_dests)
      ) branches in
      Bra resolved_branches

(* Helper to create initial intermediate state kind *)
let initial_intermediate_state_kind () : intermediate_state_kind =
  Msg ("", [])

let of_global (g : int global) : graph =
  (* Fresh state ids *)
  let counter = ref 0 in
  let fresh_state () = 
    let id = !counter in 
    if id >= 1000 then 
      failwith "Too many states in automaton (max 1000)"
    else 
      incr counter; id 
  in
  
  (* Variable mapping: var_name -> state_id *)
  let var_map = ref (IntMap.empty : int IntMap.t) in
  
  (* Track all states created during traversal *)
  let all_states = ref IntSet.empty in
  
  (* Pre-allocate arrays with a reasonable size *)
  let max_states = 1000 in
  let roles = Array.make max_states ("", "") in
  let intermediate_kinds = Array.make max_states (initial_intermediate_state_kind ()) in
  
  (* Helper to add a state *)
  let add_state p q kind =
    let state_id = fresh_state () in
    all_states := IntSet.add state_id !all_states;
    roles.(state_id) <- (p, q);
    intermediate_kinds.(state_id) <- kind;
    state_id
  in
  
  (* Build the graph by traversing the global type *)
  let rec build_graph (g : int global) : destination list =
    match g with
    | GEnd _ -> 
        (* GEnd doesn't correspond to a state *)
        []
    | GVar (v, _) -> 
        (* Variable reference *)
        [Variable v]
    | GRec (v, body, _) ->
        (* For GRec, we need to know the state ID of the body first *)
        let body_dests = build_graph body in
        (* The GRec node shares the same ID as its body *)
        (* Store the mapping for variable resolution *)
        (match body_dests with
         | [State state_id] -> var_map := IntMap.add v state_id !var_map
         | _ -> ());
        body_dests
    | GBra (p, q, branches, _) ->
        (* Process all branches to get their successor destinations *)
        let branch_results = List.map (fun (label, branch_g) ->
          let branch_dests = build_graph branch_g in
          (label, branch_dests)
        ) branches in
        (* Create branching transitions *)
        let branches_with_dests = List.map (fun (label, dests) ->
          (label, dests)
        ) branch_results in
        (* Create the state *)
        let state_id = add_state p q (Bra branches_with_dests) in
        [State state_id]
    | GMsg (p, q, base, cont, _) ->
        let cont_dests = build_graph cont in
        (* Create the state *)
        let state_id = add_state p q (Msg (base, cont_dests)) in
        [State state_id]
    | GPar (left, right, _) ->
        (* For parallel composition, we need to handle both sides *)
        let left_dests = build_graph left in
        let right_dests = build_graph right in
        (* Combine both sides *)
        left_dests @ right_dests
  in
  
  (* Collect all start states from parallel composition *)
  let rec collect_start_states (g : int global) : IntSet.t =
    match g with
    | GEnd _ -> IntSet.empty
    | GVar (v, _) ->
        (match IntMap.find_opt v !var_map with
         | Some state_id -> IntSet.singleton state_id
         | None -> IntSet.empty)
    | GRec (_, body, _) ->
        (* For GRec, the start state is the state of the body *)
        collect_start_states body
    | GBra (_, _, branches, _) ->
        (* For branching, we need to collect start states from all branches *)
        List.fold_left (fun acc (_, branch_g) ->
          IntSet.union acc (collect_start_states branch_g)
        ) IntSet.empty branches
    | GMsg (_, _, _, cont, _) ->
        (* For messages, the start state is the start state of the continuation *)
        collect_start_states cont
    | GPar (left, right, _) ->
        (* For parallel composition, we collect start states from both sides *)
        IntSet.union (collect_start_states left) (collect_start_states right)
  in
  
  (* First pass: build the graph structure *)
  let start_dests = build_graph g in
  
  (* Second pass: resolve variable references and convert to final format *)
  let final_kinds = Array.init !counter (fun i ->
    resolve_state_kind intermediate_kinds.(i) !var_map
  ) in
  
  (* Determine start states *)
  let start_states = 
    match start_dests with
    | [] -> collect_start_states g
    | [State state_id] -> IntSet.singleton state_id
    | [Variable var_id] ->
        (match IntMap.find_opt var_id !var_map with
         | Some state_id -> IntSet.singleton state_id
         | None -> IntSet.empty)
    | _ ->
        List.fold_left (fun acc dest ->
          match dest with
          | State state_id -> IntSet.add state_id acc
          | Variable var_id ->
              (match IntMap.find_opt var_id !var_map with
               | Some state_id -> IntSet.add state_id acc
               | None -> acc)
        ) IntSet.empty start_dests
  in
  
  {
    num_states = !counter;
    start_states;
    roles = Array.sub roles 0 !counter;
    kinds = final_kinds;
  }

(* Pretty printing for debugging and tests *)
let pp_state fmt (id : int) (roles : (role * role) array) =
  let (p, q) = roles.(id) in
  Format.fprintf fmt "state %d (%s,%s)" id p q

let pp_state_kind fmt (id : int) (kinds : state_kind array) =
  match kinds.(id) with
  | Msg (_, dsts) when IntSet.is_empty dsts -> () (* no transitions *)
  | Msg (base, dsts) ->
      Format.fprintf fmt "from %d --base %s--> %s\n" id base
        (String.concat "," (List.map string_of_int (IntSet.elements dsts)))
  | Bra branches ->
      List.iter (fun (lbl, dsts) ->
        Format.fprintf fmt "from %d --label %s--> %s\n" id lbl
          (String.concat "," (List.map string_of_int (IntSet.elements dsts)))
      ) branches

let pp_graph fmt (g : graph) =
  let state_ids = List.init g.num_states (fun i -> i) in
  Format.fprintf fmt "States: %s\n" 
    (String.concat ", " (List.map string_of_int state_ids));
  Format.fprintf fmt "Start states: %s\n" 
    (String.concat ", " (List.map string_of_int (IntSet.elements g.start_states)));
  List.iter (fun id -> 
    pp_state fmt id g.roles;
    Format.fprintf fmt "\n"
  ) state_ids;
  (* Print transitions in order of source state ID *)
  List.iter (fun id -> 
    pp_state_kind fmt id g.kinds
  ) state_ids

let string_of_graph g =
  Format.asprintf "%a" pp_graph g

(* DOT / JSON export *)
let escape s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let dot_of_graph (g : graph) : string =
  let b = Buffer.create 512 in
  Buffer.add_string b "digraph global {\n  rankdir=LR;\n";
  if not (IntSet.is_empty g.start_states) then (
    Buffer.add_string b "  start [shape=point];\n";
    IntSet.iter (fun s -> Buffer.add_string b (Printf.sprintf "  start -> %d;\n" s)) g.start_states
  );
  for i = 0 to g.num_states - 1 do
    let p, q = g.roles.(i) in
    Buffer.add_string b (Printf.sprintf "  %d [label=\"%d: %s -> %s\"];\n" i i (escape p) (escape q))
  done;
  let add_edge src dst lbl =
    Buffer.add_string b (Printf.sprintf "  %d -> %d [label=\"%s\"];\n" src dst (escape lbl))
  in
  for i = 0 to g.num_states - 1 do
    match g.kinds.(i) with
    | Msg (base, dsts) ->
        IntSet.iter (fun d -> add_edge i d base) dsts
    | Bra branches ->
        List.iter (fun (lbl, dsts) ->
          IntSet.iter (fun d -> add_edge i d lbl) dsts
        ) branches
  done;
  Buffer.add_string b "}\n";
  Buffer.contents b

let json_of_graph (g : graph) : string =
  let state_json i =
    let p, q = g.roles.(i) in
    match g.kinds.(i) with
    | Msg (base, dsts) ->
        let dst_list = IntSet.elements dsts |> List.map string_of_int |> String.concat "," in
        Printf.sprintf
          {|{"id":%d,"roles":{"sender":"%s","receiver":"%s"},"kind":{"tag":"msg","base":"%s","dests":[%s]}}|}
          i (escape p) (escape q) (escape base) dst_list
    | Bra branches ->
        let branches_json =
          branches
          |> List.map (fun (lbl, dsts) ->
                 let dst_list = IntSet.elements dsts |> List.map string_of_int |> String.concat "," in
                 Printf.sprintf {|{"label":"%s","dests":[%s]}|} (escape lbl) dst_list)
          |> String.concat ","
        in
        Printf.sprintf
          {|{"id":%d,"roles":{"sender":"%s","receiver":"%s"},"kind":{"tag":"bra","branches":[%s]}}|}
          i (escape p) (escape q) branches_json
  in
  let states =
    List.init g.num_states state_json |> String.concat ","
  in
  let starts =
    g.start_states |> IntSet.elements |> List.map string_of_int |> String.concat ","
  in
  Printf.sprintf {|{"start_states":[%s],"states":[%s]}|} starts states
