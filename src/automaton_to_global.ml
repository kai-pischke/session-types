open Ast
open Encode
open Automaton

module IntMap = Map.Make(Int)

(* ------------------------------------------------------------------ *)
(*  Helpers                                                          *)
(* ------------------------------------------------------------------ *)

(* Fresh variable names based on state id *)
let var_of_state st = Printf.sprintf "t%d" st

(* ------------------------------------------------------------------ *)
(*  Compute set of recursive states using Tarjan SCC                  *)
(* ------------------------------------------------------------------ *)

let compute_recursive_states (g : graph) : IntSet.t =
  let n = g.num_states in
  let idx   = Array.make n (-1) in
  let low   = Array.make n 0 in
  let onstk = Array.make n false in
  let stack = ref [] in
  let index = ref 0 in
  let rec_set = ref IntSet.empty in

  let push v = stack := v :: !stack; onstk.(v) <- true in

  let rec pop_component v acc st =
    match st with
    | [] -> failwith "tarjan: empty stack"
    | x::xs ->
        onstk.(x) <- false;
        let acc' = IntSet.add x acc in
        if x = v then (xs, acc') else pop_component v acc' xs
  in

  let add_scc scc =
    let size = IntSet.cardinal scc in
    if size > 1 then rec_set := IntSet.union scc !rec_set
    else (
      let v = IntSet.choose scc in
      let succs = match g.kinds.(v) with
        | Msg (_, s) -> s
        | Bra brs -> List.fold_left (fun acc (_,s)-> IntSet.union acc s) IntSet.empty brs
      in
      if IntSet.mem v succs then rec_set := IntSet.add v !rec_set)
  in

  let rec dfs v =
    idx.(v) <- !index;
    low.(v) <- !index;
    index := !index + 1;
    push v;

    let succs = match g.kinds.(v) with
      | Msg (_, s) -> s
      | Bra brs -> List.fold_left (fun acc (_,s)-> IntSet.union acc s) IntSet.empty brs
    in
    IntSet.iter (fun w ->
      if idx.(w) = -1 then (
        dfs w;
        low.(v) <- min low.(v) low.(w)
      ) else if onstk.(w) then (
        low.(v) <- min low.(v) idx.(w)
      )
    ) succs;

    if low.(v) = idx.(v) then (
      let st', scc = pop_component v IntSet.empty !stack in
      stack := st';
      add_scc scc
    )
  in

  for v = 0 to n-1 do
    if idx.(v) = -1 then dfs v
  done;
  !rec_set

(* convert : graph -> state -> env -> in_stack -> (string global * bool)
   env      : int -> string  (state  ↦ var-name)  (already assigned)
   in_stack : IntSet.t       (states currently being converted – recursion stack)
   returns  : global * bool  (global, does_body_reference_current_state_var)
*)
let rec convert (g : graph)
                (recursive_states : IntSet.t)
                (state : IntSet.elt)
                (env : (string IntMap.t))
                (in_stack : IntSet.t)
  : string global * bool =
  (* If we have seen this state before *)
  if IntMap.mem state env then
    let v = IntMap.find state env in
    if IntSet.mem state in_stack then
      (* Back-edge → recursive reference *)
      (GVar (v, Loc.dummy), true)
    else
      (* Forward/side reference – still a variable, but does not create recursion
         for the caller.                                                     *)
      (GVar (v, Loc.dummy), false)
  else
    (* First time we visit this state – assign a variable name but delay the
       binding decision until we know if it is actually used recursively.    *)
    let var_name = var_of_state state in
    let env' = IntMap.add state var_name env in
    let in_stack' = IntSet.add state in_stack in

    (* Build continuation helper *)
    let convert_succ s = convert g recursive_states s env' in_stack' in

    (* Determine node kind *)
    let node, used =
      match g.kinds.(state) with
      | Msg (base, succs) ->
          let sender, receiver = g.roles.(state) in
          let cont, used_in_cont =
            if IntSet.is_empty succs then (GEnd Loc.dummy, false)
            else if IntSet.cardinal succs = 1 then
              let n = IntSet.choose succs in
              convert_succ n
            else
              (* Parallel successors *)
              let globs, used_flags =
                IntSet.elements succs
                |> List.map convert_succ
                |> List.split in
              let par =
                List.fold_left (fun acc br -> GPar (acc, br, Loc.dummy))
                  (List.hd globs) (List.tl globs)
              in
              (par, List.exists (fun x -> x) used_flags)
          in
          (GMsg (sender, receiver, base, cont, Loc.dummy), used_in_cont)

      | Bra branches ->
          let sender, receiver = g.roles.(state) in
          let brs, used_flags =
            List.map (fun (lbl, succs) ->
              if IntSet.is_empty succs then ((lbl, GEnd Loc.dummy), false)
              else if IntSet.cardinal succs = 1 then
                let n = IntSet.choose succs in
                let g', u = convert_succ n in
                ((lbl, g'), u)
              else
                let globs, flags =
                  IntSet.elements succs |> List.map convert_succ |> List.split in
                let par =
                  List.fold_left (fun acc br -> GPar (acc, br, Loc.dummy))
                    (List.hd globs) (List.tl globs) in
                ((lbl, par), List.exists (fun x -> x) flags)
            ) branches
            |> List.split in
          (GBra (sender, receiver, brs, Loc.dummy), List.exists (fun x->x) used_flags)
    in
    let _ = IntSet.remove state in_stack' in
    (* If this state is recursive, always bind it with GRec *)
    if IntSet.mem state recursive_states then
      (GRec (var_name, node, Loc.dummy), false)
    else
      (node, used)

(* Entry for a given start state – ensures env is shared *)

let convert_start (g : graph) (recursive_states:IntSet.t) (state : IntSet.elt) (env_ref : (string IntMap.t) ref)
  : string global =
  let env = !env_ref in
  let glob, _ = convert g recursive_states state env IntSet.empty in
  env_ref := env; (* keep env for later starts *)
  glob

(* ------------------------------------------------------------------ *)
(*  Public API                                                        *)
(* ------------------------------------------------------------------ *)

let automaton_to_global (g : graph) : int global =
  if IntSet.is_empty g.start_states then
    GEnd Loc.dummy |> encode
  else
    let rec_states = compute_recursive_states g in
    let env = ref IntMap.empty in
    let globs_rev =
      IntSet.fold (fun st acc -> (convert_start g rec_states st env)::acc) g.start_states [] in
    let globs = List.rev globs_rev in
    let string_global =
      match globs with
      | [] -> GEnd Loc.dummy
      | [g] -> g
      | g1::rest -> List.fold_left (fun acc br -> GPar (acc, br, Loc.dummy)) g1 rest
    in
    encode string_global 