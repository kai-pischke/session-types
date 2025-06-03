(*--------------------------------------------------------------------*)
(*  Well-formedness checks for global session types                    *)
(*--------------------------------------------------------------------*)
open Ast

module StringSet = Set.Make (String)

exception Error of Loc.t * string

let error loc msg = raise (Error (loc, msg))

(*--------------------------------------------------------------------*)
(*  Collect participants (roles)                                       *)
(*--------------------------------------------------------------------*)
let rec roles_of_global (g : global) : StringSet.t =
  let open StringSet in
  match g with
  | GEnd _ | GVar _ -> empty
  | GRec (_, body, _) -> roles_of_global body
  | GMsg (p, q, branches, _) ->
      let acc = add p (add q empty) in
      List.fold_left
        (fun s (_, g') -> union s (roles_of_global g'))
        acc branches
  | GPar (g1, g2, _) -> union (roles_of_global g1) (roles_of_global g2)

(*--------------------------------------------------------------------*)
(*  Free recursion variables                                           *)
(*--------------------------------------------------------------------*)
let rec free_vars (g : global) (bound : StringSet.t) : StringSet.t =
  match g with
  | GEnd _ -> StringSet.empty
  | GVar (v, _) -> if StringSet.mem v bound then StringSet.empty else StringSet.singleton v
  | GRec (v, body, _) -> free_vars body (StringSet.add v bound)
  | GMsg (_, _, branches, _) ->
      List.fold_left
        (fun acc (_, g') -> StringSet.union acc (free_vars g' bound))
        StringSet.empty branches
  | GPar (g1, g2, _) ->
      StringSet.union (free_vars g1 bound) (free_vars g2 bound)

(*--------------------------------------------------------------------*)
(*  Check #1: self-messaging                                           *)
(*--------------------------------------------------------------------*)
let rec check_no_self_msg (g : global) : unit =
  match g with
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> 
      check_no_self_msg body
  | GMsg (p, q, branches, loc) ->
      if p = q then error loc (Printf.sprintf "Self-messaging: role %s sends to itself" p);
      List.iter (fun (_, g') -> check_no_self_msg g') branches
  | GPar (g1, g2, _) ->
      check_no_self_msg g1; check_no_self_msg g2

(*--------------------------------------------------------------------*)
(*  Check #2: overlapping roles in parallel                            *)
(*--------------------------------------------------------------------*)
let rec check_parallel_roles (g : global) : unit =
  match g with
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> check_parallel_roles body
  | GMsg (_, _, branches, _) ->
      List.iter (fun (_, g') -> check_parallel_roles g') branches
  | GPar (g1, g2, loc) ->
      let r1 = roles_of_global g1 in
      let r2 = roles_of_global g2 in
      if not (StringSet.is_empty (StringSet.inter r1 r2)) then
        error loc "Parallel components share roles";
      check_parallel_roles g1; check_parallel_roles g2

(*--------------------------------------------------------------------*)
(*  Check #3: unbound recursion variables                              *)
(*--------------------------------------------------------------------*)
let check_unbound_variables (g : global) : unit =
  let rec aux env = function
    | GEnd _ -> ()
    | GVar (v, loc) ->
        if not (StringSet.mem v env) then error loc ("Unbound recursion variable " ^ v)
    | GRec (v, body, _) -> aux (StringSet.add v env) body
    | GMsg (_, _, branches, _) -> List.iter (fun (_, g') -> aux env g') branches
    | GPar (g1, g2, _) -> aux env g1; aux env g2
  in
  aux StringSet.empty g

(*--------------------------------------------------------------------*)
(*  Check #4: components of a parallel must be closed                  *)
(*--------------------------------------------------------------------*)
let rec check_parallel_closed (g : global) : unit =
  match g with
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> check_parallel_closed body
  | GMsg (_, _, branches, _) -> List.iter (fun (_, g') -> check_parallel_closed g') branches
  | GPar (g1, g2, loc) ->
      let fv1 = free_vars g1 StringSet.empty in
      let fv2 = free_vars g2 StringSet.empty in
      if not (StringSet.is_empty fv1) then
        error loc "Left branch of parallel is not closed (free recursion vars)";
      if not (StringSet.is_empty fv2) then
        error loc "Right branch of parallel is not closed (free recursion vars)";
      check_parallel_closed g1; check_parallel_closed g2

(*--------------------------------------------------------------------*)
(*  Check #5: guarded recursion                                        *)
(*--------------------------------------------------------------------*)
let check_unguarded_recursion (g : global) : unit =
  (* For every [rec t. body] ensure every occurrence of [t] is under a GMsg. *)
  let rec guarded var guard = function
    | GEnd _ -> ()
    | GVar (v, loc) when v = var -> if not guard then error loc ("Unguarded recursion variable " ^ v)
    | GVar _ -> ()
    | GRec (v, body, _) -> guarded v false body  (* inner shadowing *)
    | GMsg (_, _, branches, _) ->
        List.iter (fun (_, g') -> guarded var true g') branches
    | GPar (g1, g2, _) ->
        guarded var guard g1; guarded var guard g2
  in
  let rec visit = function
    | GEnd _ | GVar _ -> ()
    | GRec (v, body, _) -> guarded v false body; visit body
    | GMsg (_, _, branches, _) -> List.iter (fun (_, g') -> visit g') branches
    | GPar (g1, g2, _) -> visit g1; visit g2
  in
  visit g

(*--------------------------------------------------------------------*)
(*  Public entry                                                      *)
(*--------------------------------------------------------------------*)
let check_global (g : global) : unit =
  check_no_self_msg g;
  check_unbound_variables g;
  check_parallel_roles g;
  check_parallel_closed g;
  check_unguarded_recursion g

