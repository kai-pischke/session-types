(*--------------------------------------------------------------------*)
(*  Well-formedness checks for global session types                   *)
(*--------------------------------------------------------------------*)
open Ast

module StringSet = Set.Make (String)
module VarMap    = Map.Make (String)

exception Error of Loc.t * string
let error loc msg = raise (Error (loc, msg))

(*--------------------------------------------------------------------*)
(*  Helpers                                                            *)
(*--------------------------------------------------------------------*)
let rec roles_of_global (g : global) : StringSet.t =
  match g with
  | GEnd _ | GVar _ -> StringSet.empty
  | GRec (_, body, _) -> roles_of_global body
  | GMsg (p, q, branches, _) ->
      List.fold_left
        (fun s (_, g') -> StringSet.union s (roles_of_global g'))
        (StringSet.add p (StringSet.singleton q)) branches
  | GPar (g1, g2, _) -> StringSet.union (roles_of_global g1) (roles_of_global g2)

let rec free_vars (g : global) (bound : StringSet.t) : StringSet.t =
  match g with
  | GEnd _ -> StringSet.empty
  | GVar (v, _) -> if StringSet.mem v bound then StringSet.empty else StringSet.singleton v
  | GRec (v, body, _) -> free_vars body (StringSet.add v bound)
  | GMsg (_, _, bs, _) ->
      List.fold_left (fun acc (_, g') -> StringSet.union acc (free_vars g' bound))
        StringSet.empty bs
  | GPar (g1, g2, _) -> StringSet.union (free_vars g1 bound) (free_vars g2 bound)

(*--------------------------------------------------------------------*)
(*  Check #1: no self-messaging                                       *)
(*--------------------------------------------------------------------*)
let rec check_no_self_msg = function
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> check_no_self_msg body
  | GMsg (p, q, branches, loc) ->
      if p = q then error loc (Printf.sprintf "Self-messaging: role %s sends to itself" p);
      List.iter (fun (_, g') -> check_no_self_msg g') branches
  | GPar (g1, g2, _) -> check_no_self_msg g1; check_no_self_msg g2

(*--------------------------------------------------------------------*)
(*  Check #2: overlapping roles in parallel                           *)
(*--------------------------------------------------------------------*)
let rec check_parallel_roles = function
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> check_parallel_roles body
  | GMsg (_, _, bs, _) -> List.iter (fun (_, g') -> check_parallel_roles g') bs
  | GPar (g1, g2, loc) ->
      let i = StringSet.inter (roles_of_global g1) (roles_of_global g2) in
      if not (StringSet.is_empty i) then error loc "Parallel components share roles";
      check_parallel_roles g1; check_parallel_roles g2

(*--------------------------------------------------------------------*)
(*  Check #3: unbound recursion variables                              *)
(*--------------------------------------------------------------------*)
let check_unbound_variables (g : global) : unit =
  let rec aux env = function
    | GEnd _ -> ()
    | GVar (v, loc) -> if not (StringSet.mem v env) then error loc ("Unbound recursion variable " ^ v)
    | GRec (v, body, _) -> aux (StringSet.add v env) body
    | GMsg (_, _, bs, _) -> List.iter (fun (_, g') -> aux env g') bs
    | GPar (g1, g2, _) -> aux env g1; aux env g2
  in
  aux StringSet.empty g

(*--------------------------------------------------------------------*)
(*  Check #4: parallel components must be closed                       *)
(*--------------------------------------------------------------------*)
let rec check_parallel_closed = function
  | GEnd _ | GVar _ -> ()
  | GRec (_, body, _) -> check_parallel_closed body
  | GMsg (_, _, bs, _) -> List.iter (fun (_, g') -> check_parallel_closed g') bs
  | GPar (g1, g2, loc) ->
      let fv1 = free_vars g1 StringSet.empty in
      let fv2 = free_vars g2 StringSet.empty in
      if not (StringSet.is_empty fv1) then error loc "Left branch of parallel is not closed";
      if not (StringSet.is_empty fv2) then error loc "Right branch of parallel is not closed";
      check_parallel_closed g1; check_parallel_closed g2

(*--------------------------------------------------------------------*)
(*  Check #5: guarded recursion                                       *)
(*--------------------------------------------------------------------*)
(*  We track, for each bound variable in scope, whether we have seen a
    communication (GMsg) on the path so far.  A GMsg flips the [guard]
    flag to [true] for the recursive descent of that branch.  If we
    reach an occurrence of [v] with [guard = false], the recursion is
    unguarded.  The environment is a map var ↦ guard-flag. *)

let check_unguarded_recursion (g : global) : unit =
  let rec trav guard env = function
    | GEnd _ -> ()
    | GVar (v, loc) ->
        (match VarMap.find_opt v env with
         | Some false when not guard -> error loc ("Unguarded recursion variable " ^ v)
         | _ -> ())
    | GRec (v, body, _) ->
        let env' = VarMap.add v false env in   (* newly bound, not yet guarded *)
        trav guard env' body
    | GMsg (_, _, branches, _) ->
        List.iter (fun (_, g') -> trav true env g') branches
    | GPar (g1, g2, _) -> trav guard env g1; trav guard env g2
  in
  trav false VarMap.empty g

(*--------------------------------------------------------------------*)
(*  Public entry                                                      *)
(*--------------------------------------------------------------------*)
let check_global g =
  check_no_self_msg g;
  check_unbound_variables g;
  check_parallel_roles g;
  check_parallel_closed g;
  check_unguarded_recursion g