open Automaton

module IntSet = Automaton.IntSet
module RoleSet = Set.Make (String)
(*--------------------------------------------------------------------*)
(*  Balance analysis for global-type automata                         *)
(*                                                                    *)
(*  Intuition                                                         *)
(*  ---------                                                         *)
(*  •   A participant (role) is *reachable* from a state s if *some*  *)
(*      execution (path) that starts in s eventually reaches a state  *)
(*      mentioning that role.                                         *)
(*  •   A participant is *unavoidable* from s if *every* execution    *)
(*      that starts in s will reach a state mentioning that role      *)
(*      within finitely many steps.                                   *)
(*                                                                    *)
(*  Balanced                     ⟺   Reach(s) = Unav(s)  ∀ s          *)
(*--------------------------------------------------------------------*)

(* Collect roles of a state *)
let roles_of_state g id : RoleSet.t =
  let p, q = g.roles.(id) in
  RoleSet.(add p (singleton q))

(* successor states of a given state *)
let succs g id : IntSet.t list =
  match g.kinds.(id) with
  | Msg (_, s) -> [s]                       (* one nondeterministic choice among s *)
  | Bra bs     -> List.map snd bs           (* each branch provides its own successor set *)

(* fixed-point computation of reachable and unavoidable role sets *)
let compute_sets (g : graph) : RoleSet.t array * RoleSet.t array =
  let n = g.num_states in

  let reach  = Array.make n RoleSet.empty in
  let unav   = Array.make n RoleSet.empty in 
  
  (* init reach and unav with empty sets (bottom-up computation) *)
  for i = 0 to n - 1 do
    reach.(i) <- RoleSet.empty;
    unav.(i) <- RoleSet.empty
  done;

  (* Compute reachability sets by upward iteration (union) *)
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to n - 1 do
      let own_roles = roles_of_state g i in
      let union_succ =
        List.fold_left (fun acc set ->
            IntSet.fold (fun j acc' -> RoleSet.union reach.(j) acc') set acc)
          RoleSet.empty (succs g i)
      in
      let new_reach = RoleSet.union own_roles union_succ in
      if not (RoleSet.equal new_reach reach.(i)) then (reach.(i) <- new_reach; changed := true)
    done
  done;

  (* Compute unavoidability sets by upward iteration *)
  (* For Msg: union of successor unavoidability *)
  (* For Bra: intersection of branch unavoidability *)
  changed := true;
  while !changed do
    changed := false;
    for i = 0 to n - 1 do
      let own_roles = roles_of_state g i in
      let new_unav = match g.kinds.(i) with
        | Msg (_, succs) ->
            (* Union of successor unavoidability *)
            let union_succ = IntSet.fold (fun j acc -> RoleSet.union unav.(j) acc) succs RoleSet.empty in
            RoleSet.union own_roles union_succ
        | Bra branches ->
            (* Intersection of branch unavoidability *)
            let branch_unavs = List.map (fun (_, succs) ->
              IntSet.fold (fun j acc -> RoleSet.union unav.(j) acc) succs RoleSet.empty
            ) branches in
            let intersection = match branch_unavs with
              | [] -> RoleSet.empty
              | first :: rest -> List.fold_left RoleSet.inter first rest
            in
            RoleSet.union own_roles intersection
      in
      if not (RoleSet.equal new_unav unav.(i)) then (unav.(i) <- new_unav; changed := true)
    done
  done;
  
  reach, unav

let is_balanced (g : graph) : bool =
  let reach, unav = compute_sets g in
  let balanced = ref true in
  for i = 0 to g.num_states - 1 do
    if not (RoleSet.equal reach.(i) unav.(i)) then balanced := false
  done;
  !balanced 