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
(*                                                                    *)
(*  The equations                                                     *)
(*  ----------------                                                  *)
(*  Let SuccSets(s) be the list of successor sets determined by the   *)
(*  outgoing transition(s) of s:                                      *)
(*      • Msg(_, S)          →  [ S ]                                 *)
(*      • Bra [ (lbl₁,S₁); … ] → [ S₁; … ]                            *)
(*                                                                    *)
(*   Reach(s) = roles(s) ∪ ⋃_{S∈SuccSets(s)} ⋃_{t∈S} Reach(t)         *)
(*   Unav(s)  = roles(s) ∪ ⋂_{S∈SuccSets(s)} ⋃_{t∈S} Unav(t)          *)
(*                                                                    *)
(*  These are two monotone functions on the lattice (RoleSet, ⊆).     *)
(*  • Reach is the *least* fix-point  →  computed bottom-up (↑).       *)
(*  • Unav  is the *greatest* fix-point →  computed top-down (↓),      *)
(*    using a worklist to propagate removals through cycles.           *)
(*    Special case: for a pure self-loop, only the state's own roles   *)
(*    are unavoidable.                                                *)
(*                                                                    *)
(*  Correctness argument (sketch)                                      *)
(*  -----------------------------                                      *)
(*  1.  The upward iteration for Reach starts at roles(s) and only     *)
(*      adds roles appearing deeper in the graph, so it grows along   *)
(*      the ascending chain and stabilises at the least fix-point.    *)
(*  2.  The downward work-list for Unav starts at the universe U and   *)
(*      removes roles as justified by the equations above, including  *)
(*      through cycles, and for pure self-loops, only the state's     *)
(*      own roles remain. When the process stabilises we have reached *)
(*      the greatest fix-point.                                       *)
(*  3.  By definition, balanced ⇔ Reach = Unav, hence the predicate    *)
(*      computed below is correct.                                    *)
(*                                                                    *)
(*  Complexity                                                        *)
(*  ----------                                                        *)
(*  Let n = |States|, m = |Edges|, r = |Roles|.                       *)
(*  •  Upward phase: every edge can add at most r roles → O(r·m).      *)
(*  •  Downward phase: every edge can cause at most r removals →       *)
(*     also O(r·m).                                                    *)
(*  Thus the overall asymptotic complexity is O(r·m) which is optimal *)
(*  for such data-flow analysis.                                       *)
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
  (* Universe of all roles in the automaton *)
  let universe =
    let acc = ref RoleSet.empty in
    for i = 0 to n - 1 do
      acc := RoleSet.union !acc (roles_of_state g i)
    done;
    !acc
  in

  let reach  = Array.make n RoleSet.empty in
  let unav   = Array.make n universe in  (* start from top element *)
  (* init reach with own roles *)
  for i = 0 to n - 1 do
    reach.(i) <- roles_of_state g i
  done;

  (* Build predecessor sets to enable efficient work-list update *)
  let preds = Array.make n IntSet.empty in
  for i = 0 to n - 1 do
    List.iter (fun succ_set ->
      IntSet.iter (fun j -> preds.(j) <- IntSet.add i preds.(j)) succ_set
    ) (succs g i)
  done;

  (* Work-list initialised with every state *)
  let q = Queue.create () in
  for i = 0 to n - 1 do Queue.push i q done;

  (* Compute reachability sets by upward iteration (union) *)
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to n - 1 do
      let union_succ =
        List.fold_left (fun acc set ->
            IntSet.fold (fun j acc' -> RoleSet.union reach.(j) acc') set acc)
          RoleSet.empty (succs g i)
      in
      let new_reach = RoleSet.union (roles_of_state g i) union_succ in
      if not (RoleSet.equal new_reach reach.(i)) then (reach.(i) <- new_reach; changed := true)
    done
  done;

  (* Build unav sets by downward iteration (intersection) with worklist *)
  let worklist = Queue.create () in
  for i = 0 to n - 1 do Queue.push i worklist done;
  while not (Queue.is_empty worklist) do
    let i = Queue.pop worklist in
    let choices = succs g i in
    let inter_succ =
      match choices with
      | [] -> RoleSet.empty
      | [succ_set] when IntSet.cardinal succ_set = 1 && IntSet.mem i succ_set ->
          (* Pure self-loop: unavoidable set is just own roles *)
          RoleSet.empty
      | _ ->
          List.fold_left (fun acc choice ->
            if IntSet.is_empty choice then
              RoleSet.empty
            else
              let union_choice =
                IntSet.fold (fun j accc -> RoleSet.union accc unav.(j)) choice RoleSet.empty
              in
              if RoleSet.is_empty acc then union_choice else RoleSet.inter acc union_choice
          ) RoleSet.empty choices
    in
    let new_unav = RoleSet.union (roles_of_state g i) inter_succ in
    if not (RoleSet.equal new_unav unav.(i)) then begin
      unav.(i) <- new_unav;
      IntSet.iter (fun p -> Queue.push p worklist) preds.(i)
    end
  done;
  reach, unav

let is_balanced (g : graph) : bool =
  let reach, unav = compute_sets g in
  let balanced = ref true in
  for i = 0 to g.num_states - 1 do
    if not (RoleSet.equal reach.(i) unav.(i)) then balanced := false
  done;
  !balanced 