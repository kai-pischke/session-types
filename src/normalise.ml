open Ast
open Loc

(* Flatten all parallel compositions in [g] into a list of non-parallel sub-terms. *)
let rec flatten_par g =
  match g with
  | GPar (g1, g2, _) -> flatten_par g1 @ flatten_par g2
  | _ -> [ g ]

(* Build a (right-associated) parallel tree from a list.  If the list is empty we
   return GEnd.  If it has a single element we return just that element. *)
let rec build_par = function
  | [] -> GEnd dummy
  | [ g ] -> g
  | g :: tl -> GPar (g, build_par tl, dummy)

(* Normalise parallel composition in a global type.
   The result is:
   1. All nested GPar nodes are flattened into a single list.
   2. The branches are sorted according to a user-supplied key to obtain a
      canonical ordering.  A convenient key is the pretty-printed string.

   [pp_var] pretty-prints variables (int or string) for determinism when
   computing the ordering.
*)
let normalise_parallel ~pp_var g =
  (* Cache pretty strings for efficiency. *)
  let rec norm g =
    match g with
    | GPar _ ->
        let branches = flatten_par g |> List.map norm in
        let cmp a b =
          String.compare (Pretty.string_of pp_var a) (Pretty.string_of pp_var b)
        in
        let branches_sorted = List.sort cmp branches in
        build_par branches_sorted
    | GRec (v, body, loc) -> GRec (v, norm body, loc)
    | GMsg (p,q,base, cont, loc) -> GMsg (p,q,base, norm cont, loc)
    | GBra (p,q, brs, loc) ->
        let brs' = List.map (fun (lbl, g) -> lbl, norm g) brs in
        GBra (p,q, brs', loc)
    | GEnd _ | GVar _ -> g
  in
  norm g 