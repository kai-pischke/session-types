open Ast
(* open Loc *)

module IntMap = Map.Make(Int)

(* Alpha-equivalence for global types *)
let global eq_var g1 g2 =
  let norm1 = Normalise.normalise_parallel ~pp_var:(fun _ _ -> ()) g1 in
  let norm2 = Normalise.normalise_parallel ~pp_var:(fun _ _ -> ()) g2 in
  let rec go env1 env2 g1 g2 =
    match g1, g2 with
    | GEnd _, GEnd _ -> true
    | GVar (v1, _), GVar (v2, _) ->
        (try IntMap.find v1 env1 = IntMap.find v2 env2 with Not_found -> eq_var v1 v2)
    | GRec (v1, body1, _), GRec (v2, body2, _) ->
        let fresh = IntMap.cardinal env1 in
        let env1' = IntMap.add v1 fresh env1 in
        let env2' = IntMap.add v2 fresh env2 in
        go env1' env2' body1 body2
    | GMsg (p1,q1,b1,cont1,_), GMsg (p2,q2,b2,cont2,_) ->
        p1 = p2 && q1 = q2 && b1 = b2 && go env1 env2 cont1 cont2
    | GBra (p1,q1,brs1,_), GBra (p2,q2,brs2,_) ->
        p1 = p2 && q1 = q2 &&
        let sort = List.sort (fun (l1,_) (l2,_) -> compare l1 l2) in
        let brs1 = sort brs1 and brs2 = sort brs2 in
        List.length brs1 = List.length brs2 &&
        List.for_all2 (fun (l1,g1) (l2,g2) -> l1 = l2 && go env1 env2 g1 g2) brs1 brs2
    | GPar (a1,b1,_), GPar (a2,b2,_) ->
        go env1 env2 a1 a2 && go env1 env2 b1 b2
    | _ -> false
  in
  go IntMap.empty IntMap.empty norm1 norm2 

(* ------------------------------------------------------------------ *)
(* Alpha-equivalence for local types (no parallel)                     *)
(* ------------------------------------------------------------------ *)

let local eq_var l1 l2 =
  let rec go env1 env2 l1 l2 =
    match l1, l2 with
    | LEnd _, LEnd _ -> true
    | LVar (v1,_), LVar (v2,_) ->
        (try IntMap.find v1 env1 = IntMap.find v2 env2 with Not_found -> eq_var v1 v2)
    | LRec (v1, body1, _), LRec (v2, body2, _) ->
        let fresh = IntMap.cardinal env1 in
        let env1' = IntMap.add v1 fresh env1 in
        let env2' = IntMap.add v2 fresh env2 in
        go env1' env2' body1 body2
    | LSend (r1,b1,c1,_), LSend (r2,b2,c2,_) ->
        r1=r2 && b1=b2 && go env1 env2 c1 c2
    | LRecv (r1,b1,c1,_), LRecv (r2,b2,c2,_) ->
        r1=r2 && b1=b2 && go env1 env2 c1 c2
    | LInt (r1, brs1,_), LInt (r2, brs2,_)
    | LExt (r1, brs1,_), LExt (r2, brs2,_) ->
        r1=r2 &&
        let sort = List.sort (fun (l,_) (l',_) -> compare l l') in
        let brs1 = sort brs1 and brs2 = sort brs2 in
        List.length brs1 = List.length brs2 &&
        List.for_all2 (fun (lbl1,g1) (lbl2,g2) -> lbl1=lbl2 && go env1 env2 g1 g2) brs1 brs2
    | _ -> false
  in
  go IntMap.empty IntMap.empty l1 l2 