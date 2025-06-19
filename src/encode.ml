open Ast

(* ------------------------------------------------------------------ *)
(* 1.  A private generator – reset for every call to [encode]          *)
(* ------------------------------------------------------------------ *)

let make_fresh () =
  let c = ref 0 in
  fun () ->                       (* returns 0,1,2,… every time it's called *)
    let v = !c in incr c ; v

(* ------------------------------------------------------------------ *)
(* 2.  Auxiliary operations                                           *)
(* ------------------------------------------------------------------ *)

let rec subst (old_id : int) (new_id : int) (g : int global) : int global =
  match g with
  | GEnd _                                   -> g
  | GVar (v,l) when v = old_id               -> GVar (new_id,l)
  | GVar _                                   -> g
  | GRec (v,body,l) when v = old_id          -> GRec (v,body,l)         (* shadowing *)
  | GRec (v,body,l)                          -> GRec (v, subst old_id new_id body, l)
  | GMsg (p,q,b,cont,l)                      -> GMsg (p,q,b, subst old_id new_id cont, l)
  | GBra (p,q,bs,l) ->
      GBra (p,q, List.map (fun (lbl,gi)-> lbl, subst old_id new_id gi) bs, l)
  | GPar (g1,g2,l)                           -> GPar (subst old_id new_id g1,
                                                       subst old_id new_id g2, l)

let ensure_rec fresh g =
  match g with
  | GRec _ -> g
  | _      -> let id = fresh () in GRec (id, g, loc_of_global g)

let rec fuse id body =
  match body with
  | GRec (id2, inner, _) -> fuse id (subst id2 id inner)
  | other                -> other

(* ------------------------------------------------------------------ *)
(* 3.  First pass – normalise                                         *)
(* ------------------------------------------------------------------ *)

let normalise (fresh : unit -> int) (g : string global) : int global =
  let rec norm env g =
    match g with
    | GEnd _ as e -> Obj.magic e                      (* only the location matters *)
    | GVar (x,l)  ->
        (try GVar (List.assoc x env, l)
         with Not_found -> failwith ("encode: unbound variable "^x))
    | GRec (x,body,l) ->
        let id     = fresh () in
        let body'  = norm ((x,id)::env) body in
        let body'' = fuse id body' in
        GRec (id, body'', l)
    | GMsg (p,q,b,cont,l) ->
        GMsg (p,q,b, ensure_rec fresh (norm env cont), l)
    | GBra (p,q,bs,l) ->
        let bs' = List.map (fun (lbl,gi)-> lbl, ensure_rec fresh (norm env gi)) bs in
        GBra (p,q,bs',l)
    | GPar (g1,g2,l) ->
        let g1' =
          match g1 with
          | GRec _ -> norm env g1
          | _ -> let id = fresh () in GRec (id, norm env g1, Loc.dummy)
        in
        let g2' =
          match g2 with
          | GRec _ -> norm env g2
          | _ -> let id = fresh () in GRec (id, norm env g2, Loc.dummy)
        in
        GPar (g1', g2', l)
  in
  ensure_rec fresh (norm [] g)       (* make sure the whole tree is under one GRec *)

(* ------------------------------------------------------------------ *)
(* 4.  Second pass – *renumber* so the set of ids is {0,…,n-1}        *)
(* ------------------------------------------------------------------ *)

module IS  = Set.Make(Int)
module IM  = Map.Make(Int)

let renumber (g : int global) : int global =
  (* collect every binder/occurrence *)
  let rec collect acc = function
    | GEnd _                    -> acc
    | GVar (v,_)                -> IS.add v acc
    | GRec (v,body,_)           -> collect (IS.add v acc) body
    | GMsg (_,_,_,cont,_)       -> collect acc cont
    | GBra (_,_,brs,_)          -> List.fold_left (fun s (_,gi)-> collect s gi) acc brs
    | GPar (g1,g2,_)            -> collect (collect acc g1) g2
  in
  let ids = collect IS.empty g in

  (* build  old-id ↦ new-id  map, assigning 0,1,2,… in ascending order *)
  let mapping, _ =
    IS.fold
      (fun old (m,next) -> IM.add old next m, next+1)
      ids
      (IM.empty, 0)
  in

  (* apply the mapping everywhere *)
  let rec apply = function
    | GEnd _ as e                 -> e
    | GVar (v,l)                  -> GVar (IM.find v mapping, l)
    | GRec (v,body,l)             -> GRec (IM.find v mapping, apply body, l)
    | GMsg (p,q,b,cont,l)         -> GMsg (p,q,b, apply cont, l)
    | GBra (p,q,brs,l)            -> GBra (p,q,List.map (fun (lbl,gi)-> lbl, apply gi) brs, l)
    | GPar (g1,g2,l)              -> GPar (apply g1, apply g2, l)
  in
  apply g

(* ------------------------------------------------------------------ *)
(* 5.  Public entry point                                             *)
(* ------------------------------------------------------------------ *)

let encode (g : string global) : int global =
  let fresh = make_fresh () in
  g |> normalise fresh |> renumber
