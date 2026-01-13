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

(* Check if a variable is used in a global type *)
let rec var_used (var : int) (g : int global) : bool =
  match g with
  | GEnd _ -> false
  | GVar (v, _) -> v = var
  | GRec (v, _, _) when v = var -> false  (* shadowing *)
  | GRec (_, body, _) -> var_used var body
  | GMsg (_, _, _, cont, _) -> var_used var cont
  | GBra (_, _, bs, _) -> List.exists (fun (_, g) -> var_used var g) bs
  | GPar (g1, g2, _) -> var_used var g1 || var_used var g2

(* Remove unused GRec nodes *)
let rec remove_unused_rec (g : int global) : int global =
  match g with
  | GRec (v, body, l) ->
      if var_used v body then
        GRec (v, remove_unused_rec body, l)
      else
        remove_unused_rec body
  | GMsg (p, q, b, cont, l) ->
      GMsg (p, q, b, remove_unused_rec cont, l)
  | GBra (p, q, bs, l) ->
      GBra (p, q, List.map (fun (lbl, g) -> (lbl, remove_unused_rec g)) bs, l)
  | GPar (g1, g2, l) ->
      GPar (remove_unused_rec g1, remove_unused_rec g2, l)
  | other -> other

(* ------------------------------------------------------------------ *)
(* 3.  First pass – normalize (only resolve variables, don't add GRec) *)
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
        GRec (id, body', l)
    | GMsg (p,q,b,cont,l) ->
        GMsg (p,q,b, norm env cont, l)
    | GBra (p,q,bs,l) ->
        let bs' = List.map (fun (lbl,gi)-> lbl, norm env gi) bs in
        GBra (p,q,bs',l)
    | GPar (g1,g2,l) ->
        let g1' = norm env g1 in
        let g2' = norm env g2 in
        GPar (g1', g2', l)
  in
  norm [] g

(* ------------------------------------------------------------------ *)
(* 4.  Second pass – remove unused GRec and renumber *)
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
  g |> normalise fresh |> remove_unused_rec |> renumber

(* ------------------------------------------------------------------ *)
(* 6.  Local type encoding                                            *)
(* ------------------------------------------------------------------ *)

let rec subst_local (old_id : int) (new_id : int) (l : int local) : int local =
  match l with
  | LEnd _                                   -> l
  | LVar (v,loc) when v = old_id             -> LVar (new_id,loc)
  | LVar _                                   -> l
  | LRec (v,body,loc) when v = old_id        -> LRec (v,body,loc)         (* shadowing *)
  | LRec (v,body,loc)                        -> LRec (v, subst_local old_id new_id body, loc)
  | LRecv (p,b,cont,loc)                     -> LRecv (p,b, subst_local old_id new_id cont, loc)
  | LSend (p,b,cont,loc)                     -> LSend (p,b, subst_local old_id new_id cont, loc)
  | LInt (p,bs,loc) ->
      LInt (p, List.map (fun (lbl,li)-> lbl, subst_local old_id new_id li) bs, loc)
  | LExt (p,bs,loc) ->
      LExt (p, List.map (fun (lbl,li)-> lbl, subst_local old_id new_id li) bs, loc)

(* Check if a variable is used in a local type *)
let rec var_used_local (var : int) (l : int local) : bool =
  match l with
  | LEnd _ -> false
  | LVar (v, _) -> v = var
  | LRec (v, _, _) when v = var -> false  (* shadowing *)
  | LRec (_, body, _) -> var_used_local var body
  | LRecv (_, _, cont, _) -> var_used_local var cont
  | LSend (_, _, cont, _) -> var_used_local var cont
  | LInt (_, bs, _) -> List.exists (fun (_, l) -> var_used_local var l) bs
  | LExt (_, bs, _) -> List.exists (fun (_, l) -> var_used_local var l) bs

(* Remove unused LRec nodes *)
let rec remove_unused_rec_local (l : int local) : int local =
  match l with
  | LRec (v, body, loc) ->
      if var_used_local v body then
        LRec (v, remove_unused_rec_local body, loc)
      else
        remove_unused_rec_local body
  | LRecv (p, b, cont, loc) ->
      LRecv (p, b, remove_unused_rec_local cont, loc)
  | LSend (p, b, cont, loc) ->
      LSend (p, b, remove_unused_rec_local cont, loc)
  | LInt (p, bs, loc) ->
      LInt (p, List.map (fun (lbl, l) -> (lbl, remove_unused_rec_local l)) bs, loc)
  | LExt (p, bs, loc) ->
      LExt (p, List.map (fun (lbl, l) -> (lbl, remove_unused_rec_local l)) bs, loc)
  | other -> other

let normalise_local (fresh : unit -> int) (l : string local) : int local =
  let rec norm env l =
    match l with
    | LEnd _ as e -> Obj.magic e                      (* only the location matters *)
    | LVar (x,loc)  ->
        (try LVar (List.assoc x env, loc)
         with Not_found -> failwith ("encode_local: unbound variable "^x))
    | LRec (x,body,loc) ->
        let id     = fresh () in
        let body'  = norm ((x,id)::env) body in
        LRec (id, body', loc)
    | LRecv (p,b,cont,loc) ->
        LRecv (p,b, norm env cont, loc)
    | LSend (p,b,cont,loc) ->
        LSend (p,b, norm env cont, loc)
    | LInt (p,bs,loc) ->
        let bs' = List.map (fun (lbl,li)-> lbl, norm env li) bs in
        LInt (p,bs',loc)
    | LExt (p,bs,loc) ->
        let bs' = List.map (fun (lbl,li)-> lbl, norm env li) bs in
        LExt (p,bs',loc)
  in
  norm [] l

let renumber_local (l : int local) : int local =
  (* collect every binder/occurrence *)
  let rec collect acc = function
    | LEnd _                    -> acc
    | LVar (v,_)                -> IS.add v acc
    | LRec (v,body,_)           -> collect (IS.add v acc) body
    | LRecv (_,_,cont,_)        -> collect acc cont
    | LSend (_,_,cont,_)        -> collect acc cont
    | LInt (_,bs,_)             -> List.fold_left (fun s (_,li)-> collect s li) acc bs
    | LExt (_,bs,_)             -> List.fold_left (fun s (_,li)-> collect s li) acc bs
  in
  let ids = collect IS.empty l in

  (* build  old-id ↦ new-id  map, assigning 0,1,2,… in ascending order *)
  let mapping, _ =
    IS.fold
      (fun old (m,next) -> IM.add old next m, next+1)
      ids
      (IM.empty, 0)
  in

  (* apply the mapping everywhere *)
  let rec apply = function
    | LEnd _ as e                 -> e
    | LVar (v,loc)                -> LVar (IM.find v mapping, loc)
    | LRec (v,body,loc)           -> LRec (IM.find v mapping, apply body, loc)
    | LRecv (p,b,cont,loc)        -> LRecv (p,b, apply cont, loc)
    | LSend (p,b,cont,loc)        -> LSend (p,b, apply cont, loc)
    | LInt (p,bs,loc)             -> LInt (p,List.map (fun (lbl,li)-> lbl, apply li) bs, loc)
    | LExt (p,bs,loc)             -> LExt (p,List.map (fun (lbl,li)-> lbl, apply li) bs, loc)
  in
  apply l

let encode_local (l : string local) : int local =
  let fresh = make_fresh () in
  l |> normalise_local fresh |> remove_unused_rec_local |> renumber_local
