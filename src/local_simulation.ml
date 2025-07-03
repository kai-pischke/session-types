(* Local graph simulation check using deterministic HHK-style algorithm *)
open Ast
open Local_automaton

module PairHash = struct
  type t = int * int
  let equal (a1,b1) (a2,b2) = a1 = a2 && b1 = b2
  let hash = Hashtbl.hash
end

module PairTbl = Hashtbl.Make (PairHash)

(* Action labels on transitions that must be preserved by simulation *)
type action =
  | ASend of base
  | ARecv of base
  | AInt  of label            (* internal choice label *)
  | AExt  of label            (* external choice label *)

(* Build successor list for every state in the graph *)
let successors (g : graph) : (action * int option) list array =
  Array.init g.num_states (fun st ->
      match g.kinds.(st) with
      | Snd (b, dst)      -> [ (ASend b, dst) ]
      | Rcv (b, dst)      -> [ (ARecv b, dst) ]
      | Int branches      -> List.map (fun (lbl,d) -> (AInt lbl , d)) branches
      | Ext branches      -> List.map (fun (lbl,d) -> (AExt lbl , d)) branches)

(* Predecessor list (needed for fast back-prop on kills) *)
let predecessors (succs : (action * int option) list array)
    : ((int * action) list) array =
  let preds = Array.make (Array.length succs) [] in
  Array.iteri (fun src outs ->
      List.iter (fun (act, dst_opt) ->
          match dst_opt with
          | None -> ()
          | Some dst -> preds.(dst) <- (src, act) :: preds.(dst)) outs) succs;
  preds

(* Determine if a simulation exists from g1 (specification) to g2 (implementation). *)
let simulation_exists (g1 : graph) (g2 : graph) : bool =
  let n1 = g1.num_states in
  let n2 = g2.num_states in
  if n1 = 0 || n2 = 0 then false
  else
    let succ1 = successors g1 in
    let succ2 = successors g2 in

    (* alive matrix and killed queue *)
    let alive = Array.init n1 (fun _ -> Array.make n2 true) in
    let killed = Queue.create () in

    let kill (i,j) =
      if alive.(i).(j) then (
        alive.(i).(j) <- false;
        Queue.add (i,j) killed)
    in

    (* Reverse-dependency table: (child_pair) -> parents that picked it as witness *)
    let rev : ((int * int) list ref) PairTbl.t = PairTbl.create 1024 in  (* maps child pair -> parents list ref *)
    let get_list key =
      match PairTbl.find_opt rev key with
      | Some r -> r
      | None ->
          let r = ref [] in
          PairTbl.add rev key r; r
    in

    let add_rev child parent =
      let lst = get_list child in
      lst := parent :: !lst
    in

    (* Check a pair; (re)establish witnesses; may kill the pair *)
    let establish_pair (p,q) =
      if not alive.(p).(q) then ()
      else (
        match g1.kinds.(p), g2.kinds.(q) with
        | Int spec_brs, Int impl_brs ->
            (* Internal choice is contravariant: impl labels ⊆ spec labels *)
            List.iter (fun (lbl, qdst_opt) ->
              match List.find_opt (fun (l2, _) -> l2 = lbl) spec_brs with
              | None -> kill (p,q)
              | Some (_, pdst_opt) ->
                  (match pdst_opt, qdst_opt with
                   | Some pdst, Some qdst -> add_rev (pdst,qdst) (p,q)
                   | _ -> ())
            ) impl_brs

        | Ext spec_brs, Ext impl_brs ->
            (* External choice is covariant: spec labels ⊆ impl labels *)
            List.iter (fun (lbl, pdst_opt) ->
              match List.find_opt (fun (l2, _) -> l2 = lbl) impl_brs with
              | None -> kill (p,q)
              | Some (_, qdst_opt) ->
                  (match pdst_opt, qdst_opt with
                   | Some pdst, Some qdst -> add_rev (pdst,qdst) (p,q)
                   | _ -> ())
            ) spec_brs

        | _ ->
            (* Other kinds: each transition from spec must be simulated *)
            let rec loop transitions =
              match transitions with
              | [] -> ()
              | (act, pdst_opt) :: tl ->
                  let witness =
                    List.find_opt (fun (a2, qdst_opt) ->
                        a2 = act &&
                        match pdst_opt, qdst_opt with
                        | None, None -> true
                        | Some pdst, Some qdst -> alive.(pdst).(qdst)
                        | _ -> false) succ2.(q)
                  in
                  (match witness with
                   | None -> kill (p,q)
                   | Some (_, qdst_opt) ->
                       (match pdst_opt, qdst_opt with
                        | Some pdst, Some qdst -> add_rev (pdst,qdst) (p,q)
                        | _ -> ());
                       if alive.(p).(q) then loop tl)
            in
            loop succ1.(p)
        );

      (* Contravariant rule for external choice: every ext transition offered by
         implementation must exist in specification. *)
      if alive.(p).(q) then (
        match g1.kinds.(p), g2.kinds.(q) with
        | Ext _, Ext _ ->
            ()
        | _ -> () );

      (* Extra branches in implementation external choice not in specification *)
      if alive.(p).(q) then (
        match g1.kinds.(p), g2.kinds.(q) with
        | Ext spec_brs, Ext impl_brs ->
            List.iter (fun (lbl, _) ->
              if not (List.exists (fun (l2, _) -> l2 = lbl) spec_brs) then
                kill (p,q)) impl_brs
        | _ -> ()
      );
        ()
    in

    (* Initial filtering: incompatible roles cannot be in simulation *)
    for i=0 to n1-1 do
      for j=0 to n2-1 do
        if g1.roles.(i) <> g2.roles.(j) then kill (i,j)
      done
    done;

    (* After role filter, compute witnesses for remaining pairs *)
    for i=0 to n1-1 do
      for j=0 to n2-1 do establish_pair (i,j) done
    done;

    (* Propagate kills *)
    while not (Queue.is_empty killed) do
      let child = Queue.take killed in
      match PairTbl.find_opt rev child with
      | None -> ()
      | Some parents_ref ->
          List.iter (fun parent -> establish_pair parent) !parents_ref;
          PairTbl.remove rev child
    done;

    (* Decide based on designated start states *)
    match g1.start_state, g2.start_state with
    | Some s1, Some s2 -> alive.(s1).(s2)
    | _ -> false 