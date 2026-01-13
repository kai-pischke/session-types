(*--------------------------------------------------------------------*)
(*  Size Calculation for Session Types                                 *)
(*--------------------------------------------------------------------*)
(*
  This module implements recursive size calculation for global and local 
  session types based on their syntactic structure.
  
  SIZE DEFINITION:
  - size(end) = 1
  - size(t) = 1 (for variable references)  
  - size(G) = 1 + Σ size(G') for all immediate subterms G' of G
  
  The size reflects the structural complexity of session types, useful for
  complexity analysis and termination proofs.
*)

open Ast

(*--------------------------------------------------------------------*)
(*  Size Calculation for Global Session Types                        *)
(*--------------------------------------------------------------------*)

(**
  size_global: 'v global -> int
  
  Pre: global is a well-formed global session type
  Post: Returns the syntactic size of the global type
        size >= 1 for any well-formed type
*)
let rec size_global (g : 'v global) : int =
  match g with
  
  | GEnd _ ->
      (* Base case: end has size 1 *)
      1
  
  | GVar (_, _) ->
      (* Base case: variable reference has size 1 *)
      1
  
  | GRec (_, body, _) ->
      (* Recursive type: 1 + size of body *)
      1 + size_global body
  
  | GMsg (_, _, _, continuation, _) ->
      (* Message passing: 1 + size of continuation *)
      1 + size_global continuation
  
  | GBra (_, _, branches, _) ->
      (* Branching: 1 + sum of sizes of all branch bodies *)
      let branch_sizes = List.map (fun (_, branch_body) -> size_global branch_body) branches in
      1 + List.fold_left (+) 0 branch_sizes
  
  | GPar (left, right, _) ->
      (* Parallel composition: 1 + size of left + size of right *)
      1 + size_global left + size_global right

(*--------------------------------------------------------------------*)
(*  Size Calculation for Local Session Types                         *)
(*--------------------------------------------------------------------*)

(**
  size_local: 'v local -> int
  
  Pre: local is a well-formed local session type
  Post: Returns the syntactic size of the local type
        size >= 1 for any well-formed type
*)
let rec size_local (l : 'v local) : int =
  match l with
  
  | LEnd _ ->
      (* Base case: end has size 1 *)
      1
  
  | LVar (_, _) ->
      (* Base case: variable reference has size 1 *)
      1
  
  | LRec (_, body, _) ->
      (* Recursive type: 1 + size of body *)
      1 + size_local body
  
  | LRecv (_, _, continuation, _) ->
      (* Receive: 1 + size of continuation *)
      1 + size_local continuation
  
  | LSend (_, _, continuation, _) ->
      (* Send: 1 + size of continuation *)
      1 + size_local continuation
  
  | LInt (_, branches, _) ->
      (* Internal choice: 1 + sum of sizes of all branch bodies *)
      let branch_sizes = List.map (fun (_, branch_body) -> size_local branch_body) branches in
      1 + List.fold_left (+) 0 branch_sizes
  
  | LExt (_, branches, _) ->
      (* External choice: 1 + sum of sizes of all branch bodies *)
      let branch_sizes = List.map (fun (_, branch_body) -> size_local branch_body) branches in
      1 + List.fold_left (+) 0 branch_sizes

(*--------------------------------------------------------------------*)
(*  Utility Functions                                                 *)
(*--------------------------------------------------------------------*)

(**
  size_global_list: 'v global list -> int list
  
  Pre: globals is a list of well-formed global session types
  Post: Returns list of sizes corresponding to each global type
*)
let size_global_list (globals : 'v global list) : int list =
  List.map size_global globals

(**
  size_local_list: 'v local list -> int list
  
  Pre: locals is a list of well-formed local session types  
  Post: Returns list of sizes corresponding to each local type
*)
let size_local_list (locals : 'v local list) : int list =
  List.map size_local locals

(**
  total_size_global: 'v global list -> int
  
  Pre: globals is a list of well-formed global session types
  Post: Returns the sum of sizes of all global types in the list
*)
let total_size_global (globals : 'v global list) : int =
  List.fold_left (+) 0 (size_global_list globals)

(**
  total_size_local: 'v local list -> int
  
  Pre: locals is a list of well-formed local session types
  Post: Returns the sum of sizes of all local types in the list
*)
let total_size_local (locals : 'v local list) : int =
  List.fold_left (+) 0 (size_local_list locals)

(**
  max_size_global: 'v global list -> int
  
  Pre: globals is a non-empty list of well-formed global session types
  Post: Returns the maximum size among all global types in the list
*)
let max_size_global (globals : 'v global list) : int =
  match globals with
  | [] -> 0
  | _ -> List.fold_left max 0 (size_global_list globals)

(**
  max_size_local: 'v local list -> int
  
  Pre: locals is a non-empty list of well-formed local session types
  Post: Returns the maximum size among all local types in the list
*)
let max_size_local (locals : 'v local list) : int =
  match locals with
  | [] -> 0
  | _ -> List.fold_left max 0 (size_local_list locals)

(*--------------------------------------------------------------------*)
(*  Pretty Printing for Size Information                             *)
(*--------------------------------------------------------------------*)


let string_of_size_info_global (g : 'v global) : string =
  Printf.sprintf "Global type size: %d" (size_global g)


let string_of_size_info_local (l : 'v local) : string =
  Printf.sprintf "Local type size: %d" (size_local l)