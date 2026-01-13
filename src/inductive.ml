open Ast

(* Merge mode: plain (equality-based) or full (sophisticated merging) *)
type merge_mode = Plain | Full

(* Alpha-conversion for local types *)
module VarMap = Map.Make(struct type t = int let compare = compare end)

(* Alpha-convert a local type to use fresh variables *)
let rec alpha_convert_local (var_map : int VarMap.t) (fresh_counter : int ref) (lt : int local) : int local =
  match lt with
  | LEnd loc -> LEnd loc
  | LVar (v, loc) -> 
      begin try
        let fresh_v = VarMap.find v var_map in
        LVar (fresh_v, loc)
      with Not_found -> LVar (v, loc)  (* Unbound variable, keep as-is *)
      end
  | LRec (v, body, loc) ->
      let fresh_v = !fresh_counter in
      incr fresh_counter;
      let new_map = VarMap.add v fresh_v var_map in
      let alpha_body = alpha_convert_local new_map fresh_counter body in
      LRec (fresh_v, alpha_body, loc)
  | LInt (role, branches, loc) ->
      let alpha_branches = List.map (fun (lbl, lt) -> 
        (lbl, alpha_convert_local var_map fresh_counter lt)) branches in
      LInt (role, alpha_branches, loc)
  | LExt (role, branches, loc) ->
      let alpha_branches = List.map (fun (lbl, lt) -> 
        (lbl, alpha_convert_local var_map fresh_counter lt)) branches in
      LExt (role, alpha_branches, loc)
  | LSend (role, base, cont, loc) ->
      LSend (role, base, alpha_convert_local var_map fresh_counter cont, loc)
  | LRecv (role, base, cont, loc) ->
      LRecv (role, base, alpha_convert_local var_map fresh_counter cont, loc)

(* Projection result with detailed error messages *)
type 'v projection_result = ('v local, string) result

(* Project a global type to a local type for a given participant *)
let rec project ?(mode = Full) ?(var_to_string = fun _ -> "<var>") (global_type : 'v global) (participant : role) : 'v projection_result =
  match global_type with
  
  (* End projects to end *)
  | GEnd loc -> 
      Ok (LEnd loc)
  
  (* Variable projects to variable *)
  | GVar (v, loc) -> 
      Ok (LVar (v, loc))
  
  (* Recursion projects to recursion with projected body *)
  | GRec (v, body, loc) -> 
      begin match project ~mode ~var_to_string body participant with
      | Ok projected_body -> Ok (LRec (v, projected_body, loc))
      | Error msg -> Error ("In recursion " ^ var_to_string v ^ ": " ^ msg)
      end
  
  (* Message passing: sender sends, receiver receives, others continue *)
  | GMsg (sender, receiver, base, continuation, loc) ->
      if participant = sender then
        (* Participant is the sender *)
        begin match project ~mode ~var_to_string continuation participant with
        | Ok projected_cont -> Ok (LSend (receiver, base, projected_cont, loc))
        | Error msg -> Error ("In message " ^ sender ^ " -> " ^ receiver ^ " [" ^ base ^ "]: " ^ msg)
        end
      else if participant = receiver then
        (* Participant is the receiver *)
        begin match project ~mode ~var_to_string continuation participant with
        | Ok projected_cont -> Ok (LRecv (sender, base, projected_cont, loc))
        | Error msg -> Error ("In message " ^ sender ^ " -> " ^ receiver ^ " [" ^ base ^ "]: " ^ msg)
        end
      else
        (* Participant is neither sender nor receiver, just project continuation *)
        project ~mode ~var_to_string continuation participant
  
  (* Branching: sender has internal choice, receiver has external choice, others continue *)
  | GBra (sender, receiver, branches, loc) ->
      if participant = sender then
        (* Participant is the sender - internal choice *)
        let project_branch (label, global_cont) =
          match project ~mode ~var_to_string global_cont participant with
          | Ok local_cont -> Ok (label, local_cont)
          | Error msg -> Error ("In branch " ^ label ^ ": " ^ msg)
        in
        let projected_branches = List.map project_branch branches in
        let failed_branches = List.filter_map (function 
          | Error msg -> Some msg 
          | Ok _ -> None) projected_branches in
        if failed_branches <> [] then
          Error ("Internal choice projection failed for " ^ sender ^ " -> " ^ receiver ^ ": " ^ String.concat "; " failed_branches)
        else
          let valid_branches = List.map (function Ok x -> x | Error _ -> assert false) projected_branches in
          Ok (LInt (receiver, valid_branches, loc))
      else if participant = receiver then
        (* Participant is the receiver - external choice *)
        let project_branch (label, global_cont) =
          match project ~mode ~var_to_string global_cont participant with
          | Ok local_cont -> Ok (label, local_cont)
          | Error msg -> Error ("In branch " ^ label ^ ": " ^ msg)
        in
        let projected_branches = List.map project_branch branches in
        let failed_branches = List.filter_map (function 
          | Error msg -> Some msg 
          | Ok _ -> None) projected_branches in
        if failed_branches <> [] then
          Error ("External choice projection failed for " ^ sender ^ " -> " ^ receiver ^ ": " ^ String.concat "; " failed_branches)
        else
          let valid_branches = List.map (function Ok x -> x | Error _ -> assert false) projected_branches in
          Ok (LExt (sender, valid_branches, loc))
      else
        (* Participant is neither sender nor receiver *)
        (* Project each branch - if any fails, the whole thing fails *)
        let project_branch (label, global_cont) = 
          match project ~mode ~var_to_string global_cont participant with
          | Ok local_cont -> Ok (label, local_cont)
          | Error msg -> Error ("In branch " ^ label ^ ": " ^ msg)
        in
        let projected_branches = List.map project_branch branches in
        let failed_branches = List.filter_map (function 
          | Error msg -> Some msg 
          | Ok _ -> None) projected_branches in
        if failed_branches <> [] then
          Error ("Observer projection failed for " ^ participant ^ " in branching " ^ sender ^ " -> " ^ receiver ^ ": " ^ String.concat "; " failed_branches)
        else
          let valid_branches = List.map (function Ok x -> x | Error _ -> assert false) projected_branches in
          merge_projected_branches mode var_to_string valid_branches loc
  
  (* Parallel composition - participants are disjoint on either side *)
  | GPar (left, right, _) ->
      let left_proj = project ~mode ~var_to_string left participant in
      let right_proj = project ~mode ~var_to_string right participant in
      match left_proj, right_proj with
      | Ok l, Error _ -> Ok l      (* Participant only in left branch *)
      | Error _, Ok r -> Ok r      (* Participant only in right branch *)
      | Error _, Error _ -> Error ("Participant " ^ participant ^ " not found in parallel composition")
      | Ok _, Ok _ -> 
          (* This should not happen if participants are truly disjoint *)
          Error ("Participant " ^ participant ^ " found in both branches of parallel composition (violates disjoint assumption)")

(* Merge projected branches from a global branching construct *)
and merge_projected_branches (mode : merge_mode) (var_to_string : 'v -> string) (branches : (label * 'v local) list) (loc : Loc.t) : 'v projection_result =
  match branches with
  | [] -> Error "No branches to merge"
  | [(_, single)] -> Ok single
  | _ ->
      match mode with
      | Plain ->
          (* Plain mode: all branches must be identical *)
          let local_types = List.map snd branches in
          let first = List.hd local_types in
          if List.for_all (fun lt -> lt = first) local_types then
            Ok first
          else
            Error ("Plain merge failed: branches have different types")
      | Full ->
          (* Full mode: try to merge all branches recursively *)
          let local_types = List.map snd branches in
          merge_local_types_list mode var_to_string local_types loc

(* Merge a list of local types *)
and merge_local_types_list (mode : merge_mode) (var_to_string : 'v -> string) (types : 'v local list) (loc : Loc.t) : 'v projection_result =
  match types with
  | [] -> Error "No types to merge"
  | [single] -> Ok single
  | first :: rest ->
      List.fold_left (fun acc lt ->
        match acc with
        | Error msg -> Error msg
        | Ok merged -> merge_local_types mode var_to_string merged lt loc
      ) (Ok first) rest

(* Merge two local types recursively *)
and merge_local_types (mode : merge_mode) (var_to_string : 'v -> string) (lt1 : 'v local) (lt2 : 'v local) (loc : Loc.t) : 'v projection_result =
  match lt1, lt2 with
  (* End types - only merge with end *)
  | LEnd _, LEnd _ -> Ok lt1
  | LEnd _, _ | _, LEnd _ -> Error "Cannot merge end with non-end type"
  
  (* Variable types - must be identical *)
  | LVar (v1, _), LVar (v2, _) when v1 = v2 -> Ok lt1
  | LVar (v1, _), LVar (v2, _) -> Error ("Cannot merge different variables: " ^ var_to_string v1 ^ " and " ^ var_to_string v2)
  
  (* Recursion - merge bodies with same variable, or alpha-convert for compatibility *)
  | LRec (v1, body1, _), LRec (v2, body2, _) when v1 = v2 ->
      begin match merge_local_types mode var_to_string body1 body2 loc with
      | Ok merged_body -> Ok (LRec (v1, merged_body, loc))
      | Error msg -> Error ("In recursion " ^ var_to_string v1 ^ ": " ^ msg)
      end
  | LRec (v1, body1, _), LRec (v2, body2, _) ->
      (* Try alpha-conversion normalization for int variables in Full mode *)
      begin match mode with
      | Plain -> Error ("Cannot merge recursions with different variables: " ^ var_to_string v1 ^ " and " ^ var_to_string v2)
      | Full -> 
          (* Attempt alpha-conversion merge for int variables *)
          try_alpha_merge_recursions mode var_to_string v1 body1 v2 body2 loc
      end
  
  (* External choice - combine branches *)
  | LExt (role1, branches1, _), LExt (role2, branches2, _) when role1 = role2 ->
      begin match combine_external_branches mode var_to_string branches1 branches2 loc with
      | Ok combined_branches -> Ok (LExt (role1, combined_branches, loc))
      | Error msg -> Error ("In external choice for " ^ role1 ^ ": " ^ msg)
      end
  | LExt (role1, _, _), LExt (role2, _, _) -> Error ("Cannot merge external choices with different roles: " ^ role1 ^ " and " ^ role2)
  
  (* Internal choice - expect identical labels and merge continuations *)
  | LInt (role1, branches1, _), LInt (role2, branches2, _) when role1 = role2 ->
      merge_internal_branches mode var_to_string role1 branches1 branches2 loc
  | LInt (role1, _, _), LInt (role2, _, _) -> Error ("Cannot merge internal choices with different roles: " ^ role1 ^ " and " ^ role2)
  
  (* Send/Receive - must be identical *)
  | LSend (role1, base1, cont1, _), LSend (role2, base2, cont2, _) 
    when role1 = role2 && base1 = base2 ->
      begin match merge_local_types mode var_to_string cont1 cont2 loc with
      | Ok merged_cont -> Ok (LSend (role1, base1, merged_cont, loc))
      | Error msg -> Error ("In send " ^ role1 ^ "![" ^ base1 ^ "]: " ^ msg)
      end
  | LSend (role1, _, _, _), LSend (role2, _, _, _) when role1 <> role2 ->
      Error ("Cannot merge sends with different roles: " ^ role1 ^ " and " ^ role2)
  | LSend (_, base1, _, _), LSend (_, base2, _, _) ->
      Error ("Cannot merge sends with different base types: " ^ base1 ^ " and " ^ base2)
  
  | LRecv (role1, base1, cont1, _), LRecv (role2, base2, cont2, _) 
    when role1 = role2 && base1 = base2 ->
      begin match merge_local_types mode var_to_string cont1 cont2 loc with
      | Ok merged_cont -> Ok (LRecv (role1, base1, merged_cont, loc))
      | Error msg -> Error ("In receive " ^ role1 ^ "?[" ^ base1 ^ "]: " ^ msg)
      end
  | LRecv (role1, _, _, _), LRecv (role2, _, _, _) when role1 <> role2 ->
      Error ("Cannot merge receives with different roles: " ^ role1 ^ " and " ^ role2)
  | LRecv (_, base1, _, _), LRecv (_, base2, _, _) ->
      Error ("Cannot merge receives with different base types: " ^ base1 ^ " and " ^ base2)
  
  (* Different types cannot be merged *)
  | _ -> Error ("Cannot merge incompatible local types")

(* Combine branches from external choices - merge continuations for same labels *)
and combine_external_branches (mode : merge_mode)
                              (var_to_string : 'v -> string)
                              (branches1 : (label * 'v local) list) 
                              (branches2 : (label * 'v local) list)
                              (loc : Loc.t) : ((label * 'v local) list, string) result =
  let all_labels = 
    List.sort_uniq String.compare 
      ((List.map fst branches1) @ (List.map fst branches2))
  in
  let find_branch lbl branches = 
    List.find_opt (fun (l, _) -> l = lbl) branches |> Option.map snd
  in
  let rec process_labels acc = function
    | [] -> Ok (List.rev acc)
    | lbl :: rest ->
        match find_branch lbl branches1, find_branch lbl branches2 with
        | Some cont1, Some cont2 ->
            (* Label exists in both - merge continuations *)
            begin match merge_local_types mode var_to_string cont1 cont2 loc with
            | Ok merged_cont -> process_labels ((lbl, merged_cont) :: acc) rest
            | Error msg -> Error ("Failed to merge branch " ^ lbl ^ ": " ^ msg)
            end
        | Some cont, None | None, Some cont ->
            (* Label exists only in one side - keep as-is *)
            process_labels ((lbl, cont) :: acc) rest
        | None, None ->
            (* This shouldn't happen given how we constructed all_labels *)
            Error ("Internal error: label " ^ lbl ^ " disappeared")
  in
  process_labels [] all_labels

(* Merge branches from internal choices - expect same labels, merge continuations *)
and merge_internal_branches (mode : merge_mode)
                           (var_to_string : 'v -> string)
                           (role : role) 
                           (branches1 : (label * 'v local) list)
                           (branches2 : (label * 'v local) list) 
                           (loc : Loc.t) : 'v projection_result =
  (* Check if both have the same labels *)
  let labels1 = List.map fst branches1 |> List.sort String.compare in
  let labels2 = List.map fst branches2 |> List.sort String.compare in
  if labels1 <> labels2 then
    Error ("Internal choice label mismatch: expected [" ^ String.concat "; " labels1 ^ "] but got [" ^ String.concat "; " labels2 ^ "]")
  else
    match mode with
    | Plain ->
        (* Plain mode: branches must be identical (order-independent) *)
        let sorted_branches1 = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) branches1 in
        let sorted_branches2 = List.sort (fun (l1, _) (l2, _) -> String.compare l1 l2) branches2 in
        if sorted_branches1 = sorted_branches2 then
          Ok (LInt (role, sorted_branches1, loc))
        else
          Error ("Internal choice branches are not identical in plain merge mode")
    | Full ->
        (* Full mode: merge continuations for each label *)
        let rec merge_all_branches acc = function
          | [] -> Ok (List.rev acc)
          | lbl :: rest ->
              let find_branch lbl branches = 
                List.find_opt (fun (l, _) -> l = lbl) branches |> Option.map snd
              in
              match find_branch lbl branches1, find_branch lbl branches2 with
              | Some cont1, Some cont2 ->
                  begin match merge_local_types mode var_to_string cont1 cont2 loc with
                  | Ok merged -> merge_all_branches ((lbl, merged) :: acc) rest
                  | Error msg -> Error ("Failed to merge internal choice branch " ^ lbl ^ ": " ^ msg)
                  end
              | _ -> Error ("Internal error: label " ^ lbl ^ " missing from branches")
        in
        match merge_all_branches [] labels1 with
        | Ok merged_branches -> Ok (LInt (role, merged_branches, loc))
        | Error msg -> Error msg

(* Attempt alpha-conversion merge for recursions with different variables *)
and try_alpha_merge_recursions mode var_to_string v1 body1 v2 body2 loc =
  (* This works specifically for int variables from synthesis *)
  try
    (* Cast to int local types for alpha conversion *)
    let int_body1 = (Obj.magic body1 : int local) in
    let int_body2 = (Obj.magic body2 : int local) in
    
    (* Create a fresh variable counter starting from a high number to avoid conflicts *)
    let fresh_counter = ref 10000 in
    let fresh_var = !fresh_counter in
    incr fresh_counter;
    
    (* Alpha-convert both bodies to use the fresh variable *)
    let var_map1 = VarMap.add (Obj.magic v1 : int) fresh_var VarMap.empty in
    let var_map2 = VarMap.add (Obj.magic v2 : int) fresh_var VarMap.empty in
    
    let alpha_body1 = alpha_convert_local var_map1 fresh_counter int_body1 in
    let alpha_body2 = alpha_convert_local var_map2 fresh_counter int_body2 in
    
    (* Try to merge the alpha-converted bodies *)
    match merge_local_types mode var_to_string (Obj.magic alpha_body1) (Obj.magic alpha_body2) loc with
    | Ok merged_body -> Ok (LRec ((Obj.magic fresh_var), merged_body, loc))
    | Error msg -> Error ("Alpha-conversion merge failed: " ^ msg)
  with
  | _ -> Error ("Cannot alpha-convert merge recursions with different variables: " ^ var_to_string v1 ^ " and " ^ var_to_string v2)
