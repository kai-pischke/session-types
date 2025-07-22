(*--------------------------------------------------------------------*)
(*  Unit‑tests for the encode module                                   *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let encode_tests =
  [
    test_case "no rec for end" `Quick (fun () ->
      let g = parse_global "end" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GEnd _ -> ()
      | _ -> fail "Expected just end"
    );

    test_case "rec is preserved if used" `Quick (fun () ->
      let g = parse_global "rec t. a -> b { Ok: t }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _))], _), _) -> ()
      | _ -> fail "Expected rec to be preserved and variable renamed"
    );

    test_case "unused rec is removed" `Quick (fun () ->
      let g = parse_global "rec t. end" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GEnd _ -> ()
      | _ -> fail "Unused rec should be removed"
    );

    test_case "adjacent recs are preserved if used" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t, Err: u }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GRec (1, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _)); ("Err", Ast.GVar (1, _))], _), _), _) -> ()
      | _ -> fail "Both recs should be preserved and renamed"
    );

    test_case "inner unused rec is removed" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t }" in
      let g' = Encode.encode g in
      match g' with
      | Ast.GRec (0, Ast.GBra ("a", "b", [("Ok", Ast.GVar (0, _))], _), _) -> ()
      | _ -> fail "Inner unused rec should be removed"
    );

    test_case "variable renaming is consecutive" `Quick (fun () ->
      let g = parse_global "rec t. rec u. a -> b { Ok: t, Err: u }" in
      let g' = Encode.encode g in
      let rec collect acc = function
        | Ast.GEnd _ -> acc
        | Ast.GVar (v, _) -> v :: acc
        | Ast.GRec (v, body, _) -> v :: collect acc body
        | Ast.GMsg (_, _, _, cont, _) -> collect acc cont
        | Ast.GBra (_, _, bs, _) -> List.fold_left (fun a (_, g) -> collect a g) acc bs
        | Ast.GPar (g1, g2, _) -> collect (collect acc g1) g2
      in
      let ids = List.sort_uniq compare (collect [] g') in
      if ids <> [0;1] then fail "Variables should be 0 and 1"
    );
  ]

let suite = ("encode", encode_tests) 