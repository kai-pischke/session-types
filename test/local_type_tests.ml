(*--------------------------------------------------------------------*)
(*  Unit‑tests for local type parsing                                  *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let local_type_tests =
  let open Ast in
  [
    test_case "internal choice" `Quick (fun () ->
      let l = parse_local "p ! {L1: end, L2: end}" in
      match l with
      | LInt ("p", [("L1", LEnd _); ("L2", LEnd _)], _) -> ()
      | _ -> fail "Did not parse internal choice"
    );

    test_case "external choice" `Quick (fun () ->
      let l = parse_local "p ? {L1: end, L2: end}" in
      match l with
      | LExt ("p", [("L1", LEnd _); ("L2", LEnd _)], _) -> ()
      | _ -> fail "Did not parse external choice"
    );

    test_case "send message" `Quick (fun () ->
      let l = parse_local "q ! [Int]; end" in
      match l with
      | LSend ("q", "Int", LEnd _, _) -> ()
      | _ -> fail "Did not parse send message"
    );

    test_case "receive message" `Quick (fun () ->
      let l = parse_local "q ? [String]; end" in
      match l with
      | LRecv ("q", "String", LEnd _, _) -> ()
      | _ -> fail "Did not parse receive message"
    );

    test_case "send with continuation" `Quick (fun () ->
      let l = parse_local "q ! [Bool]; r ? [Int]; end" in
      match l with
      | LSend ("q", "Bool", LRecv ("r", "Int", LEnd _, _), _) -> ()
      | _ -> fail "Did not parse send with continuation"
    );

    test_case "receive with continuation" `Quick (fun () ->
      let l = parse_local "q ? [Float]; r ! [Char]; end" in
      match l with
      | LRecv ("q", "Float", LSend ("r", "Char", LEnd _, _), _) -> ()
      | _ -> fail "Did not parse receive with continuation"
    );

    test_case "choice with messages" `Quick (fun () ->
      let l = parse_local "p ! {Ok: q ! [Int]; end, Err: q ? [String]; end}" in
      match l with
      | LInt ("p", [("Ok", LSend ("q", "Int", LEnd _, _)); ("Err", LRecv ("q", "String", LEnd _, _))], _) -> ()
      | _ -> fail "Did not parse choice with messages"
    );

    test_case "recursion with messages" `Quick (fun () ->
      let l = parse_local "rec t. q ! [Int]; t" in
      match l with
      | LRec (var_id, LSend ("q", "Int", LVar (var_id2, _), _), _) when var_id = var_id2 -> ()
      | _ -> fail "Did not parse recursion with messages"
    );
  ]

let suite = ("local-types", local_type_tests) 