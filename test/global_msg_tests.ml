(*--------------------------------------------------------------------*)
(*  Unit‑tests for global message parsing                              *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let global_msg_test =
  test_case "explicit message" `Quick (fun () ->
    let g = parse_global "a -> b : [Int]; end" in
    match g with
    | Ast.GMsg ("a", "b", "Int", Ast.GEnd _, _) -> ()
    | _ -> fail "Did not parse explicit message"
  )

let suite = ("global-msg", [global_msg_test]) 