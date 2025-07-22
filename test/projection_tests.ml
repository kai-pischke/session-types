(*--------------------------------------------------------------------*)
(*  Unit-tests for the projection module                               *)
(*  We compare the computed local automaton with the automaton         *)
(*  obtained from an *expected* local type written in concrete syntax. *)
(*--------------------------------------------------------------------*)
open Alcotest
open Test_helpers

let projection_tests =
  let open Projection in
  
  (* Build a local automaton from a concrete local type string *)
  let la_of_local (src : string) : Local_automaton.graph =
    src |> parse_local |> Encode.encode_local |> Local_automaton.of_local
  in

  (* Structural equality modulo permutation of Int/Ext branch lists. *)
  let opt_eq eq a b = match a,b with | None,None -> true | Some x,Some y -> eq x y | _ -> false in

  let branch_list_eq l1 l2 =
    let sort = List.sort (fun (lbl1,_) (lbl2,_) -> String.compare lbl1 lbl2) in
    sort l1 = sort l2
  in

  let kind_eq k1 k2 =
    let open Local_automaton in
    match k1,k2 with
    | Snd(b1,d1), Snd(b2,d2) -> String.equal b1 b2 && opt_eq Int.equal d1 d2
    | Rcv(b1,d1), Rcv(b2,d2) -> String.equal b1 b2 && opt_eq Int.equal d1 d2
    | Int br1,  Int br2  -> branch_list_eq br1 br2
    | Ext br1,  Ext br2  -> branch_list_eq br1 br2
    | _ -> false
  in

  let equal_la (g1:Local_automaton.graph) (g2:Local_automaton.graph) : bool =
    g1.num_states = g2.num_states &&
    opt_eq Int.equal g1.start_state g2.start_state &&
    Array.for_all2 (fun r1 r2 -> r1 = r2) g1.roles g2.roles &&
    Array.for_all2 kind_eq g1.kinds g2.kinds
  in

  (* Helper: compute projection and check against expected local type.  *)
  let check_projection ~name ~(global:string) ~(role:string) ~(expected_local:string) =
    test_case name `Quick @@ fun () ->
      (* Global → automaton *)
      let g_ast   = parse_global global |> Encode.encode in
      let g_aut   = Automaton.of_global g_ast in
      (* Compute projection *)
      match project g_aut role with
      | Error msg -> failf "projection failed: %s" msg
      | Ok la_proj ->
          let la_expect = la_of_local expected_local in
          if not (equal_la la_proj la_expect) then
            failf "projection mismatch\nExpected:\n%s\nGot:\n%s"
              (Local_automaton.string_of_graph la_expect)
              (Local_automaton.string_of_graph la_proj)
  in

  (* Helper: check that projection fails for unprojectable types *)
  let check_unprojectable ~name ~(global:string) ~(role:string) =
    test_case name `Quick @@ fun () ->
      (* Global → automaton *)
      let g_ast   = parse_global global |> Encode.encode in
      let g_aut   = Automaton.of_global g_ast in
      (* Compute projection *)
      match project g_aut role with
      | Error _ -> () (* Expected to fail *)
      | Ok _ -> failf "projection should have failed for unprojectable type"
  in

  [ (* 1. Simple message *)
    check_projection
      ~name:"sender proj simple msg"
      ~global:"a -> b : [Int]; end"
      ~role:"a"
      ~expected_local:"b ! [Int]; end";

    check_projection
      ~name:"receiver proj simple msg"
      ~global:"a -> b : [Int]; end"
      ~role:"b"
      ~expected_local:"a ? [Int]; end";

    (* 2. Branching *)
    check_projection
      ~name:"internal choice proj"
      ~global:"a -> b { Ok: end, Err: end }"
      ~role:"a"
      ~expected_local:"b ! {Ok: end, Err: end}";

    check_projection
      ~name:"external choice proj"
      ~global:"a -> b { Ok: end, Err: end }"
      ~role:"b"
      ~expected_local:"a ? {Ok: end, Err: end}";

    (* 3. Absent participant — expect empty automaton *)
    check_projection
      ~name:"absent participant"
      ~global:"a -> b : [Int]; end"
      ~role:"c"
      ~expected_local:"end";
    
    (* 4. With recursion *)
    check_projection
      ~name:"simple recursion"
      ~global:"rec t . a -> b : [Int]; t"
      ~role:"a"
      ~expected_local:"rec t . b ! [Int]; t";
    
    check_projection
      ~name:"dawit-style recursion"
      ~global:"rec t . a -> b : [Int]; rec r . b -> c { X: t, Y: a -> b : [Int]; r }"
      ~role:"a"
      ~expected_local:"b ! [Int]; rec t . b ! [Int]; t";
    
    (* 5. Including parallel composition *)
    check_projection
      ~name:"parallel composition"
      ~global:"a -> b { X: (a -> b : [Int]; end) | (c -> d : [Int]; end), Y: c -> d : [Int]; end }"
      ~role:"c"
      ~expected_local:"d ! [Int]; end";

    (* 6. Unprojectable types *)
    check_unprojectable
      ~name:"unprojectable - inconsistent message types"
      ~global:"a -> b { Ok: c -> d : [Int]; end, Err: c -> d : [Bool]; end }"
      ~role:"c";

    check_unprojectable
      ~name:"unprojectable - mixed send/recv"
      ~global:"a -> b { Ok: c -> d : [Int]; end, Err: d -> c : [Bool]; end }"
      ~role:"c";

    check_unprojectable
      ~name:"unprojectable - inconsistent peer roles"
      ~global:"a -> b { Ok: c -> d : [Int]; end, Err: c -> e : [Bool]; end }"
      ~role:"c";
  ]

let suite = ("projection", projection_tests) 