(*--------------------------------------------------------------------*)
(*  Pretty: minimal global-type pretty-printer                         *)
(*--------------------------------------------------------------------*)
open Format
open Ast

(* pp_global pp_var fmt g  ------------------------------------------------*)
(*  - [pp_var] prints a variable (either int or string)                   *)
(*  - [fmt]    is the formatter                                          *)
(*  - [g]      is the global type                                        *)
let rec pp_global pp_var fmt = function
| GEnd _ ->
    pp_print_string fmt "end"

| GVar (v, _) ->
    pp_var fmt v

| GRec (v, body, _) ->
    fprintf fmt "rec %a. %a" pp_var v (pp_global pp_var) body

| GMsg (p, q, base, cont, _) ->
    fprintf fmt "%s -> %s : %s. %a"
        p q base (pp_global pp_var) cont

| GBra (p, q, branches, _) ->
    fprintf fmt "%s -> %s { " p q;
    let first = ref true in
    List.iter
        (fun (lbl, g) ->
            if !first then first := false else pp_print_string fmt " ; ";
            fprintf fmt "%s: %a" lbl (pp_global pp_var) g)
        branches;
    pp_print_string fmt " }"

| GPar (g1, g2, _) ->
    fprintf fmt "( %a | %a )"
        (pp_global pp_var) g1
        (pp_global pp_var) g2

(* Convenience: get a string back ---------------------------------------*)
let string_of pp_var g =
asprintf "%a" (pp_global pp_var) g

(* pp_local pp_var fmt l  ------------------------------------------------*)
let rec pp_local pp_var fmt = function
| LEnd _ ->
    pp_print_string fmt "end"

| LVar (v, _) ->
    pp_var fmt v

| LRec (v, body, _) ->
    fprintf fmt "rec %a. %a" pp_var v (pp_local pp_var) body

| LSend (r, base, cont, _) ->
    fprintf fmt "%s ! %s. %a" r base (pp_local pp_var) cont

| LRecv (r, base, cont, _) ->
    fprintf fmt "%s ? %s. %a" r base (pp_local pp_var) cont

| LInt (r, branches, _) ->
    fprintf fmt "%s ? { " r;
    let first = ref true in
    List.iter
      (fun (lbl, l) ->
        if !first then first := false else pp_print_string fmt " ; ";
        fprintf fmt "%s: %a" lbl (pp_local pp_var) l)
      branches;
    pp_print_string fmt " }"

| LExt (r, branches, _) ->
    fprintf fmt "%s ! { " r;
    let first = ref true in
    List.iter
      (fun (lbl, l) ->
        if !first then first := false else pp_print_string fmt " ; ";
        fprintf fmt "%s: %a" lbl (pp_local pp_var) l)
      branches;
    pp_print_string fmt " }"

let string_of_local pp_var l =
  asprintf "%a" (pp_local pp_var) l

