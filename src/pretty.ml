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

