(* Ast.ml: 
This file defines the abstract syntax for processes 
and for local and global types.*)

(* Locations are used for debugging. *)
open Loc

type role  = string (* p, q, ... *)
type label = string (* l1, l2, ...*)
type base  = string (* int, char, ...*)

(* Global types G1, G2, ...*)
type 'v global =
  | GEnd                                 of t
  | GVar       of 'v                     * t
  | GRec       of 'v * 'v global         * t
  | GBra       of role * role * (label * 'v global) list * t
  | GMsg       of role * role * base * 'v global * t
  | GPar       of 'v global * 'v global  * t

(* Local Types T1, T2, ...*)
type 'v local =
  (* end *)
  | LEnd                                 of t
  (* t *)
  | LVar       of 'v                     * t
  (* 𝜇t.T *)
  | LRec       of 'v * 'v local          * t
  (* p&{l1, ..., ln} *)
  | LInt       of role * (label * 'v local) list * t
  (* p⊕{l1, ..., ln} *)
  | LExt       of role * (label * 'v local) list * t
  (* p?⟨B⟩; T*)
  | LRecv      of role * base * 'v local * t
  (* p!⟨B⟩; T *)
  | LSend      of role * base * 'v local * t

(* Expressions e1, e2, ...
type 'v expr =
  (* true *)
  | ETrue      of t
  (* false *)
  | EFalse     of t
  (* e.g. 42 *)
  | EInt       of int * t 
  (* x *)
  | EVar       of string * t 
  (* ¬e *)
  | ENot       of 'v expr
  (* e1 ∨ e2*)
  | EOr        of 'v expr * 'v expr
  (* e1 + e2*)
  | EPlus      of 'v expr * 'v expr

(* Processes P1, P2, ...*)
type 'v processes =
  (* 0 *)
  | PInact     of t
  (* X *)
  | PVar       of 'v                     * t
  (* 𝜇X.P *)
  | PRec       of 'v * 'v local          * t
  (* p ⊲ l.P *)
  | PInt       of role * (label * 'v local) list * t
  (* p ⊳ {l1,...,ln} *)
  | PExt       of role * (label * 'v local) list * t
  (* p!⟨e⟩.P *)
  | PSend      of role * base * 'v local * t
  (* p(x).P *)
  | PRecv      of role * base * 'v local * t
 *)
(* --- boring utility functions ---*)
(* Extract the location from a global. *)
let loc_of_global = function
  | GEnd       l
  | GVar (_,l)
  | GRec (_,_,l)
  | GMsg (_,_,_,_,l)
  | GBra (_,_,_,l)
  | GPar (_,_,l) -> l

(* Extract the location from a local. *)
let loc_of_local = function
  | LEnd       l
  | LVar (_,l)
  | LRec (_,_,l)
  | LInt (_,_,l)
  | LExt (_,_,l)
  | LRecv (_,_,_,l)
  | LSend (_,_,_,l) -> l
