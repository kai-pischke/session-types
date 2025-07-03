open Ast

(* Type-safe representation of destinations during building *)
type destination =
  | State of int                    (* Direct state reference *)
  | Variable of int                 (* Variable reference to be resolved *)
  | End                             (* End state *)

(* Intermediate state kind during building *)
type intermediate_state_kind =
  | Snd of base * destination        (* send base + single successor *)
  | Rcv of base * destination        (* receive base + single successor *)
  | Int of (label * destination) list   (* internal choice on labels *)
  | Ext of (label * destination) list   (* external choice on labels *)

(* Final state kind after resolution *)
type state_kind =
  | Snd of base * int option         (* base + successor (None for end) *)
  | Rcv of base * int option         (* base + successor (None for end) *)
  | Int of (label * int option) list (* internal choice on labels *)
  | Ext of (label * int option) list (* external choice on labels *)

(* The local automaton/graph: *)
type graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_state  : int option;           (* S_0: start state (None if empty) *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : role array;           (* id ↦ role                        *)
  kinds        : state_kind array;     (* id ↦ behaviour (Snd | Rcv | Int | Ext) *)
}

(* Convert a local type to an automaton *)
val of_local : int local -> graph

(* Pretty printing *)
val pp_graph : Format.formatter -> graph -> unit
val string_of_graph : graph -> string 