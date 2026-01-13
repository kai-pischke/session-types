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

(* Int-based version of the graph *)
type int_graph = {
  num_states   : int;                  (* n: states are 0..n-1            *)
  start_state  : int option;           (* S_0: start state (None if empty) *)

  (* Efficient state-indexed maps (arrays) *)
  roles        : int array;            (* id ↦ role (as int)               *)
  kinds        : state_kind array;     (* id ↦ behaviour (Snd | Rcv | Int | Ext) *)
}

(* Convert a local type to an automaton *)
val of_local : int local -> graph

(* Helper functions for creating empty graphs *)
val empty_graph : unit -> graph
val empty_int_graph : unit -> int_graph

(* Conversion functions between string and int representations *)
val graph_to_int_graph : graph -> (role -> int) -> int_graph
val int_graph_to_graph : int_graph -> (int -> role) -> graph

(* Create int-based graph directly from local type *)
val of_local_int : int local -> (role -> int) -> int_graph

(* Pretty printing *)
val pp_graph : Format.formatter -> graph -> unit
val string_of_graph : graph -> string
val pp_int_graph : Format.formatter -> int_graph -> unit
val string_of_int_graph : int_graph -> string

(* Export *)
val dot_of_graph : graph -> string
val json_of_graph : graph -> string
