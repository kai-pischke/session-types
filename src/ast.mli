open Loc

type role  = string
type label = string
type base  = string

(** Global session types *)
type 'v global =
  | GEnd                                 of t
  | GVar       of 'v                     * t
  | GRec       of 'v * 'v global         * t
  | GBra       of role * role * (label * 'v global) list * t
  | GMsg       of role * role * base * 'v global * t
  | GPar       of 'v global * 'v global  * t

(** Local session types *)
type 'v local =
  | LEnd                                 of t
  | LVar       of 'v                     * t
  | LRec       of 'v * 'v local          * t
  | LRecv      of role * (label * 'v local) list * t
  | LSend      of role * (label * 'v local) list * t


val loc_of_global : 'v global -> t
val loc_of_local  : 'v local  -> t

