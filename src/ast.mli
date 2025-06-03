open Loc

type role  = string
type label = string
type var   = string  (** recursion variables *)

(** Global session types *)
type global =
  | GEnd                                 of t
  | GVar       of var                    * t
  | GRec       of var * global           * t
  | GMsg       of role * role * (label * global) list * t
  | GPar       of global * global        * t

(** Local session types *)
type local =
  | LEnd                                 of t
  | LVar       of var                    * t
  | LRec       of var * local            * t
  | LRecv      of role * (label * local) list * t
  | LSend      of role * (label * local) list * t

val loc_of_global : global -> t
val loc_of_local  : local  -> t

