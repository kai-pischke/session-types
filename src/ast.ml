open Loc

type role  = string
type label = string
type var   = string

type global =
  | GEnd                                 of t
  | GVar       of var                    * t
  | GRec       of var * global           * t
  | GMsg       of role * role * (label * global) list * t
  | GPar       of global * global        * t

type local =
  | LEnd                                 of t
  | LVar       of var                    * t
  | LRec       of var * local            * t
  | LRecv      of role * (label * local) list * t
  | LSend      of role * (label * local) list * t

let loc_of_global = function
  | GEnd       l
  | GVar (_,l)
  | GRec (_,_,l)
  | GMsg (_,_,_,l)
  | GPar (_,_,l) -> l

let loc_of_local = function
  | LEnd       l
  | LVar (_,l)
  | LRec (_,_,l)
  | LRecv (_,_,l)
  | LSend (_,_,l) -> l

