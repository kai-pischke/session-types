open Loc

type role  = string
type label = string
type base  = string 

type 'v global =
  | GEnd                                 of t
  | GVar       of 'v                     * t
  | GRec       of 'v * 'v global         * t
  | GBra       of role * role * (label * 'v global) list * t
  | GMsg       of role * role * base * 'v global * t
  | GPar       of 'v global * 'v global  * t

type 'v local =
  | LEnd                                 of t
  | LVar       of 'v                     * t
  | LRec       of 'v * 'v local          * t
  | LRecv      of role * (label * 'v local) list * t
  | LSend      of role * (label * 'v local) list * t

let loc_of_global = function
  | GEnd       l
  | GVar (_,l)
  | GRec (_,_,l)
  | GMsg (_,_,_,_,l)
  | GBra (_,_,_,l)
  | GPar (_,_,l) -> l

let loc_of_local = function
  | LEnd       l
  | LVar (_,l)
  | LRec (_,_,l)
  | LRecv (_,_,l)
  | LSend (_,_,l) -> l

