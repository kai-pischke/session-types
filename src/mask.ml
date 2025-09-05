(* For small role sets (<64) we can use an int64 as bitmask *)
type t = Int64.t  (* bit i set iff role i is in the set *)
let empty = 0L
let add idx m = Int64.logor m (Int64.shift_left 1L idx)
let union = Int64.logor
let inter = Int64.logand
let is_empty m = (m = 0L)
let mem idx m = if Int64.logand m (Int64.shift_left 1L idx) = 0L then false else true
