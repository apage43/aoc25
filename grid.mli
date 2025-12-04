type 'a t = { w : int; h : int; grid : 'a array }

val at : 'a t -> int * int -> 'a option
val all_cells : 'a t -> (int * int) Base.list
val adjacents : int * int -> (int * int) list
val of_2d_array : 'a array array -> 'a t
val copy : 'a t -> 'a t
val set : 'a t -> int * int -> 'a -> unit
