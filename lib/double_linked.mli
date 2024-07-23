type 'a t

val is_empty : 'a t -> bool
val get_exn : 'a t -> 'a
val get : 'a t -> 'a option
val prev : 'a t -> 'a t
val next : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val ( <-> ) : 'a t -> 'a t -> 'a t
val delete : 'a t -> unit
val remove : 'a t -> 'a t
val node : 'a -> 'a t
val empty : 'a t
val map : f:('a -> 'b) -> 'a t -> 'b t
val map_inplace : f:('a -> 'a) -> 'a t -> unit
val filter : predicate:('a -> bool) -> 'a t -> 'a t
val filter_inplace : predicate:('a -> bool) -> 'a t -> unit
val fold_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a t -> 'acc
val pop_front : 'a t -> 'a option
val find : predicate:('a -> bool) -> 'a t -> 'a t option
val range : int -> int t
val length : 'a t -> int
val copy : 'a t -> 'a t
val nth : int -> 'a t -> 'a option
val nth_node : int -> 'a t -> 'a t option
val pointer_array : 'a t -> 'a t array
