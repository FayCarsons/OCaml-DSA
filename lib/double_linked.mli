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
val map : ('a -> 'a) -> 'a t -> 'a t
val pop_front : 'a t -> 'a option
val find : ('a -> bool) -> 'a t -> 'a t option
val range : int -> int t
