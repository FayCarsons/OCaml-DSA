(** Min and Max heaps
    Because they're necessary for other algorithms in the project *)

type ('k, 'v) t

val size : ('k, 'v) t -> int
val insert : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
val first : ('k, 'v) t -> ('k * 'v) option
val pop : ('k, 'v) t -> ('k, 'v) t * ('k * 'v) option

(** Create an empty Min Heap *)
val create_min : unit -> ('k, 'v) t

(** Create an empty Max Heap *)
val create_max : unit -> ('k, 'v) t

(** Create a Min Heap from a list *)
val min_of_list : ('k * 'v) list -> ('k, 'v) t

(** Create a Max Heap from a list *)
val max_of_list : ('k * 'v) list -> ('k, 'v) t

val is_empty : ('k, 'v) t -> bool
val pp : (string, int) t -> unit
