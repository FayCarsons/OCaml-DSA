module DList = Double_linked

type ('k, 'v) t =
  { cache : ('k, ('k * 'v) DList.t) Hashtbl.t
  ; mutable list : ('k * 'v) DList.t
  ; mutable least_used : ('k * 'v) DList.t
  ; threshold : int
  ; mutable size : int
  }

let create threshold =
  let cache = Hashtbl.create threshold in
  let list = DList.empty in
  let least_used = list in
  let size = 0 in
  { cache; list; least_used; threshold; size }
;;
(*
   let to_string a = Marshal.to_string a []

   (* Wraps 'f', a two argument function, in a memoization function that stores
   'threshold' most recent outputs *)
   let memoize2 f threshold =
   let lru = create threshold in
   fun arg1 arg2 ->
   let key =
   let a1 = to_string arg1
   and a2 = to_string arg2 in
   a1 ^ a2
   in
   match get lru key with
   | Some value -> value
   | None ->
   let new_value = f arg1 arg2 in
   set lru key new_value;
   new_value
   ;;

   let memoize3 f threshold =
   let lru = create threshold in
   fun arg1 arg2 arg3 ->
   let key =
   let a1 = to_string arg1
   and a2 = to_string arg2
   and a3 = to_string arg3 in
   a1 ^ a2 ^ a3
   in
   match get lru key with
   | Some value -> value
   | None ->
   let new_value = f arg1 arg2 arg3 in
   set lru key new_value;
   new_value
   ;;
*)
