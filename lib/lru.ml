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

let get : ('k, 'v) t -> 'k -> 'v option =
  fun self key ->
  (* Check if cache contains key *)
  Hashtbl.find_opt self.cache key
  (* If so, remove node pointed to in cache,
     and prepend (<->) to front of list *)
  |> Option.map (fun list_node ->
    let open DList in
    (* Remove the node from the list, leaving us with an isolated node *)
    let temp_node = remove list_node in
    (* prepend node to front *)
    let _ = temp_node <-> self.list in
    (* Advance the least_used pointer *)
    self.least_used <- next self.least_used;
    let _, value = get_exn temp_node in
    value)
;;

let set self key value =
  let open DList in
  if self.size >= self.threshold
  then (
    let temp_key, _ = get_exn self.least_used in
    self.least_used <- prev self.least_used;
    Hashtbl.remove self.cache temp_key;
    self.size <- pred self.size);
  let new_node = node (key, value) in
  self.list <- new_node <-> self.list;
  Hashtbl.add self.cache key new_node;
  self.size <- succ self.size;
  if self.least_used = DList.empty then self.least_used <- new_node
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
