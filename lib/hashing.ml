let hash_string = String.fold_left (fun hash c -> hash lxor Char.code c) 0

module Hashmap = struct
  type 'a node =
    { key : string
    ; value : 'a
    }

  type 'a t =
    { mutable buckets : 'a node list Array.t
    ; mutable size : int
    ; mutable capacity : int
    }

  let create capacity = { buckets = Array.make capacity []; size = 0; capacity }

  let rec insert map key value =
    let hashed_key = hash_string key in
    if map.size < map.capacity / 2
    then (
      let index = hashed_key mod map.capacity in
      map.buckets.(index) <- { key; value } :: map.buckets.(index);
      map.size <- succ map.size)
    else (
      map.capacity <- map.capacity * 2;
      map.size <- 0;
      let nodes = Array.fold_left ( @ ) [] map.buckets in
      List.iter (fun { key; value } -> insert map key value) nodes)
  ;;

  let get self k =
    let index = hash_string k mod self.capacity in
    let bucket = self.buckets.(index) in
    let entry = List.find (fun { key; _ } -> key = k) bucket in
    entry.value
  ;;

  let remove map key =
    let index = hash_string key mod map.capacity in
    match map.buckets.(index) with
    | [] -> None
    | [ { value; _ } ] ->
      map.buckets.(index) <- [];
      Some value
    | many ->
      let rec go acc = function
        | { key = k; value } :: xs when k = key -> Some value, acc @ xs
        | x :: xs -> go (x :: acc) xs
        | [] -> None, acc
      in
      let entry, new_bucket = go [] many in
      map.buckets.(index) <- new_bucket;
      entry
  ;;
end

(* Create a data structure with O(1) lookups, removal, and rand-elt *)
module Solution = struct
  (*
     We assume the data structure will store uints to avoid the headache of module functors or boxing our array contents
     a -1 elt represents an empty index;

     alternatively...simply omit the 'buckets' feature of the hashmap, which could cause problems w collisions
     but would give us O(1) random elt lookup
  *)
  type t =
    { map : int Hashmap.t
    ; mutable array : int Array.t
    ; mutable capacity : int
    ; mutable size : int
    }

  let create capacity =
    { map = Hashmap.create capacity
    ; array = Array.make capacity (-1)
    ; size = 0
    ; capacity
    }
  ;;

  let insert self k v =
    if self.size > self.capacity / 2
    then (
      (* Realloc *)
      let new_capacity = self.capacity * 2 in
      let new_arr = Array.make new_capacity (-1) in
      Array.blit self.array 0 new_arr 0 (pred self.size);
      self.array <- new_arr);
    self.array.(self.size) <- v;
    Hashmap.insert self.map k self.size;
    self.size <- succ self.size
  ;;

  let remove self k =
    let index = Hashmap.get self.map k in
    self.array.(index) <- -1;
    Hashmap.remove self.map k |> ignore;
    self.size <- pred self.size
  ;;

  let rand_elt self =
    let idx = Random.int self.size in
    self.array.(idx)
  ;;
end
