let hash_string string =
  let len = String.length string in
  let rec go state idx =
    if idx = len
    then state
    else (
      let state = state lxor Char.code string.[idx] in
      go state (succ idx))
  in
  go 0 0
;;

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

  (* WRONG not O(1) !! *)

  module Set = Set.Make (Int)

  let rand_entry map =
    let rec get_idx visited =
      let maybe_idx = Random.int map.capacity in
      if Set.mem maybe_idx visited then get_idx visited else maybe_idx
    in
    let rec go visited =
      let idx = get_idx visited in
      match map.buckets.(idx) with
      | { value; _ } :: _ -> Some value
      | [] -> go @@ Set.add idx visited
    in
    go Set.empty
  ;;
end
