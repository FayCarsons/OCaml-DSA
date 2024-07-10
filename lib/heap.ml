type ('k, 'v) heap =
  | Node of (('k, 'v) heap * 'k * 'v * ('k, 'v) heap)
  | Empty

type ('k, 'v) t =
  { heap : ('k, 'v) heap
  ; size : int
  ; order : 'k -> 'k -> bool
  }

let create_min () = { heap = Empty; size = 0; order = ( > ) }
let create_max () = { heap = Empty; size = 0; order = ( < ) }

let first = function
  | { heap = Empty; _ } -> None
  | { heap = Node (_, key, value, _); _ } -> Some (key, value)
;;

let pop = function
  | { heap = Empty; _ } as t -> t, None
  | { heap = Node (left, key, value, right); size; order } as t ->
    let rec merge_heap : ('k, 'v) heap * ('k, 'v) heap -> ('k, 'v) heap = function
      | Empty, node | node, Empty -> node
      | (Node (l1, k1, v1, r1) as left), (Node (l2, k2, v2, r2) as right) ->
        if order k1 k2
        then Node (merge_heap (r1, right), k1, v1, l1)
        else Node (merge_heap (r2, left), k2, v2, l2)
    in
    { t with heap = merge_heap (left, right); size = pred size }, Some (key, value)
;;

let is_empty = function
  | { heap = Empty; _ } -> true
  | _ -> false
;;

let rec count = function
  | Node (left, _, _, right) -> 1 + count left + count right
  | Empty -> 0
;;

let size { size; _ } = size

let insert ({ heap; size = heap_size; order } as self) key value =
  let rec place = function
    | Empty -> Node (Empty, key, value, Empty) (* New node *)
    | Node (left, k, v, right) ->
      if count left <= count right
      then bubble_up @@ Node (place left, k, v, right)
      else bubble_up @@ Node (left, k, v, place right)
  and bubble_up = function
    | Empty -> Empty
    | Node (left, k, v, right) as current_node ->
      let new_left =
        match left with
        | Empty -> current_node
        | Node (left_left, left_key, left_value, left_right) ->
          if order k left_key
          then Node (Node (left_left, k, v, left_right), left_key, left_value, right)
          else current_node
      in
      (match new_left with
       | Node (left, k, v, right) as current_node ->
         (match right with
          | Empty -> Node (left, k, v, Empty)
          | Node (right_left, right_key, right_value, right_right) ->
            if order k right_key
            then Node (left, right_key, right_value, Node (right_left, k, v, right_right))
            else current_node)
       | _ -> new_left)
  in
  { self with heap = place heap; size = succ heap_size }
;;

let min_of_list : ('k * 'v) list -> ('k, 'v) t =
  fun elts -> List.fold_left (fun heap (k, v) -> insert heap k v) (create_min ()) elts
;;

let max_of_list : ('k * 'v) list -> ('k, 'v) t =
  fun elts -> List.fold_left (fun heap (k, v) -> insert heap k v) (create_max ()) elts
;;

let pp { heap; _ } =
  let rec go indent = function
    | Node (left, key, value, right) ->
      let indentation = String.make indent '\t' in
      Printf.sprintf
        "%sNode {\n%s\tleft: %s\n%s\tkey: %s\n%s\tvalue: %d\n%s\tright: %s\n%s}"
        indentation
        indentation
        (go (indent + 1) left)
        indentation
        key
        indentation
        value
        indentation
        (go (indent + 1) right)
        indentation
    | Empty -> "Empty"
  in
  print_string @@ go 0 heap
;;

let%test "Heap property" =
  let elts = List.init 10 (fun i -> string_of_int i, i) in
  let heap = create_min () in
  let with_elts = List.fold_left (fun heap (k, v) -> insert heap k v) heap elts in
  first with_elts = Some ("0", 0)
;;

let%test "Size" =
  let elts = List.init 10 (fun i -> string_of_int i, i) in
  let heap = create_min () in
  let with_elts = List.fold_left (fun heap (k, v) -> insert heap k v) heap elts in
  size with_elts = 10
;;

let%test "Floats" =
  let elts = [ 3., 'A'; 7., 'B'; 2., 'C'; 9.375, 'D'; 1.111, 'E'; 0.01, 'F' ] in
  let heap = min_of_list elts in
  first heap = Some (0.01, 'F')
;;
