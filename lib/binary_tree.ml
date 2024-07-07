type 'a t =
  | Empty
  | Node of
      { value : 'a
      ; left : 'a t
      ; right : 'a t
      }

let empty = Empty

let rec member needle = function
  | Empty -> false
  | Node { value; left; right } ->
    value = needle || member needle left || member needle right
;;

let insert tree elt =
  let rec go = function
    | Node { value; left; right } ->
      if value < elt
      then Node { value; left; right = go right }
      else Node { value; left = go left; right }
    | Empty -> Node { value = elt; left = Empty; right = Empty }
  in
  go tree
;;

let rec height : 'a t -> int = function
  | Node { left; right; _ } -> 1 + max (height left) (height right)
  | Empty -> 0
;;

let rec is_balanced = function
  | Node { left; right; _ } ->
    abs (height left - height right) < 2 && is_balanced left && is_balanced right
  | Empty -> true
;;

let split_at list idx =
  let rec go acc elts = function
    | n when n < idx ->
      (match elts with
       | x :: xs -> go (x :: acc) xs (succ n)
       | [] -> List.rev acc, [])
    | _ -> List.rev acc, elts
  in
  go [] list 0
;;

let of_list ?(comparison_fn = compare) list =
  let rec go = function
    | [] -> Empty
    | _ :: _ as elts ->
      let mid = List.length elts / 2 in
      let before, after = split_at elts mid in
      Node { value = List.hd after; left = go before; right = go @@ List.tl after }
  in
  go @@ List.fast_sort comparison_fn list
;;

let rec to_list = function
  | Node { value; left; right } -> to_list left @ [ value ] @ to_list right
  | Empty -> []
;;

let compare_in predicate = function
  | Empty -> true
  | Node { value; _ } -> predicate value
;;

let rec is_valid ?(min = None) ?(max = None) = function
  | Empty -> true
  | Node { value; left; right } ->
    let left_valid =
      match min with
      | Some v -> value > v
      | None -> true
    in
    let right_valid =
      match max with
      | Some v -> value < v
      | None -> true
    in
    left_valid
    && right_valid
    && is_valid ~min ~max:(Some value) left
    && is_valid ~min:(Some value) ~max right
;;

let%test "Member" =
  let list = List.init 100 Fun.id in
  let tree = of_list list in
  member 37 tree
;;

(** Valid balanced tree for testing
    5
    / \
    3   7
    \  / \
    4  6  9 *)
let test_tree =
  Node
    { value = 5
    ; left =
        Node
          { value = 3
          ; left = Empty
          ; right = Node { value = 4; left = Empty; right = Empty }
          }
    ; right =
        Node
          { value = 7
          ; left = Node { value = 6; left = Empty; right = Empty }
          ; right = Node { value = 9; left = Empty; right = Empty }
          }
    }
;;

let%test "Valid" = is_valid test_tree
let%test "Balanced" = is_balanced test_tree
let%test "Height" = height test_tree = 3

let%test "Insert" =
  let updated_tree = insert test_tree 8 in
  member 8 updated_tree && is_valid updated_tree && is_balanced updated_tree
;;
