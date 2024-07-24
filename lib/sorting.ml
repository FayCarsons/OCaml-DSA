let test_list = [ 2; 6; 1; 99; 42; 690; 4; 5 ]
let test_list_sorted = [ 1; 2; 4; 5; 6; 42; 99; 690 ]

let rec merge : 'a list * 'a list -> 'a list = function
  | x :: xs, y :: ys ->
    if x < y then x :: merge (xs, y :: ys) else y :: merge (x :: xs, ys)
  | [], right -> right
  | left, [] -> left
;;

let split n list =
  let rec go before idx = function
    | x :: xs when idx < n -> go (x :: before) (succ idx) xs
    | rest -> List.rev before, rest
  in
  go [] 0 list
;;

let rec merge_sort : 'a list -> 'a list = function
  | [] -> []
  | x :: [] -> [ x ]
  | list ->
    let mid = List.length list / 2 in
    let left, right = split mid list in
    merge (merge_sort left, merge_sort right)
;;

let%test "Merge Sort" = merge_sort test_list = test_list_sorted

let split_with_mid n list =
  let rec go acc idx = function
    | x :: xs when idx < n -> go (x :: acc) (succ idx) xs
    | mid :: rest -> acc, mid, rest
    | [] -> raise (Invalid_argument "Split_with_mid")
  in
  go [] 0 list
;;

let partition predicate list =
  let rec go (left, right) = function
    | x :: xs ->
      if predicate x then go (x :: left, right) xs else go (left, x :: right) xs
    | [] -> left, right
  in
  go ([], []) list
;;

let rec quicksort : 'a list -> 'a list = function
  | ([] | [ _ ]) as xs -> xs
  | xs ->
    let mid = List.length xs / 2 in
    let before, mid, after = split_with_mid mid xs in
    let less_than, greater_than = partition (fun n -> n < mid) (before @ after) in
    quicksort less_than @ (mid :: quicksort greater_than)
;;

let%test "Quick Sort" = quicksort test_list = test_list_sorted

open Core

let rec bubble_sort_array array =
  let rec loop acc = function
    | 0 -> assert false
    | n when n = Array.length array -> acc
    | n ->
      if array.(pred n) > array.(n)
      then (
        Array.swap array (pred n) n;
        loop true (succ n))
      else loop acc (succ n)
  in
  if loop false 1 then bubble_sort_array array
;;

let%test "Bubble sort array" =
  let a = Array.init 10 ~f:(fun _ -> Random.int 100) in
  bubble_sort_array a;
  Array.is_sorted ~compare:Int.compare a
;;

let rec bubble_sort ~compare list =
  let rec loop acc swapped = function
    | x :: y :: xs ->
      if compare x y = 1
      then loop (y :: acc) true (x :: xs)
      else loop (x :: acc) swapped (y :: xs)
    | x :: [] -> loop (x :: acc) swapped []
    | [] -> acc, swapped
  in
  let list, swapped = loop [] false list in
  let list = List.rev list in
  if swapped then bubble_sort ~compare list else list
;;
