let test_list = [ 2; 6; 1; 99; 42; 690; 4; 5 ]
let test_list_sorted = [ 1; 2; 4; 5; 6; 42; 99; 690 ]

let rec merge : 'a list * 'a list -> 'a list = function
  | x :: xs, y :: ys ->
    if x < y then x :: merge (xs, y :: ys) else y :: merge (x :: xs, ys)
  | [], right -> right
  | left, [] -> left
;;

let split_at n list =
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
    let left, right = split_at mid list in
    merge (merge_sort left, merge_sort right)
;;

let%test "Merge Sort" = merge_sort test_list = test_list_sorted

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
    let[@warning "-8"] before, mid :: after = split_at mid xs in
    let less_than, greater_than = partition (fun n -> n < mid) (before @ after) in
    quicksort less_than @ (mid :: quicksort greater_than)
;;

let%test "Quick Sort" = quicksort test_list = test_list_sorted
let ( >>= ) = Result.bind
let ( >>| ) = Result.map
