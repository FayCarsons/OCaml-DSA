let rec last = function
  | x :: [] -> Some x
  | _ :: xs -> last xs
  | [] -> None
;;

let%test "Last" = last (List.init 10 Fun.id) = Some 9

let rec last_two = function
  | [ fst; snd ] -> Some (fst, snd)
  | _ :: xs -> last_two xs
  | [] -> None
;;

let%test "Last Two" = last_two (List.init 10 Fun.id) = Some (8, 9)

let nth n list =
  let rec go idx = function
    | x :: _ when idx = pred n -> Some x
    | _ :: xs -> go (succ idx) xs
    | [] -> None
  in
  go 0 list
;;

let%test "Nth" = nth 4 (List.init 10 Fun.id) = Some 3

let len list =
  let rec go length = function
    | _ :: xs -> go (succ length) xs
    | [] -> length
  in
  go 0 list
;;

let%test "Length" = len (List.init 10 Fun.id) = 10

let reverse list =
  let rec go acc = function
    | x :: xs -> go (x :: acc) xs
    | [] -> acc
  in
  go [] list
;;

let%test "Reverse" = reverse [ 1; 2; 3; 4 ] = [ 4; 3; 2; 1 ]

let is_palindrome list =
  let mid = List.length list / 2 in
  let rec split_mid acc idx = function
    | x :: xs when idx < mid -> split_mid (x :: acc) (succ idx) xs
    | _ :: xs -> acc, xs
    | [] -> acc, []
  in
  let left, right = split_mid [] 0 list in
  left = right
;;

let%test "Is palindrome" = is_palindrome [ 'a'; 'b'; 'c'; 'd'; 'c'; 'b'; 'a' ]

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten nodes =
  let rec go acc = function
    | [] -> acc
    | One x :: xs -> go (x :: acc) xs
    | Many nodes :: xs -> go (go acc nodes) xs
  in
  List.rev @@ go [] nodes
;;

let%test "Flatten nodes" =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;

let dedup list =
  let rec go acc last = function
    | x :: xs when x = last -> go acc last xs
    | x :: xs -> go (x :: acc) x xs
    | [] -> List.rev acc
  in
  match list with
  | x :: xs -> go [ x ] x xs
  | [] -> []
;;

let%test "Dedup" =
  dedup [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

let pack_dups list =
  let rec go acc current_list current_elt = function
    | x :: xs when x = current_elt -> go acc (x :: current_list) current_elt xs
    | x :: xs -> go (current_list :: acc) [ x ] x xs
    | [] -> List.rev (current_list :: acc)
  in
  match list with
  | x :: xs -> go [] [ x ] x xs
  | [] -> []
;;

let%test "Pack Dups" =
  pack_dups [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

let encode list =
  let rec go acc count current = function
    | x :: xs when x = current -> go acc (succ count) current xs
    | x :: xs -> go ((count, current) :: acc) 1 x xs
    | [] -> List.rev @@ ((count, current) :: acc)
  in
  match list with
  | x :: xs -> go [] 1 x xs
  | [] -> []
;;

let%test "Run length encoding" =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

type 'a rle =
  | Single of 'a
  | More of int * 'a

let node_encode list =
  let rec go acc count current = function
    | x :: xs when x = current -> go acc (succ count) current xs
    | x :: xs ->
      let node = if count > 1 then More (count, current) else Single current in
      go (node :: acc) 1 x xs
    | [] ->
      let node = if count > 1 then More (count, current) else Single current in
      List.rev @@ (node :: acc)
  in
  match list with
  | x :: xs -> go [] 1 x xs
  | [] -> []
;;

let%test "Node encoding" =
  node_encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ More (4, "a"); Single "b"; More (2, "c"); More (2, "a"); Single "d"; More (4, "e") ]
;;

let node_decode list =
  let rec repeat acc x = function
    | 0 -> acc
    | n -> repeat (x :: acc) x (pred n)
  in
  let rec go acc = function
    | More (n, x) :: xs -> go (repeat acc x n) xs
    | Single x :: xs -> go (x :: acc) xs
    | [] -> List.rev acc
  in
  go [] list
;;

let dup n list =
  let rec repeat acc x = function
    | 0 -> acc
    | n -> repeat (x :: acc) x (pred n)
  in
  let rec go acc = function
    | x :: xs -> go (repeat acc x n) xs
    | [] -> List.rev acc
  in
  go [] list
;;

let drop_every n list =
  let rec go acc count = function
    | _ :: xs when count = n -> go acc 1 xs
    | x :: xs -> go (x :: acc) (succ count) xs
    | [] -> List.rev acc
  in
  go [] 1 list
;;

let slice start end' list =
  let rec go acc idx = function
    | x :: xs when idx >= start && idx <= end' -> go (x :: acc) (succ idx) xs
    | _ :: xs -> go acc (succ idx) xs
    | _ when idx > end' -> List.rev acc
    | [] -> List.rev acc
  in
  go [] 0 list
;;

let rotate n list =
  let rec go acc count = function
    | x :: xs when count < n -> go (x :: acc) (succ count) xs
    | [] -> List.rev acc
    | rest -> rest @ List.rev acc
  in
  go [] 0 list
;;

let permute list =
  let len = List.length list in
  List.map (fun x -> x, Random.int len) list
  |> List.fast_sort (fun (_, a) (_, b) -> compare a b)
  |> List.map fst
;;

(* Taken from Neetcode's 'two-pointers' section
   O(n^2) solution *)
let largest_container heights =
  let len = Array.length heights in
  let area (left_x, left_y) (right_x, right_y) =
    let width = right_x - left_x
    and height = min left_y right_y in
    width * height
  in
  let rec find_largest current_best left right =
    if left = len
    then current_best
    else if right = len
    then find_largest current_best (succ left) 0
    else (
      let current_area = area (left, heights.(left)) (right, heights.(right)) in
      find_largest (max current_best current_area) left (succ right))
  in
  find_largest 0 0 0
;;

let largest_container_linear heights =
  let len = Array.length heights in
  let area x1 y1 x2 y2 =
    let width = x2 - x1
    and height = min y1 y2 in
    width * height
  in
  let rec go best left right =
    if left < right
    then (
      let current_area = area left heights.(left) right heights.(right) in
      if heights.(left) < heights.(right)
      then go (max best current_area) (succ left) right
      else go (max best current_area) left (pred right))
    else best
  in
  go 0 0 (pred len)
;;

let%test "Largest container - Case one" =
  let heights = [| 1; 7; 2; 5; 4; 7; 3; 6 |] in
  largest_container heights = 36
;;

let%test "Largest contaner - case two" =
  let heights = [| 2; 2; 2 |] in
  largest_container heights = 4
;;

let cartesian_product list =
  let rec loop acc = function
    | x :: xs ->
      let paired = List.map (fun y -> x, y) list in
      loop (paired @ acc) xs
    | [] -> acc
  in
  loop [] list
;;

(* Taken from leetcode 'Kids with candies' *)
let kids_with_candies candies extra =
  let max_candies = List.fold_left (fun acc n -> if n > acc then n else acc) 0 candies in
  let result = Array.make (List.length candies) false in
  let _ =
    List.iteri
      (fun idx n -> if n + extra >= max_candies then result.(idx) <- true)
      candies
  in
  result
;;

let%test "Kids with candies - Case one" =
  let candies = [ 2; 3; 5; 1; 3 ]
  and extra = 3 in
  kids_with_candies candies extra = [| true; true; true; false; true |]
;;

let increasing_triplet nums =
  let rec loop first second = function
    | x :: xs when x <= first -> loop x second xs
    | x :: xs when x <= second -> loop first x xs
    | _ :: _ -> true
    | [] -> false
  in
  loop Int.max_int Int.max_int nums
;;
