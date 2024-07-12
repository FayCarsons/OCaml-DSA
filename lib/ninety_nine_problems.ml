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
