open Core

type 'a tree =
  | Node of 'a tree list
  | Leaf of 'a
  | Hole

let rec pp = function
  | Leaf x -> "Leaf: " ^ x
  | Node children ->
    "Node {\n  "
    ^ List.fold_left ~f:(fun acc node -> acc ^ pp node) ~init:"" children
    ^ "\n}"
  | Hole -> "*"
;;

let recurry f a b = f (a, b)
let inner children = Node children

let choose = function
  | t, Hole -> t
  | _, replacement -> replacement
;;

let replace (tree : string tree) (pattern : string tree) (replacement : string tree) =
  let go = function
    | Leaf a ->
      (match pattern with
       | Leaf b when String.equal a b -> replacement
       | _ -> Leaf a)
    | Hole -> failwith "Unreachable"
    | node ->
      let rec is_equal = function
        | _, Hole -> true
        | Leaf a, Leaf b -> String.equal a b
        | Node t, Node pat ->
          (try List.for_all2_exn ~f:(recurry is_equal) t pat with
           | _ -> false)
        | _ -> false
      in
      if is_equal (node, pattern) then replacement else node
  in
  go tree
;;

let test_tree = Node [ Leaf "A"; Node [ Leaf "C" ]; Leaf "B" ]
let test_pattern = Node [ Leaf "A"; Hole; Leaf "B" ]
let test_replacement = Node [ Leaf "C"; Hole ]

let%test "Trivial" =
  let tree = Node [ Leaf "A"; Node [ Leaf "C" ]; Leaf "B" ] in
  let pattern = Node [ Leaf "A"; Hole; Leaf "B" ] in
  let replacement = Hole in
  let result = Node [ Leaf "C" ] in
  let ret = replace tree pattern replacement in
  print_endline @@ pp ret;
  Poly.equal ret result
;;

let%test "Recursive case" =
  let tree = Node [ Leaf "A"; Node [ Leaf "B"; Node [ Leaf "A"; Node [ Leaf "" ] ] ] ] in
  let pattern = Node [ Leaf "A"; Hole ] in
  let replacement = Node [ Leaf "C"; Hole ] in
  let result =
    Node [ Leaf "C"; Node [ Leaf "B"; Node [ Leaf "C"; Node [ Leaf "" ] ] ] ]
  in
  Poly.equal (replace tree pattern replacement) result
;;

let%test "Degenerate case" =
  let tree = Node [ Leaf "A"; Node [ Leaf "B"; Node [ Leaf "A"; Node [ Leaf "" ] ] ] ] in
  let pattern = Hole in
  let replacement = Leaf "D" in
  let result = Leaf "D" in
  Poly.equal (replace tree pattern replacement) result
;;
