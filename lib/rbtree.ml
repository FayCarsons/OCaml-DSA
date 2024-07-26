(* Red-Black tree implementation described in Chris Okasaki's `Purely functional data structures` *)

open! Core

type ('key, 'data) t =
  | Node of ('key, 'data) node
  | Empty

and ('key, 'data) node =
  { color : color
  ; left : ('key, 'data) t
  ; key : 'key
  ; data : 'data
  ; right : ('key, 'data) t
  }

and color =
  | Red
  | Black

let member : ?compare:('key -> 'key -> int) -> key:'key -> ('key, 'data) t -> bool =
  fun ?(compare = Poly.compare) ~key tree ->
  let rec search = function
    | Node { left; key = key'; right; _ } ->
      (match compare key key' with
       | 0 -> true
       | -1 -> search left
       | _ -> search right)
    | Empty -> false
  in
  search tree
;;

(*
   Balances the (sub)tree
   If you have a problem with this code take it up with Chris Okasaki.
   Personally, I find it easier to reason about than my prevous implementations.
   TODO : nevermind, switching to product type node has ruined this, pls format
*)

let balance : ('key, 'data) t -> ('key, 'data) t = function
  | Node
      { color = Black
      ; left =
          Node
            { color = Red
            ; left = Node { color = Red; left = a; key = kx; data = dx; right = b }
            ; key = ky
            ; data = dy
            ; right = c
            }
      ; key = kz
      ; data = dz
      ; right = d
      }
  | Node
      { color = Black
      ; left =
          Node
            { color = Red
            ; left = a
            ; key = kx
            ; data = dx
            ; right = Node { color = Red; left = b; key = ky; data = dy; right = c }
            }
      ; key = kz
      ; data = dz
      ; right = d
      }
  | Node
      { color = Black
      ; left = a
      ; key = kx
      ; data = dx
      ; right =
          Node
            { color = Red
            ; left = Node { color = Red; left = b; key = ky; data = dy; right = c }
            ; key = kz
            ; data = dz
            ; right = d
            }
      }
  | Node
      { color = Black
      ; left = a
      ; key = kx
      ; data = dx
      ; right =
          Node
            { color = Red
            ; left = b
            ; key = ky
            ; data = dy
            ; right = Node { color = Red; left = c; key = kz; data = dz; right = d }
            }
      } ->
    Node
      { color = Red
      ; left = Node { color = Black; left = a; key = kx; data = dx; right = b }
      ; key = ky
      ; data = dy
      ; right = Node { color = Black; left = c; key = kz; data = dz; right = d }
      }
  | t -> t
;;

let curry f (a, b) = f a b

let assert_invariants self =
  let rec valid = function
    | Node { color = Red; left = Node { color = Red; _ }; _ }
    | Node { color = Red; right = Node { color = Red; _ }; _ } -> None
    | Node { color; left; right; _ } ->
      let num_black =
        match color with
        | Black -> 1
        | Red -> 0
      in
      (* Ensure that both left + right subtrees uphold invariants *)
      Option.both (valid left) (valid right)
      |> Option.filter ~f:(curry ( = ))
      (* Return the left's count (as left = right), incremented if the current node is black *)
      |> Option.map ~f:Util.(fst >> ( + ) num_black)
    | Empty -> Some 0
  in
  match self with
  | Node { color = Black; _ } -> Option.is_some @@ valid self
  | _ -> false
;;

let insert
  : ?compare:('key -> 'key -> int) -> ('key, 'data) t -> 'key -> 'data -> ('key, 'data) t
  =
  fun ?(compare = Poly.compare) self key data ->
  let rec insert' = function
    | Node node ->
      (match compare key node.key with
       | 1 -> balance @@ Node { node with right = insert' node.right }
       | 0 -> Node { node with key; data }
       | -1 -> balance @@ Node { node with left = insert' node.left }
       | _ -> assert false)
    | Empty -> Node { color = Red; left = Empty; key; data; right = Empty }
  in
  match insert' self with
  | Node node -> Node { node with color = Black }
  (* Unreachable - the result of this operation cannot be empty *)
  | _ -> assert false
;;

let find : ?compare:('key -> 'key -> int) -> ('key, 'data) t -> key:'key -> 'data option =
  fun ?(compare = Poly.compare) self ~key ->
  let rec find' = function
    | Node node ->
      if key = node.key
      then Some node.data
      else if compare key node.key = -1
      then find' node.left
      else find' node.right
    | Empty -> None
  in
  find' self
;;

let rec successor_of_right = function
  | Node { left = Empty; _ } as node -> node
  | Node has_left_child -> successor_of_right has_left_child.left
  | Empty -> assert false
;;

let rec delete
  : ?compare:('key -> 'key -> int) -> ('key, 'data) t -> key:'key -> ('key, 'data) t
  =
  fun ?(compare = Poly.compare) self ~key ->
  let rec delete' = function
    | Node node ->
      (match compare key node.key with
       | -1 -> Node { node with left = delete' node.left }
       | 1 -> Node { node with right = delete' node.right }
       | _ -> aux node)
    | Empty -> Empty
  and aux = function
    | { left = Empty; right; _ } -> right
    | { right = Empty; left; _ } -> left
    | _ ->
      (match self with
       | Node { left = Empty; right; _ } -> right
       | Node { right = Empty; left; _ } -> left
       | Node root ->
         let[@warning "-8"] (Node { key; _ }) = successor_of_right root.right in
         Node { root with right = delete ~compare root.right ~key }
       | _ -> assert false)
  in
  delete' self
;;
