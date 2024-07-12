type 'a t =
  | Empty
  | Node of 'a node

and 'a node =
  { mutable prev : 'a t
  ; mutable inner : 'a
  ; mutable next : 'a t
  }

let is_empty = function
  | Empty -> true
  | _ -> false
;;

(** Will raise if node is empty *)
let[@warning "-8"] get_exn (Node node) = node.inner

let get = function
  | Node { inner; _ } -> Some inner
  | Empty -> None
;;

let prev = function
  | Node { prev; _ } -> prev
  | node -> node
;;

let next = function
  | Node { next; _ } -> next
  | node -> node
;;

let append left right =
  match left, right with
  | Empty, Empty -> Empty
  | Empty, _ -> right
  | node, Empty -> node
  | Node left, Node right ->
    let rec walk_left node =
      match node.next with
      | Empty -> node
      | Node next_node -> walk_left next_node
    in
    let last_of_left = walk_left left in
    last_of_left.next <- Node right;
    right.prev <- Node last_of_left;
    Node left
;;

let ( <-> ) = append

(** Given a pointer to a node somewhere in the list,
    remove references to that node *)
let delete = function
  | Node { prev = Node prev; next = Node next; _ } ->
    prev.next <- Node next;
    next.prev <- Node prev
  | Node { prev = Node prev; next = Empty; _ } -> prev.next <- Empty
  | Node { prev = Empty; next = Node next; _ } -> next.prev <- Empty
  | _ -> ()
;;

let isolate = function
  | Node node ->
    node.prev <- Empty;
    node.next <- Empty;
    Node node
  | Empty -> Empty
;;

(** Remove a node from its space in the list, returning the node *)
let remove = function
  | Node { prev = Node prev; next = Node next; _ } as current ->
    prev.next <- Node next;
    next.prev <- Node prev;
    isolate current
  | Node { prev = Node prev; next = Empty; _ } as current ->
    prev.next <- Empty;
    isolate current
  | Node { prev = Empty; next = Node next; _ } as current ->
    next.prev <- Empty;
    isolate current
  | node -> node
;;

let node inner = Node { prev = Empty; inner; next = Empty }
let empty = Empty

(* Rewrite to be tail-recursive *)
let rec map (f : 'a -> 'a) = function
  | Empty -> Empty
  | Node node ->
    node.inner <- f node.inner;
    node.next <- map f node.next;
    Node node
;;

let rec find predicate = function
  | Node { inner; next; _ } as node ->
    if predicate inner then Some node else find predicate next
  | Empty -> None
;;

let pop_front = function
  | Empty -> None
  | Node node ->
    (match node.next with
     | Empty -> Some node.inner
     | Node next ->
       let value = node.inner in
       node.prev <- Empty;
       node.inner <- next.inner;
       node.next <- next.next;
       Some value)
;;

let range n =
  let rec go (acc : int t) : int -> int t = function
    | idx when idx = n -> acc
    | idx -> go (acc <-> node idx) (succ idx)
  in
  go Empty 0
;;
