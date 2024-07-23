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

let rec length' accum : 'a t -> int = function
  | Node { next; _ } -> length' (succ accum) next
  | Empty -> accum
;;

let length (list : 'a t) = length' 0 list

let append left right =
  match left, right with
  | Empty, Empty -> Empty
  | Empty, _ -> right
  | node, Empty -> node
  | Node left, Node right ->
    let rec walk_left = function
      | { next = Empty; _ } as node -> node
      | { next = Node next_node; _ } -> walk_left next_node
    in
    let last_of_left = walk_left left in
    last_of_left.next <- Node right;
    right.prev <- Node last_of_left;
    Node left
;;

let ( <-> ) = append

(** Given a pointer to a node somewhere in the list,
    remove neighboring references to that node *)
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

let copy list =
  let rec go copy = function
    | Node current -> go (copy <-> Node current) current.next
    | Empty -> copy
  in
  go Empty list
;;

let nth index list =
  let rec go idx = function
    | Empty -> None
    | Node node when idx = index -> Some node.inner
    | Node node -> go (succ idx) node.next
  in
  go 0 list
;;

let nth_node index list =
  let rec loop idx = function
    | Empty -> None
    | Node node when idx = index -> Some (Node node)
    | Node node -> loop (succ idx) node.next
  in
  loop 0 list
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

let rec map_inplace ~(f : 'a -> 'a) = function
  | Empty -> ()
  | Node node ->
    node.inner <- f node.inner;
    map_inplace ~f node.next
;;

let map ~f list =
  let rec go accum last_in_accum = function
    | Node node ->
      let new_node = Node { prev = last_in_accum; inner = f node.inner; next = Empty } in
      let tail =
        match last_in_accum with
        | Empty -> new_node
        | Node t ->
          t.next <- new_node;
          new_node
      in
      go accum tail node.next
    | Empty -> accum
  in
  let head = Empty in
  go head head list
;;

let filter ~(predicate : 'a -> 'b) list =
  let rec go acc = function
    | Node node ->
      if predicate node.inner then go (acc <-> Node node) node.next else go acc node.next
    | Empty -> acc
  in
  go Empty list
;;

let rec filter_inplace ~predicate = function
  | Node { prev; inner; next } ->
    if predicate inner
    then filter_inplace ~predicate next
    else (
      prev <-> next |> ignore;
      filter_inplace ~predicate next)
  | Empty -> ()
;;

let rec fold_left ~f ~init = function
  | Node node -> fold_left ~f ~init:(f init node.inner) node.next
  | Empty -> init
;;

let rec find ~predicate = function
  | Node { inner; next; _ } as node ->
    if predicate inner then Some node else find ~predicate next
  | Empty -> None
;;

(** Create an array where each index holds the corresponding list node. *)
let pointer_array list =
  let len = length' 0 list in
  let array = Array.make len Empty in
  let rec fill i = function
    | Node node ->
      Array.unsafe_set array i (Node node);
      fill (succ i) node.next
    | Empty -> ()
  in
  fill 0 list;
  array
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
