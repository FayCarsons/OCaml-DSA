type 'a t =
  { mutable prev : 'a t option
  ; mutable inner : 'a
  ; mutable next : 'a t option
  }

let ( <-> ) left right =
  left.next <- Some right;
  right.prev <- Some left;
  left
;;

let return inner = { prev = None; inner; next = None }

let rec map f node =
  node.inner <- f node.inner;
  Option.iter (map f) node.next
;;

let dequeue node =
  match node.next with
  | Some next ->
    let value = node.inner in
    node.prev <- next.prev;
    node.inner <- next.inner;
    node.next <- next.next;
    value
  | None -> node.inner
;;

let test_list = return 0 <-> return 1 <-> return 2 <-> return 3
