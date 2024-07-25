(* Red-Black tree implementation described in Chris Okasaki's `Purely functional data structures` *)

open! Core

type ('key, 'data) t =
  | Node of color * ('key, 'data) t * 'key * 'data * ('key, 'data) t
  | Empty

and color =
  | Red
  | Black

let member : key:'key -> ('key, 'data) t -> bool =
  fun ~key tree ->
  let rec search = function
    | Node (_, left, key', _, right) ->
      if key = key' then true else if key < key' then search left else search right
    | Empty -> false
  in
  search tree
;;

(*
   Balances the (sub)tree
   If you have a problem with this code take it up with Chris Okasaki.
   Personally, I find it easier to reason about than my prevous implementations
*)
let balance = function
  | Node (Black, Node (Red, Node (Red, a, kx, dx, b), ky, dy, c), kz, dz, d)
  | Node (Black, Node (Red, a, kx, dx, Node (Red, b, ky, dy, c)), kz, dz, d)
  | Node (Black, a, kx, dx, Node (Red, Node (Red, b, ky, dy, c), kz, dz, d))
  | Node (Black, a, kx, dx, Node (Red, b, ky, dy, Node (Red, c, kz, dz, d))) ->
    Node (Red, Node (Black, a, kx, dx, b), ky, dy, Node (Black, c, kz, dz, d))
  | t -> t
;;

let curry f (a, b) = f a b

let invariants self =
  let rec valid = function
    | Node (Red, Node (Red, _, _, _, _), _, _, _)
    | Node (Red, _, _, _, Node (Red, _, _, _, _)) -> None
    | Node (color, left, _, _, right) ->
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
  | Node (Black, _, _, _, _) -> Option.is_some @@ valid self
  | _ -> false
;;

let insert
  : ?compare:('key -> 'key -> int) -> ('key, 'data) t -> 'key -> 'data -> ('key, 'data) t
  =
  fun ?(compare = Poly.compare) self key data ->
  let rec insert' = function
    | Node (color, left, key', data', right) ->
      (match compare key key' with
       | 1 -> balance @@ Node (color, left, key', data', insert' right)
       | 0 -> Node (color, left, key, data, right)
       | -1 -> balance @@ Node (color, insert' left, key, data, right)
       | _ -> assert false)
    | Empty -> Node (Red, Empty, key, data, Empty)
  in
  match insert' self with
  | Node (_, left, key, data, right) -> Node (Black, left, key, data, right)
  (* Unreachable - the result of this operation cannot be empty *)
  | _ -> assert false
;;

let find : ?compare:('key -> 'key -> int) -> ('key, 'data) t -> key:'key -> 'data option =
  fun ?(compare = Poly.compare) self ~key ->
  let rec find' = function
    | Node (_, left, key', data, right) ->
      if key = key'
      then Some data
      else if compare key key' = -1
      then find' left
      else find' right
    | Empty -> None
  in
  find' self
;;
