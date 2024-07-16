(* Given a list of events `('start * 'end) list`, a timeline from zero to `t`, and an integer `k`,
   determine the ordering of events that produces the largest uninterrupted block of free-time
   between events by rescheduling only `k` events *)

open! Core

module Range = struct
  (* Tuple struct so we have type checking *)
  type t = Range of int * int

  let in_range n (Range (lo, hi)) = n >= lo && n <= hi
  let ( $> ) = in_range

  let compare (Range (lo_1, hi_1)) (Range (lo_2, hi_2)) =
    let first = hi_1 - lo_1
    and second = hi_2 - lo_2 in
    Int.compare first second
  ;;

  let max a b =
    match[@warning "-8"] compare a b with
    | 0 | 1 -> a
    | -1 -> b
  ;;
end

let sort_schedule =
  let open Range in
  Array.sort ~compare:(fun (Range (lo_1, _)) (Range (lo_2, _)) -> Int.compare lo_1 lo_2)
;;

let largest_free : int -> Range.t list -> Range.t option =
  fun t schedule ->
  let open Range in
  let rec go best = function
    | x :: y :: _ as rest ->
      let (Range (_, first_end)) = x
      and (Range (second_start, _)) = y in
      let difference = Range (first_end, second_start) in
      go (Range.max difference best) rest
    | Range (_, hi) :: [] -> Range.max best (Range (hi, t))
    | [] -> best
  in
  match schedule with
  | first :: _ as events -> Option.some @@ go first events
  | [] -> None
;;

let get_free_blocks : int -> Range.t list -> Range.t list =
  fun t schedule ->
  let open Range in
  let rec go acc = function
    | Range (_, first_end) :: Range (second_start, _) :: _ as rest ->
      go (Range (first_end, second_start) :: acc) rest
    | Range (_, last_end) :: [] -> List.rev @@ (Range (last_end, t) :: acc)
    | [] -> List.rev acc
  in
  go [] schedule
;;
