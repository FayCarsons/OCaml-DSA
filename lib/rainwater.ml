(* Taken from Leetcode - 'Trapping rain water' *)

open Core

let trap (heights : int array) =
  let len = Array.length heights in
  let rec loop water = function
    | left when left = len -> water
    | left when heights.(left) = 0 -> loop water (succ left)
    | left ->
      let left_height = heights.(left) in
      let rec valid_container water' = function
        | right when right = len -> None
        | right ->
          let right_height = heights.(right) in
          if right_height >= left_height && right > succ left
          then Some (right, water')
          else if right_height < left_height
          then (
            let contained = water' + (left_height - right_height) in
            valid_container contained (succ right))
          else None
      in
      (match valid_container 0 (succ left) with
       | Some (next_idx, found_water) -> loop (water + found_water) next_idx
       | None -> loop water (succ left))
  in
  loop 0 0
;;

let%test "Case one" = trap [| 0; 1; 0; 2; 1; 0; 1; 3; 2; 1; 2; 1 |] = 6
let%test "Case two" = trap [| 4; 2; 0; 3; 2; 5 |] = 9
let%test "Empty array" = trap [||] = 0
let%test "Single Element" = trap [| 5 |] = 0
let%test "Two Elements" = trap [| 5; 3 |] = 0
let%test "Descending order" = trap [| 5; 4; 3; 2; 1 |] = 0
let%test "Ascending order" = trap [| 1; 2; 3; 4; 5 |] = 0
let%test "Flat" = trap [| 2; 2; 2; 2; 2 |] = 0
let%test "Symmetrical" = trap [| 5; 4; 3; 2; 1; 2; 3; 4; 5 |] = 16
let%test "W-shaped" = trap [| 5; 1; 5; 1; 5 |] = 8
let%test "One large container" = trap [| 5; 1; 2; 1; 3; 1; 2; 1; 5 |] = 24
