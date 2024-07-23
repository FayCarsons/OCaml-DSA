open! Core

(* Given an array where every ith element represent a house i with an 'energy potenial' p,
   choose the two most ideal houses to install solar panels with the resstriction that they may not be neighbors *)

let place_solar_panels houses =
  let len = Array.length houses in
  let rec go idx =
    if idx >= len then 0 else max (houses.(idx) + go (idx + 2)) (go @@ succ idx)
  in
  go 0
;;

let%test "Solar - Case one" = place_solar_panels [| 3; 10; 3; 1; 2 |] = 12
let%test "Solar - Case two" = place_solar_panels [| 5; 1; 1; 5 |] = 10
let%test "Solar - Case three" = place_solar_panels [| 3; 2; 4; 6; 7 |] = 14
let%test "Solar - Case four" = place_solar_panels [| 1; 20; 3; 4; 5; 6 |] = 30

(*
   Description:
   You're an agricultural scientist working on optimizing crop rotation for a
   farm. The farm has a single field that can be used for N consecutive years.
   Each year, you must choose one crop to plant.

   Different crops have different effects on the soil, and the yield of a crop
   depends on what was planted in the previous year. You have data on the yield
   of each crop based on what was planted the previous year. Your goal is to
   determine the optimal sequence of crops to maximize the total yield over N years.

   Input:

   An integer N representing the number of years
   A 2D array yields where yields[i][j] represents the
   yield of crop j if crop i was planted in the previous year.
   The first row (i=0) represents the yields if it's the first crop planted.
*)

let max_yield =
  Array.max_elt ~compare:(fun (yield_a, _) (yield_b, _) -> Int.compare yield_a yield_b)
;;

let plant_crops years yields =
  (* Array of crops without year zero yields *)
  let only_crops =
    Array.init (pred @@ Array.length yields) ~f:(fun idx -> yields.(succ idx))
  in
  let rec go total_yield crops year =
    if year = years (* Base case - we've finished *)
    then total_yield, crops (* Not finished, find the next crop *)
    else (
      (* Theres always a last crop so we can throw here if this pattern matching fails *)
      let[@warning "-8"] (last_crop :: _) = crops in
      (* So the potential yields this year are the yields after plantting last_crop *)
      let potential_yields = only_crops.(last_crop) in
      (* Map over those yields with the index(crop) and recurse on each, taking the max *)
      Array.mapi
        ~f:(fun idx yield -> go (yield + total_yield) (idx :: crops) (succ year))
        potential_yields
      |> max_yield
      |> Option.value_exn)
  in
  (* Compute our initial state, year 0 *)
  let first_yield, first_crop =
    Array.mapi ~f:(fun idx yield -> yield, idx) yields.(0)
    |> max_yield
    |> Option.value_exn
  in
  let yield, crops = go first_yield [ first_crop ] 1 in
  yield, List.rev crops
;;

let%test "Crops - Case one" =
  let n = 4
  and yields =
    [| (* Columns are crops *)
       (* Yields on year zero *)
       [| 3; 4; 5 |]
     ; [| 2; 3; 4 |]
     ; [| 4; 3; 2 |]
     ; [| 3; 4; 5 |]
    |]
  in
  let yield, crops = plant_crops n yields in
  yield = 20 && List.compare Int.compare crops [ 2; 2; 2; 2 ] = 0
;;

let%test "Crops - case two" =
  let n = 5
  and yields = [| [| 3; 2; 4 |]; [| 2; 5; 3 |]; [| 4; 1; 3 |]; [| 3; 4; 2 |] |] in
  let yield, crops = plant_crops n yields in
  yield = 21 && List.compare Int.compare crops [ 2; 1; 0; 1; 0 ] = 0
;;
