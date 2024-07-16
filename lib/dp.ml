open! Core

(* Given an array where every ith element represent a house i with an 'energy potenial' p,
   choose the two most ideal houses to install solar panels with the resstriction that they may not be neighbors *)

(*
   let place_solar_panels houses =
   let len = Array.length houses in
   let dp = Array.make (len + 2) 0 in
   for i = pred len downto 0 do
   dp.(i) <- max (houses.(i) + dp.(i + 2)) dp.(i + 1)
   done;
   dp.(0)
   ;;
*)

let place_solar_panels houses =
  let len = Array.length houses in
  let rec go idx =
    if idx >= len then 0 else max (houses.(idx) + go (idx + 2)) (go @@ succ idx)
  in
  go 0
;;

let%test "Case one" = place_solar_panels [| 3; 10; 3; 1; 2 |] = 12
let%test "Case two" = place_solar_panels [| 5; 1; 1; 5 |] = 10
let%test "Case three" = place_solar_panels [| 3; 2; 4; 6; 7 |] = 14
let%test "Case four" = place_solar_panels [| 1; 20; 3; 4; 5; 6 |] = 30
