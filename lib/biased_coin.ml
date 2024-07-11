type coin =
  | Heads
  | Tails

let flip_coin probability = if Random.float 1. > probability then Tails else Heads

(*
   This works because the probability of getting H,T is p * (1 - p) and the
   probability of getting T,H is (1 - p) * p, cancelling out the bias in both cases.
   If we instead encounter a case where both flips land on the same side, we discard it and try again
*)
let unbias probability =
  let rec go () =
    let two_flips = flip_coin probability, flip_coin probability in
    match two_flips with
    | Heads, Heads | Tails, Tails -> go ()
    | Heads, _ -> Heads
    | Tails, _ -> Tails
  in
  go ()
;;

let chi_squared ~observed:(oh, ot) ~expected:(eh, et) =
  let oh, ot = float_of_int oh, float_of_int ot in
  let eh, et = float_of_int eh, float_of_int et in
  (((oh -. eh) ** 2.) /. eh) +. (((ot -. et) ** 2.) /. et)
;;

let chi_squared_p_value chi_squared df =
  let rec gamma x = if x < 0.001 then 1.0 /. x else (x -. 1.0) *. gamma (x -. 1.0) in
  let gamma_inc a x =
    let rec aux a x n sum term =
      let term = term *. x /. (a +. float_of_int n) in
      if term < 1e-8 then sum else aux a x (n + 1) (sum +. term) term
    in
    aux a x 0 0.0 (exp ((a *. log x) -. x -. log (gamma a)))
  in
  1.0 -. gamma_inc (float_of_int df /. 2.0) (chi_squared /. 2.0)
;;

let%test "Monte Carlo" =
  let p = 0.33 in
  let rec do_flips (h, t) = function
    | 0 -> h, t
    | n ->
      let flip = unbias p in
      if flip = Heads
      then do_flips (succ h, t) (pred n)
      else do_flips (h, succ t) (pred n)
  in
  let n = 1000 in
  let flips = do_flips (0, 0) n in
  let chi_squared_stat = chi_squared ~observed:flips ~expected:(n / 2, n / 2) in
  let dof = 1 in
  let p_value = chi_squared_p_value chi_squared_stat dof in
  p_value > 0.05
;;
