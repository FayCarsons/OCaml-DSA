open Core
module Table = Hashtbl.Make (Int)

let map_or default f = function
  | Some x -> Some (f x)
  | None -> Some default
;;

module TopDown = struct
  let coin_change (coins : int array) (target : int) : int option =
    Array.sort ~compare:Int.compare coins;
    let coins_available = Array.length coins in
    let cache = Table.create ~size:target () in
    let rec dfs remaining =
      if remaining < 0
      then None
      else if remaining = 0
      then Some 0
      else (
        match Hashtbl.find cache remaining with
        | Some num_coins -> num_coins
        | None ->
          let rec try_coins idx best =
            if idx = coins_available
            then best
            else (
              let coin = coins.(idx) in
              match dfs (remaining - coin) with
              | Some child ->
                let child = succ child in
                let best = map_or child (min child) best in
                try_coins (succ idx) best
              | None -> best)
          in
          let result = try_coins 0 None in
          Hashtbl.add_exn cache ~key:remaining ~data:result;
          result)
    in
    dfs target
  ;;

  let%test "TopDown - Simple" = Util.is_some_and (( = ) 3) (coin_change [| 1; 2; 5 |] 11)
  let%test "TopDown - One Coin" = Option.is_none @@ coin_change [| 2 |] 3
  let%test "TopDown - Target = 0" = Util.is_some_and (( = ) 0) @@ coin_change [| 1 |] 0
  let%test "TopDown - All 1s" = Util.is_some_and (( = ) 1) @@ coin_change [| 1 |] 1
  let%test "TopDown - 100" = Util.is_some_and (( = ) 20) @@ coin_change [| 1; 2; 5 |] 100

  let%test "TopDown - Big numbers" =
    Util.is_some_and (( = ) 20) @@ coin_change [| 186; 419; 83; 408 |] 6249
  ;;
end

module BottomUp = struct
  let coin_change (coins : int array) (target : int) : int option =
    Array.sort ~compare:Int.compare coins;
    let max_sum = succ target in
    let dp = Array.create ~len:max_sum max_sum in
    dp.(0) <- 0;
    let use_coin current_target coin =
      if current_target - coin >= 0 && dp.(current_target - coin) <= target
      then
        dp.(current_target) <- min dp.(current_target) (succ dp.(current_target - coin))
    in
    let rec loop current_target =
      if current_target = max_sum
      then ()
      else (
        Array.iter ~f:(use_coin current_target) coins;
        loop (succ current_target))
    in
    loop 1;
    let answer = dp.(target) in
    if answer = max_sum then None else Some dp.(target)
  ;;

  let%test "Bottom up - Simple" =
    Util.is_some_and (( = ) 3) (coin_change [| 1; 2; 5 |] 11)
  ;;

  let%test "Bottom up - One Coin" = Option.is_none @@ coin_change [| 2 |] 3
  let%test "Bottom up - Target = 0" = Util.is_some_and (( = ) 0) @@ coin_change [| 1 |] 0
  let%test "Bottom up - All 1s" = Util.is_some_and (( = ) 1) @@ coin_change [| 1 |] 1

  let%test "Bottom up - 100" =
    Util.is_some_and (( = ) 20) @@ coin_change [| 1; 2; 5 |] 100
  ;;

  let%test "Bottom up - Big numbers" =
    Util.is_some_and (( = ) 20) @@ coin_change [| 186; 419; 83; 408 |] 6249
  ;;
end
