module Naive = struct
  let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
end

module Memoized = struct
  let fib n =
    let memo = Hashtbl.create n in
    let rec fib' n =
      if n < 2
      then n
      else (
        match Hashtbl.find_opt memo n with
        | Some value -> value
        | None ->
          let result = fib' (n - 1) + fib' (n - 2) in
          let _ = Hashtbl.add memo n result in
          result)
    in
    fib' n
  ;;
end

module TCO = struct
  let fib n =
    if n < 2
    then n
    else (
      let rec loop (pred, pred2) = function
        | idx when idx = n -> pred + pred2
        | idx -> loop (pred2, pred + pred2) (succ idx)
      in
      loop (0, 1) 2)
  ;;
end

let%test "N = 8" =
  let n = 8 in
  let naive = Naive.fib n
  and memoized = Memoized.fib n
  and opt = TCO.fib n in
  naive = 21 && memoized = 21 && opt = 21
;;
