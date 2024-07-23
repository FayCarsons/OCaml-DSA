let repeat_string n string =
  let rec go acc = function
    | 0 -> acc
    | n -> go (acc ^ string) (pred n)
  in
  go "" n
;;

let greatest_common_prefix : string -> string -> string option =
  fun left right ->
  let left_len = String.length left in
  let right_len = String.length right in
  let len = min left_len right_len in
  let rec go = function
    | 0 -> None
    | window ->
      if left_len mod window != 0 || right_len mod window != 0
      then go (pred window)
      else (
        let candidate = String.sub left 0 window in
        if repeat_string (left_len / window) candidate = left
           && repeat_string (right_len / window) candidate = right
        then Some candidate
        else go (pred window))
  in
  go len
;;

let%test "ABC" =
  print_endline "GCD CASE ONE SUCCESS";
  let left = "ABCABC"
  and right = "ABC" in
  greatest_common_prefix left right = Some "ABC"
;;

let%test "AB" =
  print_endline "GCD CASE TWO SUCCESS";
  let left = "ABABAB"
  and right = "ABAB" in
  greatest_common_prefix left right = Some "AB"
;;

let%test "Failure case" =
  print_endline "GCD CASE THREE SUCCESS";
  let left = "O"
  and right = "CAML" in
  greatest_common_prefix left right |> Option.is_none
;;
