let longest_palindrome str : string option =
  let length = String.length str in
  let is_palindrome left right =
    let mid = (left + right) / 2 in
    let rec go = function
      | left, right when left = mid -> right = mid
      | left, right ->
        if str.[left] = str.[right] then go (succ left, pred right) else false
    in
    go (left, right)
  in
  let find_first_match left =
    let rec go right =
      if right < pred length
      then if str.[left] = str.[right] then Some right else go (succ right)
      else None
    in
    go left
  in
  let rec go acc left =
    if left < pred length
    then (
      match find_first_match left with
      | Some right ->
        if is_palindrome left right
        then (
          let substr = String.sub str left right in
          go (substr :: acc) (succ left))
        else go acc (succ left)
      | None -> go acc (succ left))
    else acc
  in
  match go [] 0 with
  | [] -> None
  | palindromes ->
    (try
       List.sort (fun a b -> -1 * compare (String.length a) (String.length b)) palindromes
       |> List.hd
       |> Option.some
     with
     | _ -> None)
;;
