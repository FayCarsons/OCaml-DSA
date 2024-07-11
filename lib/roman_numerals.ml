let is_subtraction = function
  | 'I', 'V' | 'I', 'X' | 'X', 'L' | 'X', 'C' | 'C', 'D' | 'C', 'M' -> true
  | _ -> false
;;

let to_int = function
  | 'I' -> 1
  | 'V' -> 5
  | 'X' -> 10
  | 'L' -> 50
  | 'C' -> 100
  | 'D' -> 500
  | 'M' -> 1000
  | _ -> failwith "Invalid roman numeral"
;;

let parse_roman_numeral : string -> int =
  fun str ->
  let characters = List.init (String.length str) (fun idx -> str.[idx]) in
  let rec go acc = function
    | x :: y :: xs when is_subtraction (x, y) ->
      let rhs = to_int x in
      let lhs = to_int y in
      let digit = lhs - rhs in
      go (acc + digit) xs
    | x :: xs ->
      let digit = to_int x in
      go (acc + digit) xs
    | [] -> acc
  in
  go 0 characters
;;
