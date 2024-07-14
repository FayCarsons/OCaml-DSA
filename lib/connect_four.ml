type square =
  | PlayerOne
  | PlayerTwo
  | Empty

type player = (int * int) list

type game =
  { board : square array array
  ; player_one : player
  ; player_two : player
  ; turn : int
  }

let columns = 7
and rows = 6

let ( >>= ) = Result.bind
let create_board () = Array.init rows (fun _ -> Array.init columns (Fun.const Empty))

let create_game () =
  { board = create_board (); player_one = []; player_two = []; turn = 0 }
;;

let print_board board =
  let print_side () =
    for _ = 0 to columns do
      print_string " - "
    done;
    print_newline ()
  in
  print_side ();
  Array.iter
    (fun row ->
      print_string "| ";
      Array.iter
        (function
          | PlayerOne -> print_string "🔵 "
          | PlayerTwo -> print_string "🔴 "
          | Empty -> print_string "⚪️ ")
        row;
      print_string "|\n")
    board;
  print_side ()
;;

module Map = Map.Make (Int)

let or_default map k ~default f =
  match Map.find_opt k map with
  | Some value -> Map.add k (f value) map
  | None -> Map.add k (f default) map
;;

let has_four_consecutive list =
  let rec go count last = function
    | _ when count = 4 -> true
    | x :: xs when x = succ last -> go (succ count) x xs
    | x :: xs -> go 1 x xs
    | [] -> false
  in
  match list with
  | x :: xs -> go 1 x xs
  | [] -> false
;;

let has_winner map = Map.exists (fun _ squares -> has_four_consecutive squares) map

let has_won player =
  let rec populate (rows_map, cols_map) = function
    | (row, col) :: xs ->
      let updated_rows = or_default rows_map row ~default:[] (List.cons col) in
      let updated_cols = or_default cols_map col ~default:[] (List.cons row) in
      populate (updated_rows, updated_cols) xs
    | [] -> rows_map, cols_map
  in
  let rows, cols = populate (Map.empty, Map.empty) player in
  has_winner rows || has_winner cols
;;

let check_won player_one player_two =
  if has_won player_one
  then Some "Player one has won!"
  else if has_won player_two
  then Some "Player two has won!"
  else None
;;

let whose_turn turn = if turn mod 2 = 0 then PlayerOne else PlayerTwo

(* Zero is in top left *)
let place_first_row board player col =
  (* Convert from user's 1-indexing to 0-indexing *)
  let col = pred col in
  let rec go row =
    if row < 0
    then Error (Printf.sprintf "Column %d is full!" col)
    else (
      match board.(row).(col) with
      | Empty ->
        board.(row).(col) <- player;
        Ok (col, row)
      | _ -> go (pred row))
  in
  go (pred rows)
;;

(* Sometimes reading code *should* be hard *)
let read_move player =
  Printf.printf "Player %s enter your move: " player
  |> read_line
  |> String.trim
  |> int_of_string_opt
  |> Fun.flip Option.bind (function
    | n when n >= 1 && n <= columns -> Some n
    | _ -> None)
  |> Option.to_result
       ~none:
         "A move must be one integer - the column (1 ..  =7) in which you wish to place \
          your piece"
;;

let game () =
  let rec update ({ board; player_one; player_two; turn } as state) =
    if turn = rows * columns
    then print_string "It's a tie!"
    else (
      match check_won player_one player_two with
      | Some message -> print_string message
      | None ->
        let current_player = whose_turn turn in
        let handle_move player =
          let msg = if player = PlayerOne then "one" else "two" in
          match read_move msg >>= place_first_row board current_player with
          | Ok move ->
            print_board board;
            if player = PlayerOne
            then update { state with player_one = move :: player_one; turn = succ turn }
            else update { state with player_two = move :: player_two; turn = succ turn }
          | Error e ->
            print_endline e;
            update state
        in
        handle_move current_player)
  in
  update @@ create_game ()
;;
