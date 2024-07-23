[@@@ocaml.warning "-32-34"]

type square =
  | PlayerOne
  | PlayerTwo
  | Empty

let string_of_player = function
  | PlayerOne -> "Player one"
  | PlayerTwo -> "Player two"
  | Empty -> ""
;;

type player = (int * int) list

type game =
  { board : square array array
  ; mutable player : square
  ; mutable turn : int
  }

let columns = 7
and rows = 6

let ( >>= ) = Result.bind
let create_board () = Array.init rows (fun _ -> Array.make columns Empty)
let create_game () = { board = create_board (); player = PlayerOne; turn = 0 }

(* TODO: use a polymorphic variant here or smth *)
let flip_player = function
  | PlayerOne -> PlayerTwo
  | PlayerTwo -> PlayerOne
  | _ -> failwith "Invalid argument passed to 'flip_player'"
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

let whose_turn turn = if turn mod 2 = 0 then PlayerOne else PlayerTwo
let valid_index x y = x >= 0 && x < rows && y >= 0 && y < columns

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

let read_move player =
  Printf.printf "%s enter your move: " player;
  (* TODO : refactor entire fileto use core *)
  let open Core in
  In_channel.(input_line_exn stdin)
  |> String.strip
  |> int_of_string_opt
  |> Option.bind ~f:(function
    | n when n >= 1 && n <= columns -> Some n
    | _ -> None)
  |> Result.of_option
       ~error:
         "A move must be one integer - the column (1 ..  =7) in which you wish to place \
          your piece"
;;

let get_opt array (row, col) =
  if row >= 0 && row < rows && col >= 0 && col < columns
  then Some array.(row).(col)
  else None
;;

let directions =
  [ (* Left *)
    -1, 0
  ; (* Left-up *)
    -1, 1
  ; (* Up *)
    0, 1
  ; (* Up-right *)
    1, 1
  ; (* Right *)
    1, 0
  ; (* Right down *)
    1, -1
  ; (* Down *)
    0, -1
  ; (* Down-left *)
    -1, -1
  ]
;;

let run board pos direction =
  let dx, dy = direction in
  let rec go acc (x, y) = function
    | 0 -> acc
    | n ->
      let x, y = x + dx, y + dy in
      (match get_opt board (x, y) with
       | Some piece -> go (piece :: acc) (x, y) (pred n)
       | None -> acc)
  in
  go [] pos 3
;;

let count needle list =
  let rec count n = function
    | x :: xs when x = needle -> count (succ n) xs
    | _ :: pieces -> count n pieces
    | [] -> n
  in
  count 0 list
;;

let has_won board player move =
  let row, col = move in
  let open Util in
  let neighbors = List.map (run board (row, col) >> count player) directions in
  List.exists (( = ) 3) neighbors
;;

type role =
  | Min
  | Max

type tree =
  | Node of tree list
  | Leaf

(*
   let minimax board : (int * int, int) Either.t =
   let t = Leaf in
   let rec go role depth (alpha, beta) = function
   | Node _ when depth = 0 -> (* score_position *) None
   | Node children ->
   if role = Max
   then (
   let rec reduce (alpha, value) = function
   | child :: children ->
   let value = max value (go Min (pred depth) (alpha, beta) child)
   and alpha = max alpha value in
   if alpha >= beta then value else reduce (alpha, value) children
   | [] -> value
   in
   reduce (alpha, Float.neg_infinity) children)
   else Util.unimplemented ()
   | Leaf -> Util.unimplemented () (* Return heuristic value of the node *)
   in
   ;;
*)

let computer_player_move _board = Util.unimplemented ()

let computer_game () =
  let rec loop ({ board; turn; _ } as state) =
    if turn = rows * columns
    then print_endline "It's a tie!"
    else (
      let is_ai_turn = turn mod 2 = 1 in
      if is_ai_turn
      then Util.unimplemented ()
      else (
        match read_move "" >>= place_first_row board PlayerOne with
        | Ok _move ->
          print_board board;
          loop { state with turn = succ turn }
        | Error e ->
          print_endline e;
          loop state))
  in
  loop @@ create_game ()
;;

let game () =
  let rec loop ({ board; player; turn } as state) =
    if turn = rows * columns
    then print_string "It's a tie!"
    else (
      state.player <- flip_player player;
      let handle_move () =
        let player_str = string_of_player state.player in
        match read_move player_str >>= place_first_row board state.player with
        | Ok move ->
          print_board board;
          if has_won board state.player move
          then print_endline @@ string_of_player state.player ^ " has won!"
          else state.turn <- succ turn;
          loop state
        | Error e ->
          print_endline e;
          loop state
      in
      handle_move ())
  in
  loop @@ create_game ()
;;

let run () =
  match Sys.argv with
  | [| _; "--computer" |] -> computer_game ()
  | args when Array.mem "--help" args ->
    print_string
      "USAGE: add flag `--computer` to play 'AI' opponent, otherwise its two player :3"
  | _ -> game ()
;;
