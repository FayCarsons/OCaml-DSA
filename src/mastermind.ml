type color =
  | Blue
  | Green
  | Red
  | Purple
  | Yellow

type marker =
  | Black (** Hit *)
  | White (** Wrong index *)

type turn = color array
type response = marker option array

type game =
  { code : turn
  ; guesses : (turn * response) option array
  ; turns : int
  }

let num_turns = 5
let guess_len = 4

exception Guess of string

let of_int = function
  | 0 -> Blue
  | 1 -> Green
  | 2 -> Red
  | 3 -> Purple
  | 4 -> Yellow
  | n -> failwith @@ Printf.sprintf "Integer %d is not a member of the color enum" n
;;

let color_of_string = function
  | "blue" -> Blue
  | "green" -> Green
  | "red" -> Red
  | "purple" -> Purple
  | "yellow" -> Yellow
  | s -> raise @@ Guess (Printf.sprintf "%s is not a valid color!" s)
;;

let color_to_string = function
  | Blue -> "Blue"
  | Green -> "Green"
  | Red -> "Red"
  | Purple -> "Purple"
  | Yellow -> "Yellow"
;;

let marker_to_string = function
  | Black -> "Black"
  | White -> "White"
;;

let create_code () =
  let code = Array.make 4 Blue in
  let rec go = function
    | 0 -> code
    | n ->
      let random_color = of_int @@ Random.int 5 in
      code.(pred n) <- random_color;
      go (pred n)
  in
  go 4
;;

let print_game guesses =
  Array.iteri
    (fun guess_index -> function
      | Some (turn, markers) ->
        let[@warning "-8"] [| one; two; three; four |] = Array.map color_to_string turn in
        Printf.printf "Turn %d: [ %s; %s; %s; %s ]" guess_index one two three four;
        let response_strings =
          Array.fold_left
            (fun acc -> function
              | Some marker -> marker_to_string marker :: acc
              | None -> acc)
            []
            markers
          |> List.rev
        in
        Printf.printf "Response: [ ";
        List.iter (fun s -> Printf.printf "%s; " s) response_strings;
        print_endline "]"
      | None -> ())
    guesses
;;

let parse_guess guess_string =
  let colors = String.split_on_char ' ' guess_string |> Array.of_list in
  if Array.length colors != guess_len
  then Error (Printf.sprintf "A guess must contain %d colors" guess_len)
  else
    let open Util in
    (* Lowercase the color strings so we dont have to worry about player capialization mistakes *)
    try Result.ok @@ Array.map (String.lowercase_ascii >> color_of_string) colors with
    | Guess reason -> Error reason
    | exn -> Error (Printf.sprintf "Unkown error: %s" @@ Printexc.to_string exn)
;;

let check_guess code guess =
  let response = Array.make guess_len None in
  let rec go = function
    | n when n = guess_len -> response
    | n ->
      let guessed_color = guess.(n) in
      if code.(n) = guessed_color
      then response.(n) <- Some Black
      else if Array.mem guessed_color code
      then response.(n) <- Some White;
      go @@ succ n
  in
  go 0
;;

let rec update ({ guesses; turns; code } as game) =
  print_game guesses;
  let current_turn = succ turns in
  if current_turn < num_turns
  then (
    Printf.printf "Enter guess #%d:" current_turn;
    match read_line () |> parse_guess with
    | Ok guess ->
      let response = check_guess code guess in
      if response = [| Some Black; Some Black; Some Black; Some Black |]
      then print_endline "You win!!"
      else (
        guesses.(pred current_turn) <- Some (guess, response);
        update { game with turns = current_turn })
    | Error e ->
      print_endline e;
      update game)
  else print_endline "You are not the mastermind :/"
;;

let init () =
  let guesses = Array.make num_turns None in
  let turns = 1 in
  let code = create_code () in
  update { guesses; turns; code }
;;
