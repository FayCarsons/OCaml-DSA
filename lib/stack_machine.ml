let ( let* ) = Result.bind

(*
   Simple postfix binary arithmetic language.
   Example:
   '2 2 Mul 416 Add' = 420
*)
type t =
  | Add
  | Sub
  | Mul
  | Div
  | Push of int

type err_tag =
  | FewerArgs
  | Syntax

type err = err_tag * string

let of_string = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | s ->
    (match int_of_string_opt s with
     | Some n -> Push n
     | None -> failwith "Unexpected character")
;;

let to_string = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Push n -> "Push " ^ string_of_int n
;;

let is_op = function
  | Push _ -> false
  | _ -> true
;;

let ( >> ) f g x = g @@ f x
let print_sep () = print_string ", "

let run s =
  let stack = Stack.create () in
  let instructions = String.split_on_char ' ' s |> List.map of_string in
  print_string "Instructions: [\n ";
  List.iter (to_string >> print_string >> print_sep) instructions;
  print_string "\n]\n";
  let rec go = function
    | [] ->
      Stack.pop_opt stack
      |> Option.to_result ~none:"Program has malformed or too few instructions"
    | Push n :: rest ->
      Stack.push n stack;
      go rest
    | op :: rest ->
      (try
         let left = Stack.pop stack
         and right = Stack.pop stack in
         let* fn =
           match op with
           | Add -> Ok ( + )
           | Sub -> Ok ( - )
           | Mul -> Ok ( * )
           | Div -> Ok ( / )
           | _ -> Error "Unexpected arg"
         in
         Stack.push (fn left right) stack;
         go rest
       with
       | Stack.Empty ->
         Error
           (Printf.sprintf "Syntax error: operator %s expected two args" (to_string op)))
  in
  go instructions
;;

let run_program program =
  match run program with
  | Ok n -> Printf.printf "Returned: %d" n
  | Error reason -> Printf.printf "Program failed\n%s" reason
;;

let%test "Add" =
  let program = "2 2 +" in
  let res = run program in
  res = Ok 4
;;

let%test "Sub" =
  let program = "1 70 -" in
  let res = run program in
  res = Ok 69
;;
