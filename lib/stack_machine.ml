module Sum_types = struct
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
    | Add -> "ADD"
    | Sub -> "SUB"
    | Mul -> "MUL"
    | Div -> "DIV"
    | Push n -> "PUSH " ^ string_of_int n
  ;;

  let rec parse acc = function
    | [] -> List.rev acc
    | "ADD" :: xs -> parse (Add :: acc) xs
    | "SUB" :: xs -> parse (Sub :: acc) xs
    | "MUL" :: xs -> parse (Mul :: acc) xs
    | "DIV" :: xs -> parse (Div :: acc) xs
    | "PUSH" :: n :: xs -> parse (Push (int_of_string n) :: acc) xs
    | str :: _ -> raise @@ Failure (Printf.sprintf "Unknown operator: %s" str)
  ;;

  let run s =
    let stack = Stack.create () in
    let instructions = String.split_on_char ' ' s |> parse [] in
    let rec go = function
      | [] -> Stack.pop_opt stack |> Option.to_result ~none:"Stack underflow"
      | Push n :: rest ->
        Stack.push n stack;
        go rest
      | op :: rest ->
        (try
           let left = Stack.pop stack
           and right = Stack.pop stack in
           if op = Div && right = 0
           then Error "Division by zero"
           else
             let* fn =
               match op with
               | Add -> Ok ( + )
               | Sub -> Ok ( - )
               | Mul -> Ok ( * )
               | Div -> Ok ( / )
               | _ ->
                 Result.error @@ "Unexpected arg in binary op branch: " ^ to_string op
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
    let program = "PUSH 2 PUSH 2 ADD" in
    let res = run program in
    res = Ok 4
  ;;

  let%test "Sub" =
    let program = "PUSH 1 PUSH 70 SUB" in
    let res = run program in
    res = Ok 69
  ;;

  let%test "Mul" =
    let program = "PUSH 4 PUSH 4 MUL" in
    let res = run program in
    res = Ok 16
  ;;

  let%test "Div" =
    let program = "PUSH 2 PUSH 128 DIV" in
    let res = run program in
    res = Ok 64
  ;;

  let%test "Div by zero" =
    let program = "PUSH 0 PUSH 2 DIV" in
    let res = run program in
    res = Error "Division by zero"
  ;;
end
