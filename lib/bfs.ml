type t =
  | Land
  | Water
  | Chest
  | Found of int

let ( >>= ) = Option.bind

let to_string = function
  | Land -> "Land"
  | Water -> "Water"
  | Chest -> "Chest"
  | Found distance -> "Found: " ^ string_of_int distance
;;

let get_opt (grid : t array array) ((x, y) : int * int) =
  let row_len = Array.length grid
  and col_len = Array.length grid.(0) in
  if x >= 0 && x < row_len && y >= 0 && y < col_len then Some grid.(x).(y) else None
;;

let get_neighbors (row, col) = [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]

let _compare_map a b =
  match a, b with
  | Some lhs, Some rhs -> Int.compare lhs rhs
  | None, _ -> -1
  | _, None -> 1
;;

let bfs (grid : t array array) (coord : int * int) : int option =
  let queue = Queue.create () in
  let rec go () : int option =
    if Queue.is_empty queue
    then None
    else (
      let depth, coord = Queue.take queue in
      match get_opt grid coord with
      | Some space ->
        (match space with
         | Chest -> Some depth
         | Water -> go ()
         | Land | Found _ ->
           List.iter
             (fun neighbor -> Queue.push (succ depth, neighbor) queue)
             (get_neighbors coord);
           go ())
      | None -> go ())
  in
  Queue.push (0, coord) queue;
  go ()
;;

let find_distances grid =
  Array.iteri
    (fun row_index row ->
      Array.iteri
        (fun column_index _ ->
          match grid.(row_index).(column_index) with
          | Land ->
            bfs grid (row_index, column_index)
            |> Option.iter (fun depth -> grid.(row_index).(column_index) <- Found depth)
          | _ -> ())
        row)
    grid;
  grid
;;

let print_grid grid =
  print_newline ();
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      let cell = grid.(i).(j) in
      print_string @@ to_string cell ^ "; "
    done;
    print_newline ()
  done
;;

(* -1 water
   0 Treasure chest
   n Land *)

let test_grid =
  [| [| Land; Water; Chest; Land |]
   ; [| Land; Land; Land; Water |]
   ; [| Land; Water; Land; Water |]
   ; [| Chest; Water; Land; Land |]
  |]
;;

let%test "Case one" =
  let expected =
    [| [| Found 3; Water; Chest; Found 1 |]
     ; [| Found 2; Found 2; Found 1; Water |]
     ; [| Found 1; Water; Found 2; Water |]
     ; [| Chest; Water; Found 3; Found 4 |]
    |]
  in
  print_endline "BFS - CASE ONE SUCCESSFULL";
  find_distances test_grid = expected
;;

let%test "Case two" =
  let grid = [| [| Chest; Water |]; [| Land; Land |] |] in
  let expected = [| [| Chest; Water |]; [| Found 1; Found 2 |] |] in
  find_distances grid = expected
;;
