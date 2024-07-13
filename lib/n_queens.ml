(* Are two queens threatening eachother va diagonal *)
let is_threatening (x1, y1) (x2, y2) = x1 - y1 = x2 - y2 || x1 + y1 = x2 + y2

(* Index mapping *)
let to_2d n idx = idx / n, idx mod n
let to_1d n (x, y) = (x * n) + y

(* To dedup solution list *)
module SolutionSet = Stdlib.Set.Make (struct
    type t = int list

    let compare = List.compare Int.compare
  end)

let unzip list =
  let rec go (left, right) = function
    | (left', right') :: rest -> go (left' :: left, right' :: right) rest
    | [] -> left, right
  in
  go ([], []) list
;;

module Set = Stdlib.Set.Make (Int)

(* Find all squares that do not lie on the same row or column as a queen *)
let available_spaces n queens =
  let rows, cols = unzip queens in
  let n_vector = List.init n Fun.id |> Set.of_list in
  let available_rows = Set.diff n_vector @@ Set.of_list rows |> Set.to_list in
  let available_cols = Set.diff n_vector @@ Set.of_list cols |> Set.to_list in
  Core.List.cartesian_product available_rows available_cols
;;

(* Recursively place queens while accumulator list length < n or there are saf spaces left *)
let place_queens n init_queen =
  let rec go queens =
    if List.length queens >= n
    then queens
    else (
      let potential_squares = available_spaces n queens in
      let rec find_safe = function
        | square :: squares ->
          let is_threatened = List.exists (is_threatening square) queens in
          if is_threatened then find_safe squares else Some square
        | [] -> None
      in
      match find_safe potential_squares with
      | Some queen -> go (queen :: queens)
      | None -> queens)
  in
  go [ init_queen ]
;;

(* enumerate all initial states (first queen positon) *)
let n_queens num_queens =
  let board = List.init (num_queens * num_queens) (to_2d num_queens) in
  let rec go acc = function
    | initial_queen :: queens ->
      (match place_queens num_queens initial_queen with
       | valid when List.length valid = num_queens ->
         let one_dim = List.map (to_1d num_queens) valid in
         let sorted = List.fast_sort Int.compare one_dim in
         go (SolutionSet.add sorted acc) queens
       | _ -> go acc queens)
    | [] -> acc
  in
  (* Convert back to a 2d coordinate list for debugging
     Normally, we would take the cardinality of the solution set *)
  go SolutionSet.empty board
  |> SolutionSet.to_list
  |> List.map @@ List.map (to_2d num_queens)
;;
