open Core

let to_2d bound n = n / bound, n mod bound

module Iset = Int.Set

module QueenSet = Set.Make (struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  end)

module SolutionSet = Set.Make (QueenSet)

let is_valid queens (new_x, new_y) =
  not
  @@ Set.exists
       ~f:(fun (queen_x, queen_y) ->
         new_x + new_y = queen_x + queen_y || new_x - new_y = queen_x - queen_y)
       queens
;;

type solver =
  { n : int
  ; safe_rows : Iset.t
  ; safe_cols : Iset.t
  ; queens : QueenSet.t
  ; placed : int
  }

let create n initial_state =
  { n
  ; safe_rows =
      List.range ~stop:`exclusive 0 n
      |> Iset.of_list
      |> Fun.flip Set.remove (fst initial_state)
  ; safe_cols =
      List.range ~stop:`exclusive 0 n
      |> Iset.of_list
      |> Fun.flip Set.remove (snd initial_state)
  ; queens = QueenSet.of_list [ initial_state ]
  ; placed = 1
  }
;;

let remaining solver =
  List.cartesian_product (Set.to_list solver.safe_rows) (Set.to_list solver.safe_cols)
  |> List.filter ~f:(is_valid solver.queens)
;;

let update solver queen =
  { solver with
    safe_rows = Set.remove solver.safe_rows (fst queen)
  ; safe_cols = Set.remove solver.safe_cols (snd queen)
  ; queens = Set.add solver.queens queen
  ; placed = succ solver.placed
  }
;;

let rec place solutions solver =
  match remaining solver with
  | [] when solver.placed < solver.n -> solutions
  | [] -> Set.add solutions solver.queens
  | safe_spaces ->
    List.fold_left
      ~f:(fun solutions space -> place solutions (update solver space))
      ~init:solutions
      safe_spaces
;;

let n_queens n =
  (* Enumerate every row *)
  let init_states = List.init n ~f:(fun row -> row, 0) in
  let rec solve solutions = function
    | initial_state :: remaining ->
      let solver = create n initial_state in
      solve (place solutions solver) remaining
    | [] -> solutions
  in
  solve SolutionSet.empty init_states
;;

let%test "N = 0" =
  let res = n_queens 0 in
  let num_solutions = Set.length res in
  num_solutions = 0
;;

let%test "N = 1" =
  let res = n_queens 1 in
  let num_solutions = Set.length res in
  num_solutions = 1
;;

let%test "N = 2" =
  let res = n_queens 2 in
  let num_solutions = Set.length res in
  num_solutions = 0
;;

let%test "N = 3" =
  let res = n_queens 3 in
  let num_solutions = Set.length res in
  num_solutions = 0
;;

let%test "N = 4" =
  let res = n_queens 4 in
  let num_solutions = Set.length res in
  num_solutions = 2
;;

let%test "N = 5" =
  let res = n_queens 5 in
  let num_solutions = Set.length res in
  num_solutions = 10
;;

let%test "N = 6" =
  let res = n_queens 6 in
  let num_solutions = Set.length res in
  num_solutions = 4
;;

let%test "N = 7" =
  let res = n_queens 7 in
  let num_solutions = Set.length res in
  num_solutions = 40
;;

let%test "N = 8" =
  let res = n_queens 8 in
  let num_solutions = Set.length res in
  num_solutions = 92
;;
