module Map = Map.Make (String)
module Visited = Set.Make (String)

type measurement = string
type edge = float * measurement
type measurement_graph = edge list Map.t

let parse_facts : (measurement * edge) list -> measurement_graph =
  fun facts ->
  let rec go acc = function
    | (measurement, edge) :: xs ->
      go
        (Map.update
           measurement
           (function
             | Some edges -> Some (edge :: edges)
             | None -> Some [ edge ])
           acc)
        xs
    | [] -> acc
  in
  go Map.empty facts
;;

let query : measurement_graph -> edge -> measurement -> float option =
  fun graph (quantity, start) target ->
  match Map.find_opt start graph with
  | Some edges ->
    (* Do dfs, accumulating the converted value through each path
       with reduce so that the first valid path returns.
       The function passed to reduce should accumulate a 'visited' set, walking
       the tree and returning 'none' if the current path cannot end on 'target' *)
    let visited = Visited.of_list [ start ] in
    let rec search visited value (qty, unit) =
      let value = qty *. value in
      if unit = target
      then Some value
      else (
        let visited = Visited.add unit visited in
        let neighbors =
          Map.find unit graph
          |> List.filter (fun (_, unit) -> not @@ Visited.mem unit visited)
        in
        List.find_map (search visited value) neighbors)
    in
    List.find_map (search visited quantity) edges
  | None -> None
;;

(* large (AI generated) graph for testing *)
let test_graph =
  Map.of_list
    (* Distance *)
    [ "ft", [ 12., "in"; 0.333333, "yd"; 0.3048, "m" ]
    ; "in", [ 0.0833333, "ft"; 2.54, "cm" ]
    ; "yd", [ 3., "ft"; 0.9144, "m" ]
    ; "m", [ 100., "cm"; 1000., "mm"; 1.09361, "yd"; 3.28084, "ft"; 39.3701, "in" ]
    ; "cm", [ 0.01, "m"; 10., "mm"; 0.393701, "in" ]
    ; "mm", [ 0.001, "m"; 0.1, "cm" ]
    ; "km", [ 1000., "m"; 0.621371, "mi" ]
    ; "mi", [ 5280., "ft"; 1760., "yd"; 1.60934, "km" ] 

    (* Time *)
    ; "s", [ 0.0166667, "min"; 0.000277778, "hr" ]
    ; "min", [ 60., "s"; 0.0166667, "hr" ]
    ; "hr", [ 3600., "s"; 60., "min"; 0.0416667, "day" ]
    ; "day", [ 24., "hr"; 1440., "min"; 86400., "s" ]
    ; "wk", [ 7., "day" ]
    ; "mo", [ 30.4375, "day"; 4.34524, "wk" ]
    ; "yr", [ 12., "mo"; 52.1429, "wk"; 365., "day" ] 

    (* Weight *)
    ; "g", [ 1000., "mg"; 0.001, "kg" ]
    ; "mg", [ 0.001, "g" ]
    ; "kg", [ 1000., "g"; 2.20462, "lb"; 35.274, "oz" ]
    ; "lb", [ 0.453592, "kg"; 16., "oz" ]
    ; "oz", [ 0.0283495, "kg"; 0.0625, "lb" ]
    ; "l", [ 1000., "ml"; 0.001, "m³" ]
    ; "ml", [ 0.001, "l" ]
    ; "m³", [ 1000., "l" ]
    ; "gal", [ 3.78541, "l"; 128., "fl oz" ]
    ; "fl oz", [ 0.0078125, "gal"; 29.5735, "ml" ]
    ] 
[@@ocamlformat "disable"]

let is_some_and pred = function
  | Some x -> pred x
  | None -> false
;;

let%test "Foot -> Cm" =
  let start = 3., "ft" in
  let target = "cm" in
  is_some_and (( = ) 91.44) @@ query test_graph start target
;;

let%test "Should Fail" =
  let start = 1., "hr" in
  let target = "gal" in
  Option.is_none @@ query test_graph start target
;;

let%test "Miligram -> Kilo" =
  let start = 1_000_000., "mg" in
  let target = "kg" in
  is_some_and (( = ) 1.) @@ query test_graph start target
;;

(* This may be inaccurate for float reasons, ideally alll of this would be done
   with fixed point arithmetic but eh *)
let%test "Year -> Hour" =
  let start = 1., "yr" in
  let target = "hr" in
  let res = query test_graph start target in
  is_some_and (( = ) 8766.) res
;;

(* Generic DFS and BFS functions *)

(* We'll assume that our node IDs are integers so we can represent a graph as an array of neighbor sets *)
module Set = Set.Make (Int)

type graph = Set.t array

let dfs : graph -> int list =
  fun graph ->
  let rec go acc unvisited current_node =
    if Set.mem current_node unvisited
    then (
      let unvisited_neighbors = Set.inter unvisited graph.(current_node) in
      let updated_unvisited = Set.remove current_node unvisited in
      Set.fold
        (fun node acc -> go acc updated_unvisited node)
        unvisited_neighbors
        (current_node :: acc))
    else acc
  in
  let unvisited = Seq.ints 0 |> Seq.take (Array.length graph) |> Set.of_seq in
  List.rev @@ go [] unvisited 0
;;

let bfs : graph -> int list =
  fun graph ->
  let queue = Queue.of_seq @@ Seq.return 0 in
  let rec go acc unvisited =
    match Queue.take_opt queue with
    | Some node ->
      if Set.is_empty unvisited
      then List.rev acc
      else (
        let neighbors = Set.inter graph.(node) unvisited in
        Set.iter (Fun.flip Queue.add queue) neighbors;
        go (node :: acc) (Set.remove node unvisited))
    | None -> List.rev acc
  in
  let unvisited = Set.of_list @@ List.init (Array.length graph) Fun.id in
  go [] unvisited
;;

(* Test graph:
   0: [1, 2]
   1: [0, 3, 4]
   2: [0]
   3: [1]
   4: [1]
*)
let test_graph : graph =
  [| Set.of_list [ 1; 2 ]
   ; Set.of_list [ 0; 3; 4 ]
   ; Set.of_list [ 0 ]
   ; Set.of_list [ 1 ]
   ; Set.of_list [ 1 ]
  |]
;;

let%test "DFS" =
  let res = dfs test_graph in
  res = [ 0; 1; 3; 4; 2 ] || res = [ 0; 2; 3; 4; 1 ]
;;

let%test "BFS" =
  let res = bfs test_graph in
  res = [ 0; 1; 2; 3; 4 ] || res = [ 0; 2; 1; 3; 4 ]
;;
