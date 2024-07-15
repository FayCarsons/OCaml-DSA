(** Finding a common, valid topological sort for two graphs *)

module Set = Set.Make (Int)

(* A graph represented as an array of neighbor sets, an adjacency list *)
type graph = Set.t array

(* Map each node to a set of its dependents *)
let degree_of graph =
  let dependents = Array.make (Array.length graph) Set.empty in
  Array.iteri
    (fun node neighbors ->
      Set.iter
        (fun neighbor -> dependents.(neighbor) <- Set.add node dependents.(neighbor))
        neighbors)
    graph;
  dependents
;;

(* Merge two graphs into one by taking the union of their neighbors *)
let merge_graphs = Array.map2 Set.union
let has_sink : graph -> bool = Array.exists Set.is_empty

let topological_sort : graph -> graph -> int list =
  fun first second ->
  if not @@ (has_sink first && has_sink second)
  then []
  else (
    let adjacency_list = merge_graphs first second in
    let degrees = degree_of adjacency_list in
    let queue = Queue.create () in
    Array.iteri
      (fun node dependents -> if Set.is_empty dependents then Queue.add node queue)
      degrees;
    let rec go acc not_visited =
      let popped_node = Queue.take_opt queue in
      match popped_node with
      | Some current_node ->
        if Set.mem current_node not_visited
        then (
          let neighbors = adjacency_list.(current_node) in
          Set.iter
            (fun node -> degrees.(node) <- Set.remove current_node degrees.(node))
            neighbors;
          Array.iteri
            (fun node _ ->
              if Set.mem node not_visited && Set.is_empty degrees.(node)
              then Queue.add node queue)
            degrees;
          go (current_node :: acc) (Set.remove current_node not_visited))
        else go acc not_visited
      | None -> List.rev acc
    in
    let not_visited =
      let nodes = List.init (Array.length adjacency_list) Fun.id in
      Set.of_list nodes
    in
    match go [] not_visited with
    | ordering when List.length ordering = Array.length adjacency_list -> ordering
    | _ -> [])
;;

let%test "Two graphs" =
  let g1 = [| [ 1 ]; [ 2; 3 ]; []; [ 4 ]; [] |] |> Array.map Set.of_list
  and g2 = [| [ 1 ]; [ 3 ]; []; [ 2; 4 ]; [] |] |> Array.map Set.of_list in
  topological_sort g1 g2 = [ 0; 1; 3; 2; 4 ]
;;
