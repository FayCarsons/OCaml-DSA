open Core
module ISet = Int.Set
module IMap = Int.Map

let set_of_indices a =
  match ISet.of_sorted_array @@ Array.init (Array.length a) ~f:Fun.id with
  | Ok set -> set
  | Error err -> Error.raise err
;;

let degree node graph = Array.length graph.(node)

let rec match_nodes
  (graph1 : int array array)
  (graph2 : int array array)
  (mapping : int IMap.t)
  (unpaired1 : ISet.t)
  (unpaired2 : ISet.t)
  : bool
  =
  Set.is_empty unpaired1
  ||
  let node = Set.choose_exn unpaired1 in
  let has_match node2 =
    degree node graph1 = degree node2 graph2
    &&
    let new_mapping = Map.add_exn mapping ~key:node ~data:node2 in
    let is_match =
      Array.for_all
        ~f:(fun neighbor ->
          Map.find new_mapping neighbor
          |> Option.map ~f:(fun matched_neighbor ->
            Array.mem ~equal:Int.equal graph2.(node2) matched_neighbor)
          |> Option.value ~default:false)
        graph1.(node)
    in
    is_match
    && match_nodes
         graph1
         graph2
         new_mapping
         (Set.remove unpaired1 node)
         (Set.remove unpaired2 node2)
  in
  Set.exists ~f:has_match unpaired2
;;

let are_isomorphic (graph1 : int array array) (graph2 : int array array) : bool =
  Array.length graph1 = Array.length graph2
  &&
  let init_unpaired = set_of_indices graph1 in
  match_nodes graph1 graph2 IMap.empty init_unpaired init_unpaired
;;

let%test "Simple" =
  let a = [| [| 1 |]; [| 0; 2 |]; [| 1 |] |]
  and b = [| [| 2 |]; [| 2 |]; [| 0; 1 |] |] in
  are_isomorphic a b
;;

let%test "Simple2" =
  let a = [| [| 1; 2; 3 |]; [| 0 |]; [| 0; 3 |]; [| 0; 2 |] |]
  and b = [| [| 1; 2 |]; [| 0; 2; 3 |]; [| 0; 1 |]; [| 1 |] |] in
  are_isomorphic a b
;;

let%test "Complex" =
  let a_3 = [| [| 5; 1 |]; [| 0; 2 |]; [| 1; 3 |]; [| 2; 4 |]; [| 3; 5 |]; [| 4; 0 |] |]
  and b_3 =
    [| [| 1; 2 |]; [| 0; 2 |]; [| 0; 1 |]; [| 4; 5 |]; [| 3; 5 |]; [| 3; 4 |] |]
  in
  not (are_isomorphic a_3 b_3)
;;
