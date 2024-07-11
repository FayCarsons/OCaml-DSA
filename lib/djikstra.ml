let update_min map k dist = if Hashtbl.find map k > dist then Hashtbl.replace map k dist

module Set = Set.Make (String)

let shortest_paths
  : (string, (float * string) list) Hashtbl.t -> string -> (string * float) list
  =
  fun graph start ->
  let min_cost =
    Hashtbl.to_seq graph
    |> Seq.map (fun (node, _) -> node, if node = start then 0. else Float.infinity)
    |> Hashtbl.of_seq
  in
  let rec go visited heap =
    match Heap.pop heap with
    | heap, Some (current_distance, current_node) ->
      let neighbors = Hashtbl.find graph current_node in
      List.iter
        (fun (neighbor_dist, neighbor) ->
          let new_neighbor_distance = current_distance +. neighbor_dist in
          if new_neighbor_distance < Hashtbl.find min_cost neighbor
          then (
            Hashtbl.replace min_cost neighbor new_neighbor_distance;
            let new_heap = Heap.insert heap new_neighbor_distance neighbor in
            go visited new_heap))
        neighbors;
      go (Set.add current_node visited) heap
    | _, None -> ()
  in
  go Set.empty (Heap.min_of_list [ 0., start ]);
  Hashtbl.to_seq min_cost |> List.of_seq
;;

(*
   let shortest_path : (float * int) array -> int -> int -> int list =
   fun graph start target ->
   let num_nodes = Array.length graph in
   let min_cost : float array =
   Array.init num_nodes (function
   | n when n = start -> 0.
   | _ -> Float.infinity)
   in
   let visited : bool array = Array.make num_nodes false in
   let initial_heap =
   Array.mapi (fun index distance -> distance, index) min_cost
   |> Array.to_list
   |> Heap.min_of_list
   in
   []
   ;; *)

let test_graph =
  Hashtbl.of_seq
  @@ List.to_seq
       [ "A", [ 1., "B"; 4., "C" ]
       ; "B", [ 2., "C"; 5., "D" ]
       ; "C", [ 1., "D" ]
       ; "D", [ 3., "E" ]
       ; "E", []
       ]
;;

let%test "Trivial" =
  let res =
    shortest_paths test_graph "A"
    |> List.sort (fun (fst, _) (snd, _) -> String.compare fst snd)
  in
  res = [ "A", 0.; "B", 1.; "C", 3.; "D", 4.; "E", 7. ]
;;
