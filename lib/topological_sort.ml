type key = char

let ( >> ) f g x = g @@ f x

module Map = Hashtbl.Make (Char)
module Set = Set.Make (Char)

(* Map from node -> neighbors *)
type graph = key list Map.t

(* Map from node -> dependents *)
type degree_map = int Map.t

let print_sep () = print_string ", "

let print_degrees m =
  print_string "Degree-map {\n  ";
  Map.iter (fun k v -> Printf.printf "%c -> %s,\n" k @@ string_of_int v) m;
  print_string "}\n\n"
;;

let print_queue q =
  print_string "Queue: [\n   ";
  Queue.iter (print_char >> print_sep) q;
  print_string "\n]\n"
;;

let update m k f = Map.replace m k @@ f (Map.find m k)

let update_default m k ~default f =
  let entry =
    match Map.find_opt m k with
    | Some v -> f v
    | None -> f default
  in
  Map.replace m k entry
;;

let remove_dependent graph degrees node =
  let neighbors = Map.find graph node in
  List.iter (fun neighbor -> update degrees neighbor (fun n -> max 0 (pred n))) neighbors
;;

(* Get the degree (number of dependents) of each node *)
let init_degrees : graph -> degree_map =
  fun graph ->
  let degrees = Map.create @@ Map.length graph in
  Map.iter (fun k _ -> Map.add degrees k 0) graph;
  Map.iter
    (fun _ neighbors ->
      List.iter (fun neighbor -> update degrees neighbor succ) neighbors)
    graph;
  degrees
;;

(* Get all nodes with no dependents *)
let get_dependent_free degrees =
  Map.to_seq degrees
  |> Seq.filter_map (fun (k, v) ->
    if 0 = v
    then (
      (* remove node from the dependency graph in-place *)
      Map.remove degrees k;
      Some k)
    else None)
;;

let topological_sort : graph -> key list option =
  fun graph ->
  (* Create a mapping from node -> dependents *)
  let degrees = init_degrees graph in
  (* Get all zero-dependent nodes and put them in a queue *)
  let queue = get_dependent_free degrees |> Queue.of_seq in
  Queue.iter (remove_dependent graph degrees) queue;
  let rec go (acc : key list) =
    if Queue.is_empty queue
    then
      (* Check if the dependency graph is empty,
         if not we have not found an ordering and there is
         potentially a cycle *)
      if Map.length degrees = 0 then Some (List.rev acc) else None
    else (
      (* Get a new node *)
      let node = Queue.pop queue in
      (* Decrement the dependency count of its dependents *)
      remove_dependent graph degrees node;
      (* Get new zero-dependent nodes *)
      let new_zeroed = get_dependent_free degrees in
      (* Push them onto the queue *)
      Queue.add_seq queue new_zeroed;
      (* recurse with node cons'd onto accumulator *)
      go (node :: acc))
  in
  go []
;;

let map_of_list : (char * 'a) list -> 'a Map.t = List.to_seq >> Map.of_seq

let%test "Simple graph" =
  let graph = map_of_list [ 'A', [ 'B'; 'C' ]; 'B', [ 'D' ]; 'C', [ 'D' ]; 'D', [] ] in
  let res = topological_sort graph in
  Option.is_some res
;;

let%test "Complex graph" =
  let graph =
    map_of_list
      [ 'A', [ 'B' ]
      ; 'B', [ 'C'; 'D' ]
      ; 'C', [ 'E' ]
      ; 'D', [ 'E'; 'F' ]
      ; 'E', [ 'G' ]
      ; 'F', [ 'G' ]
      ; 'G', []
      ]
  in
  let res = topological_sort graph in
  Option.is_some res
;;

let%test "Single Node" =
  let graph = map_of_list [ 'A', [] ] in
  let res = topological_sort graph in
  Option.is_some res
;;

let%test "Disconnected nodes" =
  let graph = map_of_list [ 'A', []; 'B', []; 'C', [] ] in
  let res = topological_sort graph in
  Option.is_some res
;;
