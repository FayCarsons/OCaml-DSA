open! Core
module Set = Stdlib.Set.Make (Int)

module Point = Points.Point (struct
    include Float

    let mul = ( *. )
    let div = ( /. )
    let to_float = Fun.id
    let of_float = Fun.id
  end)

let connect_points start_id graph point_map (heap : (float, int) Heap.t) seen =
  let rec go (heap : (float, int) Heap.t) seen last_id =
    match Heap.pop heap with
    | updated_heap, Some (_, current_id) ->
      if Set.mem current_id seen
      then go updated_heap seen last_id
      else (
        graph.(last_id) <- current_id :: graph.(last_id);
        let current_pos = Hashtbl.find_exn point_map current_id in
        let new_heap : (float, int) Heap.t =
          Hashtbl.fold
            ~f:(fun ~key ~data heap ->
              if Set.mem key seen
              then heap
              else Heap.insert heap (Point.distance current_pos data) key)
            point_map
            ~init:updated_heap
        in
        let new_seen = Set.add current_id seen in
        go new_heap new_seen current_id)
    | _, _ when Set.cardinal seen = Hashtbl.length point_map -> Some graph
    | _ -> None
  in
  go heap seen start_id
;;

let minimum_spanning_tree : (int * Point.t) list -> int list array option =
  fun points ->
  let graph = Array.create ~len:(List.length points) [] in
  let point_map =
    match Hashtbl.of_alist (module Int) points with
    | `Ok map -> map
    | _ -> assert false
  in
  match points with
  | (id, pos) :: rest ->
    let with_distance =
      List.map ~f:(fun (curr_id, curr_pos) -> Point.distance pos curr_pos, curr_id) rest
    in
    let heap = Heap.min_of_list with_distance in
    let seen = Set.of_list [ id ] in
    connect_points id graph point_map heap seen
  | [] -> None
;;

let test_graph =
  Point.
    [ 0, { x = 0.0; y = 0.0 }
    ; 1, { x = 1.0; y = 1.0 }
    ; 2, { x = 2.0; y = 0.0 }
    ; 3, { x = 1.0; y = -1.0 }
    ]
;;

let%test "Simple MST" =
  let res = minimum_spanning_tree test_graph in
  (match res with
   | Some itinerary ->
     print_endline @@ Util.print_array (Util.print_list string_of_int) itinerary
   | None -> assert false);
  true
;;
