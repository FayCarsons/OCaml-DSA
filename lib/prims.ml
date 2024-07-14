module Set = Set.Make (Int)

module Point = Points.Point (struct
    include Float

    let to_float = Fun.id
    let of_float = Fun.id
  end)

let or_default map key ~default f =
  match Hashtbl.find_opt map key with
  | Some value -> Hashtbl.replace map key (f value)
  | None -> Hashtbl.add map key (f default)
;;

let connect_points start_id graph point_map heap seen : (int, int list) Hashtbl.t option =
  let rec go heap seen last_id =
    match Heap.pop heap with
    | updated_heap, Some (_, current_id) ->
      if Set.mem current_id seen
      then go updated_heap seen last_id
      else (
        or_default graph last_id ~default:[] (List.cons current_id);
        let current_pos = Hashtbl.find point_map current_id in
        let new_heap : (float, int) Heap.t =
          Hashtbl.fold
            (fun id pt heap ->
              if Set.mem id seen
              then heap
              else Heap.insert heap (Point.distance current_pos pt) id)
            point_map
            updated_heap
        in
        let new_seen = Set.add current_id seen in
        go new_heap new_seen current_id)
    | _, _ when Set.cardinal seen = Hashtbl.length point_map -> Some graph
    | _ -> None
  in
  go heap seen start_id
;;

let minimum_spanning_tree : (int * Point.t) list -> (int, int list) Hashtbl.t option =
  fun points ->
  let graph = Hashtbl.create @@ List.length points in
  let point_map = Hashtbl.of_seq @@ List.to_seq points in
  match points with
  | (id, pos) :: rest ->
    Hashtbl.add graph id [];
    let with_distance =
      List.map (fun (curr_id, curr_pos) -> Point.distance pos curr_pos, curr_id) rest
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
  Option.iter (Util.print_map string_of_int @@ Util.print_list string_of_int) res;
  true
;;
