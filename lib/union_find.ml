open Core
module Table = Hashtbl.Make (String)

type t =
  { parents : string Table.t
  ; ranks : int Table.t
  ; factors : float Table.t
  }

let fail_duplicate_keys = function
  | `Ok table -> table
  | `Duplicate_keys _ -> failwith "Duplicate keys!"
;;

module NeighborSet = Set.Make (struct
    type t = float * string

    let t_of_sexp = Tuple2.t_of_sexp Float.t_of_sexp String.t_of_sexp
    let sexp_of_t = Tuple2.sexp_of_t Float.sexp_of_t String.sexp_of_t

    let compare (multiplier1, unit1) (multiplier2, unit2) =
      match String.compare unit1 unit2 with
      | 0 -> Float.compare multiplier1 multiplier2
      | cmp -> cmp
    ;;
  end)

type graph = NeighborSet.t Table.t

let init (graph : graph) =
  let size = Hashtbl.length graph in
  let get_key = Hashtbl.keys graph |> Array.of_list |> Array.unsafe_get in
  let parents : string Table.t =
    Table.create_mapped ~get_key ~get_data:get_key ~size (List.init size ~f:Fun.id)
    |> fail_duplicate_keys
  in
  let ranks =
    Table.create_mapped ~get_key ~get_data:(Fun.const 0) ~size (List.init size ~f:Fun.id)
    |> fail_duplicate_keys
  in
  let factors =
    Table.create_mapped ~get_key ~get_data:(Fun.const 1.) (List.init size ~f:Fun.id)
    |> fail_duplicate_keys
  in
  { parents; ranks; factors }
;;

let rec find self node =
  let parent = Hashtbl.find_exn self.parents node in
  if String.( <> ) parent node
  then (
    let root = find self parent in
    let factor_to_parent = Hashtbl.find_exn self.factors node in
    let parent_to_root = Hashtbl.find_exn self.factors parent in
    Hashtbl.set self.parents ~key:node ~data:root;
    Hashtbl.set self.factors ~key:node ~data:(factor_to_parent *. parent_to_root);
    root)
  else node
;;

let union self node1 node2 factor =
  let root1 = find self node1
  and root2 = find self node2 in
  if String.( <> ) root1 root2
  then (
    try
      let rank1 = Hashtbl.find_exn self.ranks root1
      and rank2 = Hashtbl.find_exn self.ranks root2 in
      if rank1 < rank2
      then (
        Hashtbl.set self.parents ~key:root1 ~data:root2;
        let factor1 = Hashtbl.find_exn self.factors node1
        and factor2 = Hashtbl.find_exn self.factors node2 in
        Hashtbl.set self.factors ~key:root1 ~data:(factor2 *. factor /. factor1))
      else (
        Hashtbl.set self.parents ~key:root2 ~data:root1;
        let factor1 = Hashtbl.find_exn self.factors node1
        and factor2 = Hashtbl.find_exn self.factors node2 in
        Hashtbl.set self.factors ~key:root2 ~data:(factor1 /. (factor2 *. factor));
        if rank1 = rank2 then Hashtbl.change self.ranks root1 ~f:(Option.map ~f:succ))
    with
    | _ -> ())
;;

let convert self ~from ~target =
  let root1 = find self from
  and root2 = find self target in
  if String.( <> ) root1 root2
  then None
  else (
    let factor1 = Hashtbl.find_exn self.factors from
    and factor2 = Hashtbl.find_exn self.factors target in
    Some (factor1 /. factor2))
;;

let make (graph : graph) =
  let self = init graph in
  Hashtbl.iteri
    ~f:(fun ~key:node ~data:neighbors ->
      Set.iter ~f:(fun (factor, neighbor) -> union self node neighbor factor) neighbors)
    graph;
  self
;;

let query self (measurement, from) target =
  convert self ~from ~target |> Option.map ~f:(fun ratio -> measurement *. ratio)
;;

module Tests = struct
  let test_graph : graph = 
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
    |> List.map ~f:(fun (node, neighbors) -> (node, NeighborSet.of_list neighbors ))
    |> Table.of_alist_exn
[@@ocamlformat "disable"]

  let self = make test_graph

  let is_some_and pred = function
    | Some x -> pred x
    | None -> false
  ;;

  let%test "Foot -> Cm" =
    let start = 3., "ft" in
    let target = "cm" in
    is_some_and (Float.( = ) 91.44) @@ query self start target
  ;;

  let%test "Should Fail" =
    let start = 1., "hr" in
    let target = "gal" in
    Option.is_none @@ query self start target
  ;;

  let%test "Miligram -> Kilo" =
    let start = 1_000_000., "mg" in
    let target = "kg" in
    is_some_and (Float.( = ) 1.) @@ query self start target
  ;;

  let%test "Year -> Hour" =
    let start = 1., "yr" in
    let target = "hr" in
    let res = query self start target in
    is_some_and (Float.( = ) 8760.) res
  ;;
end
