open! Core

let ( >> ) f g x = g @@ f x
let unimplemented () = failwith "Unimplemented!"
let curry = Tuple2.curry
let uncurry = Tuple2.uncurry

let or_default map key ~default f =
  match Hashtbl.find map key with
  | Some value -> Hashtbl.set map ~key ~data:(f value)
  | None -> Hashtbl.add map ~key ~data:(f default) |> ignore
;;

type 'a elt_printer = 'a -> string

let print_list elt_printer list =
  "List [ "
  ^ List.fold_left ~f:(fun acc elt -> acc ^ elt_printer elt ^ ", ") ~init:"" list
  ^ "]\n"
;;

let print_array elt_printer array =
  "List [ "
  ^ Array.fold ~f:(fun acc elt -> acc ^ elt_printer elt ^ ", ") ~init:"" array
  ^ "]\n"
;;

let is_some_and predicate = function
  | Some v -> predicate v
  | None -> false
;;

let print_set : type elt cmp. (elt, cmp) Set.t -> elt elt_printer -> unit =
  fun s elt_printer ->
  print_endline
  @@ "Set { "
  ^ Set.fold s ~f:(fun acc elt -> acc ^ elt_printer elt ^ "; ") ~init:""
  ^ "}"
;;

let print_map
  : type key data cmp.
    (key, data, cmp) Map.t
    -> key_printer:(key -> string)
    -> data_printer:(data -> string)
    -> unit
  =
  fun m ~key_printer ~data_printer ->
  print_endline
  @@ "Map {\n"
  ^ Map.fold
      m
      ~f:(fun ~key ~data acc ->
        acc ^ Printf.sprintf "  %s: %s\n" (key_printer key) (data_printer data))
      ~init:""
  ^ "}"
;;

let print_table
  : type key data.
    (key, data) Hashtbl.t
    -> key_printer:(key -> string)
    -> data_printer:(data -> string)
    -> unit
  =
  fun table ~key_printer ~data_printer ->
  print_endline
  @@ "Hashtbl {"
  ^ Hashtbl.fold
      ~f:(fun ~key ~data acc ->
        acc ^ Printf.sprintf "    %s: %s\n" (key_printer key) (data_printer data))
      ~init:""
      table
  ^ "}"
;;

let print_queue elt_printer q =
  "Queue ( " ^ Queue.fold ~f:(fun s elt -> s ^ elt_printer elt) ~init:"" q ^ " )\n"
;;

let print_stack elt_printer stack =
  "Stack [ " ^ Stack.fold ~f:(fun s elt -> s ^ elt_printer elt) ~init:"" stack ^ " ]\n"
;;
