open! Core

module Util = struct
  let ( >> ) f g x = g @@ f x
  let unimplemented () = failwith "Unimplemented!"

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

  let print_map key_printer value_printer map =
    "Hashtbl {"
    ^ Hashtbl.fold
        ~f:(fun ~key ~data acc ->
          acc ^ Printf.sprintf "    %s: %s\n" (key_printer key) (value_printer data))
        ~init:""
        map
    ^ "}"
  ;;

  let print_queue elt_printer q =
    "Queue ( " ^ Queue.fold ~f:(fun s elt -> s ^ elt_printer elt) ~init:"" q ^ " )\n"
  ;;

  let print_stack elt_printer stack =
    "Stack [ " ^ Stack.fold ~f:(fun s elt -> s ^ elt_printer elt) ~init:"" stack ^ " ]\n"
  ;;
end
