let ( >> ) f g x = g @@ f x
let unimplemented () = failwith "Unimplemented!"
let ( let* ) = Result.bind
let ( >>= ) = Result.bind
let ( >>| ) = Result.map

type 'a elt_printer = 'a -> string

let print_list elt_printer list =
  "List [ " ^ List.fold_left (fun acc elt -> acc ^ elt_printer elt ^ ", ") "" list ^ "]"
;;

let is_some_and predicate = function
  | Some v -> predicate v
  | None -> false
;;

let print_map key_printer value_printer map =
  print_endline "Hashtbl {";
  Hashtbl.iter
    (fun k v -> Printf.printf "    %s: %s\n" (key_printer k) (value_printer v))
    map;
  print_endline "}"
;;

let print_set
  : type e a.
    (module Set.S with type t = a and type elt = e) -> e elt_printer -> a -> string
  =
  fun (module S) elt_printer set ->
  "Set { " ^ S.fold (fun elt acc -> acc ^ elt_printer elt) set "" ^ " }"
;;

let print_queue elt_printer q =
  "Queue ( " ^ Queue.fold (fun s elt -> s ^ elt_printer elt) "" q ^ " )"
;;

let print_stack elt_printer stack =
  "Stack [ " ^ Stack.fold (fun s elt -> s ^ elt_printer elt) "" stack ^ " ]"
;;
