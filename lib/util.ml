let ( >> ) f g x = g @@ f x
let unimplemented () = failwith "Unimplemented!"
let ( let* ) = Result.bind
let ( >>= ) = Result.bind
let ( >>| ) = Result.map

let print_list list elt_printer =
  print_string "[\n  ";
  List.iter elt_printer list;
  print_string "]"
;;

let is_some_and predicate = function
  | Some v -> predicate v
  | None -> false
;;
