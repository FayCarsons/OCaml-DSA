module Set = Set.Make (struct
    type t = string * string

    let compare = compare
  end)

let start = "JFK"

let or_default map k ~default f =
  match Hashtbl.find_opt map k with
  | Some v -> Hashtbl.replace map k (f v)
  | None -> Hashtbl.add map k (f default)
;;

let reconstruct_itenerary tickets =
  let graph =
    let empty = Hashtbl.create @@ List.length tickets in
    List.iter
      (fun (from, destination) ->
        or_default empty from ~default:[] (List.cons destination))
      tickets;
    empty
  in
  Hashtbl.iter
    (fun node neighbors ->
      Hashtbl.replace graph node (List.fast_sort String.compare neighbors))
    graph;
  let rec dfs itenerary current_node =
    match Hashtbl.find graph current_node with
    | [] | exception Not_found -> current_node :: itenerary
    | neighbors -> (
      let rec go acc = function 
        | neighbor :: neighbors ->
          Hashtbl.replace graph current_node neighbors;
          go (dfs acc neighbor) neighbors
        | []  -> acc 
      in
      current_node :: go itenerary neighbors
    )
  in
  dfs [] start
;;

let res_of_string res =
  "[ " ^ List.fold_left (fun acc node -> acc ^ node ^ "; ") "" res ^ "]\n"
;;

let%test "Reconstruct itenerary #1" =
  let tickets = [ 
    "MUC", "LHR"; 
    "JFK", "MUC"; 
    "SFO", "SJC"; 
    "LHR", "SFO" 
  ] [@@ocamlformat "disable"] in
  reconstruct_itenerary tickets = [ "JFK"; "MUC"; "LHR"; "SFO"; "SJC" ]
;;

(* Why is this one broken? *)

let%test "Reconstruct itenerary #2" =
  let tickets =
    [ 
      "JFK", "SFO"; 
      "JFK", "ATL"; 
      "SFO", "ATL"; 
      "ATL", "JFK"; 
      "ATL", "SFO" 
  ] [@@ocamlformat "disable"]
  in
   reconstruct_itenerary tickets 
  = [ "JFK"; "ATL"; "JFK"; "SFO"; "ATL"; "SFO" ];;
