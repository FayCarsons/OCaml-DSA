type t =
  | Root of t list
  | Node of char * t list
  | Leaf of string

let root list = Root list
let leaf s = Leaf s

let group_by_prefixes table idx strs =
  let with_prefixes = List.map (fun s -> s.[idx], s) strs in
  List.iter (fun (char, _) -> Hashtbl.remove table char) with_prefixes;
  let rec go acc = function
    | (char, parent) :: prefixes ->
      (match Hashtbl.find_opt table char with
       | Some parents ->
         Hashtbl.replace table char (parent :: parents);
         go acc prefixes
       | None ->
         Hashtbl.add table char [ parent ];
         go (char :: acc) prefixes)
    | [] -> acc
  in
  go [] with_prefixes
;;

let build list =
  let num_xs = List.length list in
  let table = Hashtbl.create @@ (num_xs * num_xs) in
  let rec build' idx strs =
    let pairs = group_by_prefixes table idx strs in
    List.map
      (fun current_char ->
        let parents = Hashtbl.find table current_char in
        match parents with
        | [ one ] when idx = pred (String.length one) -> Node (current_char, [ Leaf one ])
        | many -> Node (current_char, build' (succ idx) many))
      pairs
  in
  root @@ build' 0 list
;;

let option_to_bool = function
  | Some _ -> true
  | _ -> false
;;

let search trie str =
  let len = String.length str in
  let rec go idx = function
    | Root children -> List.find_opt (go 0) children |> option_to_bool
    | Node (_, []) -> false
    | Node (_, [ Leaf last ]) -> idx = pred len && last = str
    | Node (char, children) ->
      if char = str.[idx]
      then List.find_opt (go (succ idx)) children |> option_to_bool
      else false
    | Leaf last -> last = str
  in
  go 0 trie
;;
