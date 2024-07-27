[@@@ocaml.warning "-32-34"]

let world =
  [| [ 1; 4 ]
   ; (* North America *)
     [ 0; 2 ]
   ; (* South America *)
     [ 1; 3 ]
   ; (* Africa *)
     [ 2; 4 ]
   ; (* Europe *)
     [ 0; 3; 5 ]
   ; (* Asia *)
     [ 5 ] (* Australia *)
  |]
;;

module Player = struct
  type t =
    | Cyan
    | Orange
    | Black

  let to_int = function
    | Cyan -> 0
    | Orange -> 1
    | Black -> 2
  ;;

  let of_int = function
    | 0 -> Cyan
    | 1 -> Orange
    | 2 -> Black
    | _ -> raise (Invalid_argument "Player of int")
  ;;
end

let occupations =
  Player.
    [| Cyan, 2
     ; (* North America *)
       Orange, 3
     ; (* South America *)
       Orange, 4
     ; (* Africa *)
       Cyan, 3
     ; (* Europe *)
       Cyan, 1
     ; (* Asia *)
       Black, 5 (* Australia *)
    |]
;;

let or_default map key ~default f =
  match Hashtbl.find_opt map key with
  | Some value -> Hashtbl.replace map key (f value)
  | None -> Hashtbl.add map key (f default)
;;

module Score = struct
  module Set = Set.Make (Int)

  let rec search countries occupations unvisited player node : (Set.t * int) option =
    if Set.mem node unvisited
    then (
      let occupier, num_troops = occupations.(node) in
      if occupier = player
      then (
        let neighbors = countries.(node) in
        let unvisited, final_troops =
          List.fold_left
            (fun (unvisited, num_troops) neighbor ->
              if Set.mem neighbor unvisited
              then (
                match search countries occupations unvisited player neighbor with
                | Some (unvisited, found_troops) -> unvisited, num_troops + found_troops
                | None -> unvisited, num_troops)
              else unvisited, num_troops)
            (unvisited, num_troops)
            neighbors
        in
        Some (unvisited, final_troops))
      else None)
    else None
  ;;

  let score countries occupations =
    let player_occupations = Array.make 3 [] in
    let unvisited = Set.of_list @@ List.init (Array.length countries) Fun.id in
    let rec loop unvisited =
      if Set.is_empty unvisited
      then player_occupations
      else (
        let country = Set.choose unvisited in
        let player, found_troops = occupations.(country) in
        match search countries occupations unvisited player country with
        | Some (new_unvisited, found_troops) ->
          let player_idx = Player.to_int player in
          player_occupations.(player_idx)
          <- found_troops :: player_occupations.(player_idx);
          loop new_unvisited
        | None ->
          let player_idx = Player.to_int player in
          player_occupations.(player_idx)
          <- found_troops :: player_occupations.(player_idx);
          loop unvisited)
    in
    loop unvisited
    |> Array.mapi (fun player occupations -> Player.of_int player, occupations)
  ;;

  let expected_result = Player.[| Cyan, [ 2; 4 ]; Black, [ 5 ]; Orange, [ 7 ] |] in
  let actual_result = score world occupations in
  assert (expected_result = actual_result)
end
