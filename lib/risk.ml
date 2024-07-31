

module Player = struct
  type t =
    { tag : tag
    ; troops : int
    }

  and tag =
    | Cyan
    | Orange
    | Black

  let tag_eq a b =
    match a, b with
    | Cyan, Cyan | Orange, Orange | Black, Black -> true
    | _ -> false
  ;;

  let tag_to_int = function
    | Cyan -> 0
    | Orange -> 1
    | Black -> 2
  ;;

  let tag_of_int = function
    | 0 -> Cyan
    | 1 -> Orange
    | 2 -> Black
    | _ -> raise (Invalid_argument "Player of int")
  ;;
end

type country =
  { neighbors : int array
  ; occupation : (Player.tag * int) option
  }

type game = { countries : country array }

let world =
  [| { neighbors = [| 1; 4 |]; occupation = Some (Cyan, 2) }
   ; { neighbors = [| 0; 2 |]; occupation = Some (Orange, 3) }
   ; { neighbors = [| 1; 3 |]; occupation = Some (Orange, 4) }
   ; { neighbors = [| 2; 4 |]; occupation = Some (Cyan, 3) }
   ; { neighbors = [| 0; 3; 5 |]; occupation = Some (Cyan, 1) }
   ; { neighbors = [| 5 |]; occupation = Some (Black, 5) }
  |]
;;

let or_default map key ~default f =
  match Hashtbl.find_opt map key with
  | Some value -> Hashtbl.replace map key (f value)
  | None -> Hashtbl.add map key (f default)
;;

open Core

module Score = struct
  module ISet = Int.Hash_set

  type t =
    { world : country array
    ; unvisited : ISet.t
    ; mutable current_player : Player.tag
    }

  let create (game : game) =
    { world = game.countries
    ; unvisited = ISet.of_list @@ List.init (Array.length game.countries) ~f:Fun.id
    ; current_player = Black
    }
  ;;

  let rec search (state : t) (current_node : int) : int option =
    if Hash_set.mem state.unvisited current_node
    then (
      match state.world.(current_node) with
      | { occupation = Some (occupier, troops); neighbors }
        when Player.tag_eq occupier state.current_player ->
        Hash_set.remove state.unvisited current_node;
        Option.return
        @@ Array.fold
             ~init:troops
             ~f:(fun num_troops country ->
               if Hash_set.mem state.unvisited country
               then (
                 match search state country with
                 | Some found_troops -> num_troops + found_troops
                 | None -> num_troops)
               else num_troops)
             neighbors
      | _ -> None)
    else None
  ;;

  let score (game : game) =
    let player_scores = Array.create ~len:3 [] in
    let num_countries = Array.length game.countries in
    let unvisited =
      ISet.of_list ~size:num_countries @@ List.init num_countries ~f:Fun.id
    in
    let rec loop state =
      if Hash_set.is_empty unvisited
      then player_scores
      else (
        let country = Hash_set.find unvisited ~f:(Fun.const true) |> Option.value_exn in
        match game.countries.(country).occupation with
        | Some (player, troops) ->
          (match search state country with
           | Some found_troops ->
             let player_idx = Player.tag_to_int player in
             player_scores.(player_idx)
             <- (troops + found_troops) :: player_scores.(player_idx);
             loop state
           | None ->
             let player_idx = Player.tag_to_int player in
             player_scores.(player_idx) <- troops :: player_scores.(player_idx);
             loop state)
        | None -> loop state)
    in
    let state = create game in
    loop state
    |> Array.mapi ~f:(fun player occupations -> Player.tag_of_int player, occupations)
  ;;
  (*
     let expected_result = Player.[| Cyan, [ 2; 4 ]; Black, [ 5 ]; Orange, [ 7 ] |] in
     let actual_result = score world occupations in
     assert (expected_result = actual_result) *)
end
