open Core

module type TRIE = sig
  type t

  (** Create an empty trie *)
  val create : unit -> t

  (** Insert a word into a trie *)
  val insert : t -> string -> t

  (** Test whether a trie contains a word *)
  val search : t -> string -> bool

  (** Test  whether a given prefix describes a path in a trie *)
  val starts_with : t -> prefix:string -> bool

  (** Test whether a given string ends in a suffix contained within the tree *)
  val ends_with : t -> suffix:string -> bool
end

module Trie : TRIE = struct
  type t = Root of node option array

  and node =
    { children : node option array
    ; mutable terminal : bool
    }

  let char_to_child_index = Char.to_int
  let child_array_len = 128

  (** Creates an empty trie *)
  let create () = Root (Array.create ~len:child_array_len None)

  (** Creates a node with the given character, no children, and without the terminal flag *)
  let node () = { children = Array.create ~len:child_array_len None; terminal = false }

  let insert (Root self) word =
    let len = String.length word in
    let rec insert' char_index current_node =
      if char_index = len
      then current_node.terminal <- true
      else (
        let current_char = word.[char_index] in
        let child_index = char_to_child_index current_char in
        match current_node.children.(child_index) with
        | Some child -> insert' (succ char_index) child
        | None ->
          let new_node = node () in
          current_node.children.(child_index) <- Some new_node;
          insert' (succ char_index) new_node)
    in
    let start_index = char_to_child_index word.[0] in
    match self.(start_index) with
    | Some node ->
      insert' 1 node;
      Root self
    | None ->
      let new_node = node () in
      self.(start_index) <- Some new_node;
      insert' 1 new_node;
      Root self
  ;;

  let search (Root self) word =
    (not (String.is_empty word))
    &&
    let len = String.length word in
    let rec search' char_index nodes =
      let current_char = word.[char_index] in
      let child_index = char_to_child_index current_char in
      match nodes.(child_index) with
      | Some current_node ->
        if char_index = pred len
        then current_node.terminal
        else search' (succ char_index) current_node.children
      | None -> false
    in
    search' 0 self
  ;;

  let starts_with (Root self) ~prefix =
    String.is_empty prefix
    ||
    let len = String.length prefix in
    let rec search' char_index nodes =
      let current_character = prefix.[char_index] in
      let child_index = char_to_child_index current_character in
      match nodes.(child_index) with
      | Some next_node ->
        char_index = pred len || search' (succ char_index) next_node.children
      | None -> false
    in
    search' 0 self
  ;;

  let ends_with (Root self) ~suffix =
    String.is_empty suffix
    ||
    let len = String.length suffix in
    let rec loop character_index =
      if character_index = pred len
      then false
      else (
        let current_character = suffix.[character_index] in
        let child_index = char_to_child_index current_character in
        match self.(child_index) with
        | Some child ->
          let rec search character_index children =
            character_index = pred len
            ||
            let current_character = suffix.[character_index] in
            let child_index = char_to_child_index current_character in
            match children.(child_index) with
            | Some next -> search (succ character_index) next.children
            | None -> false
          in
          search character_index child.children
        | None -> loop (succ character_index))
    in
    loop 0
  ;;
end

let%test "Insert and search basic word" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  Trie.search trie "apple" && not (Trie.search trie "app")
;;

let%test "Search non-existent word" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  not (Trie.search trie "apples")
;;

let%test "Starts with prefix" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  Trie.starts_with trie ~prefix:"app" && Trie.starts_with trie ~prefix:"apple"
;;

let%test "Does not start with non-existent prefix" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  not (Trie.starts_with trie ~prefix:"b")
;;

let%test "Multiple word insertions" =
  let trie =
    List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple"; "apartment"; "app" ]
  in
  Trie.search trie "apartment"
  && Trie.search trie "app"
  && Trie.search trie "apple"
  && not (Trie.search trie "apart")
;;

let%test "Starts with after multiple insertions" =
  let trie =
    List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple"; "apartment"; "app" ]
  in
  Trie.starts_with trie ~prefix:"apar"
  && Trie.starts_with trie ~prefix:"app"
  && Trie.starts_with trie ~prefix:"appl"
  && not (Trie.starts_with trie ~prefix:"b")
;;

let%test "Empty string behavior" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  (not (Trie.search trie "")) && Trie.starts_with trie ~prefix:""
;;

let%test "Case sensitivity" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  (not (Trie.search trie "Apple")) && not (Trie.starts_with trie ~prefix:"App")
;;

let%test "Word with spaces" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "hello world" ] in
  Trie.search trie "hello world" && Trie.starts_with trie ~prefix:"hello "
;;

let%test "Long prefix" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "apple" ] in
  not (Trie.starts_with trie ~prefix:"apples are red")
;;

let%test "Inserting multiple words preserves previous insertions" =
  let trie = List.fold_left ~f:Trie.insert ~init:(Trie.create ()) [ "cat"; "car" ] in
  Trie.search trie "cat" && Trie.search trie "car"
;;
