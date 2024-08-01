module Naive = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let get self key = Hashtbl.find_opt self key
  let set self key value = Hashtbl.add self key value

  let rec fib cache n =
    match Hashtbl.find_opt cache n with
    | Some value -> value
    | None ->
      let new_value = fib cache (n - 1) + fib cache (n - 2) in
      Hashtbl.add cache n new_value;
      new_value
  ;;
end

module Better = struct
  type ('a, 'b) t =
    { cache : ('a, 'b) Hashtbl.t
    ; queue : ('a * 'b) Queue.t
    ; capacity : int
    ; mutable size : int
    }

  let create capacity =
    { cache = Hashtbl.create capacity; queue = Queue.create (); capacity; size = 0 }
  ;;

  let get self key = Hashtbl.find_opt self.cache key

  let set self key value =
    if self.size >= self.capacity
    then (
      let oldest_key, _ = Queue.pop self.queue in
      Hashtbl.remove self.cache oldest_key;
      self.size <- pred self.size);
    Queue.add (key, value) self.queue;
    Hashtbl.add self.cache key value;
    self.size <- succ self.size
  ;;

  let rec fib cache n =
    if n < 2
    then n
    else (
      match get cache n with
      | Some value -> value
      | None ->
        let new_value = fib cache (n - 1) + fib cache (n - 2) in
        set cache n new_value;
        new_value)
  ;;

  let%test "Queue-based cache fibonacci" =
    (* memoized *)
    let fib =
      let cache = create 128 in
      fib cache
    in
    let first = fib 4 in
    let second = fib 8 in
    let third = fib 16 in
    fib 4 = first
    && first = 3
    && fib 8 = second
    && second = 21
    && third = fib 16
    && third = 987
  ;;
end

module Best = struct
  open Lru

  let rec fib lru n =
    if n < 2
    then n
    else (
      match get lru n with
      | Some value -> value
      | None ->
        let result = if n < 2 then n else fib lru (n - 1) + fib lru (n - 2) in
        set lru n result;
        result)
  ;;

  let%test "LRU fibonacci" =
    (* memoized *)
    let fib =
      let lru = Lru.create 128 in
      fib lru
    in
    let first = fib 4 in
    let second = fib 8 in
    let third = fib 16 in
    fib 4 = first
    && first = 3
    && fib 8 = second
    && second = 21
    && third = fib 16
    && third = 987
  ;;
end
