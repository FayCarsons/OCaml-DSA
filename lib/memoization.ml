module Naive = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let get self key = Hashtbl.find_opt self key
  let set self key value = Hashtbl.add self key value

  let memoize : ('a -> 'b) -> ?size:int -> 'a -> 'b =
    fun f ?(size = 512) ->
    let cache = Hashtbl.create size in
    fun arg ->
      match Hashtbl.find_opt cache arg with
      | Some value -> value
      | None ->
        let new_value = f arg in
        Hashtbl.add cache arg new_value;
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

  let memoize : int -> ('a -> 'b) -> 'a -> 'b =
    fun capacity f ->
    let self = create capacity in
    fun arg ->
      match get self arg with
      | Some value -> value
      | None ->
        let new_value = f arg in
        set self arg new_value;
        new_value
  ;;
end

module Best = struct
  include Lru

  let get : ('k, 'v) t -> 'k -> 'v option =
    fun self key ->
    (* Check if cache contains key *)
    Hashtbl.find_opt self.cache key
    (* If so, remove node pointed to in cache,
       and prepend (<->) to front of list *)
    |> Option.map (fun list_node ->
      let open DList in
      (* Remove the node from the list, leaving us with an isolated node *)
      let temp_node = remove list_node in
      (* prepend node to front *)
      let _ = temp_node <-> self.list in
      (* Advance the least_used pointer *)
      self.least_used <- next self.least_used;
      let _, value = get_exn temp_node in
      value)
  ;;

  let set self key value =
    let open DList in
    if self.size >= self.threshold
    then (
      let temp_key, _ = get_exn self.least_used in
      self.least_used <- prev self.least_used;
      Hashtbl.remove self.cache temp_key;
      self.size <- pred self.size);
    let new_node = node (key, value) in
    self.list <- new_node <-> self.list;
    Hashtbl.add self.cache key new_node;
    self.size <- succ self.size;
    if self.least_used = DList.empty then self.least_used <- new_node
  ;;

  let rec fib lru n =
    match get lru n with
    | Some value -> value
    | None ->
      let result = if n < 2 then n else fib lru (n - 1) + fib lru (n - 2) in
      set lru n result;
      result
  ;;

  (* Wraps 'f' in a memoization function that stores 'threshold' most recently used values *)
  let memoize f threshold =
    let lru = create threshold in
    fun arg ->
      match get lru arg with
      | Some value -> value
      | None ->
        let new_value = f arg in
        set lru arg new_value;
        new_value
  ;;

  let%test "LRU fibonacci" =
    let rec fib_raw n = if n < 2 then n else fib_raw (n - 1) + fib_raw (n - 2) in
    (* memoized *)
    let num_calls = ref 0 in
    let fib =
      memoize
        (fun n ->
          num_calls := succ !num_calls;
          fib_raw n)
        32
    in
    let first = fib 4 in
    let second = fib 8 in
    let third = fib 16 in
    assert (!num_calls = 3);
    fib 4 = first
    && first = 3
    && fib 8 = second
    && second = 21
    && third = fib 16
    && third = 987
    && !num_calls = 3
  ;;
end
