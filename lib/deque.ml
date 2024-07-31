open Core

type 'a t =
  { mutable data : 'a option array
  ; mutable head : int
  ; mutable length : int
  ; mutable capacity : int
  }

let round_to_power_of_2 n =
  if n < 8
  then 8
  else (
    let rec find_next_power n =
      if n land (n - 1) = 0 then n else find_next_power @@ succ (n lor (n lsr 1))
    in
    let lower = find_next_power (n lsr 1) in
    let upper = find_next_power n in
    if n - lower < upper - n then lower else upper)
;;

let create ~capacity =
  { data = Array.create ~len:capacity None
  ; head = 0
  ; length = 0
  ; capacity = round_to_power_of_2 capacity
  }
;;

let is_empty self = self.length = 0
let is_full self = self.length = Array.length self.data

let resize self ~size =
  let new_data = Array.create ~len:size None in
  Array.iteri
    ~f:(fun idx _ ->
      let original_idx = self.head + (idx mod self.capacity) in
      new_data.(idx) <- self.data.(original_idx))
    self.data;
  self.data <- new_data;
  self.head <- 0
;;

let grow self =
  let size = max 1 (int_of_float (float self.capacity *. 1.5)) in
  resize self ~size
;;

let shrink self =
  let size = max 1 (self.capacity / 2) in
  if size >= self.length then resize self ~size
;;

let push_front self x =
  let new_head = (self.head - 1 + self.capacity) mod self.capacity in
  self.data.(new_head) <- Some x;
  self.head <- new_head;
  if not @@ is_full self then self.length <- succ self.length
;;

let push_back self x =
  if self.length = self.capacity then grow self;
  let rear = (self.head + self.length) mod self.capacity in
  self.data.(rear) <- Some x;
  self.length <- succ self.length
;;

let pop_front self =
  if self.length = 0
  then None
  else (
    let x = self.data.(self.head) in
    self.data.(self.head) <- None;
    self.head <- succ self.head mod self.capacity;
    self.length <- pred self.length;
    if self.length <= self.capacity / 4 then shrink self;
    x)
;;

let pop_back self =
  if self.length = 0
  then None
  else (
    let rear = (self.head + self.length) mod self.capacity in
    let x = self.data.(rear) in
    self.data.(rear) <- None;
    self.length <- pred self.length;
    if self.length < self.capacity / 4 then shrink self;
    x)
;;
