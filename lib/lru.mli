module DList = Double_linked

type ('k, 'v) t =
  { cache : ('k, ('k * 'v) DList.t) Hashtbl.t
  ; mutable list : ('k * 'v) DList.t
  ; mutable least_used : ('k * 'v) DList.t
  ; threshold : int
  ; mutable size : int
  }

val create : int -> ('k, 'v) t
