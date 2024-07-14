module type Number = sig
  type t

  val to_float : t -> float
  val of_float : float -> t
  val mul : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val sqrt : t -> float
end

module FloatNumber : Number = struct
  include Float

  let to_float = Fun.id
  let of_float = Fun.id
end

module IntNumber : Number = struct
  include Int

  let sqrt =
    let open Util in
    to_float >> Float.sqrt
  ;;
end

module Point (N : Number) = struct
  type t =
    { x : N.t
    ; y : N.t
    }

  let distance { x = x1; y = y1 } { x = x2; y = y2 } =
    let dx = N.sub x1 x2
    and dy = N.sub y1 y2 in
    N.sqrt @@ N.add (N.mul dx dx) (N.mul dy dy)
  ;;
end
