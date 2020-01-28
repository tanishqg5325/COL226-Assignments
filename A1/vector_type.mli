module type Vector_type = sig
  type vector = float list

  (* Dimension *)
  val vdim: vector -> int
  (* Zero Vector *)
  val mkzerov: int -> vector
  (* isZeroVector *)
  val isvzerov: vector -> bool
  (* Addition *)
  val addv: vector -> vector -> vector
  (* Scaler Multiplication *)
  val scalermultv: float -> vector -> vector
  (* Dot Product *)
  val dotprodv: vector -> vector -> float
  (* Cross Product *)
  (* val crossprodv: vector -> vector -> vector *)

  (* Define suitable exceptions when an operation is not defined. *)
end
