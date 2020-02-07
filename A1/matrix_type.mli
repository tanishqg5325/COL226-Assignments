module type Matrix_type = sig
  type vector = float list
  type matrix = float list list

  val vdim: vector -> int
  val mkzerov: int -> vector
  val iszerov: vector -> bool
  val addv: vector -> vector -> vector
  val scalarmultv: float -> vector -> vector
  val dotprodv: vector -> vector -> float
  val crossprodv: vector -> vector -> vector
  val crossprodvn: matrix -> vector

  (* Dimension *)
  val mdim: matrix -> int * int
  (* Zero Matrix *)
  val mkzerom: int -> int -> matrix
  (* IsZeroMatrix *)
  val iszerom: matrix -> bool
  (* Unit Matrix *)
  val mkunitm: int -> matrix
  (* IsUnitMatrix *)
  val isunitm: matrix -> bool
  (* Add Matrices *)
  val addm: matrix -> matrix -> matrix
  (* Scaler Multiplication *)
  val scalarmultm: float -> matrix -> matrix
  (* Matrix Multiplication *)
  val multm: matrix -> matrix -> matrix
  (* Transpose Matrix *)
  val transm: matrix -> matrix
  (* Determinant of Matrix *)
  val detm: matrix -> float
  (* Inverse of Matrix *)
  val invm: matrix -> matrix
end
