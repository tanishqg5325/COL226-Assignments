module type Matrix_type = sig
  type matrix = (float list) list

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
  val scalermultm: float -> matrix -> matrix
  (* Matrix Multiplication *)
  val multm: matrix -> matrix -> matrix
  (* Transpose Matrix *)
  val transm: matrix -> matrix
  (* Determinant of Matrix *)
  val detm: matrix -> float
  (* Inverse of Matrix *)
  val makeFirstRowNonZero: matrix -> matrix
end
