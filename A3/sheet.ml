type index = INDICE of int * int
type range = RANGE of index * index
type value = FLOAT of float | UNDEFINED
(* A sheet can have both float as well as undefined values *)
type sheet = value list list

(* Unary and Binary Operator tokens *)
type tokens = COUNT | ROWCOUNT | COLCOUNT
            | SUM   | ROWSUM   | COLSUM
            | AVG   | ROWAVG   | COLAVG
            | MIN   | ROWMIN   | COLMIN
            | MAX   | ROWMAX   | COLMAX
            | ADD | SUBT | MULT | DIV

(* return type of parser *)
type formula = 
    UNARY of index * tokens * range
  | BINARY1 of index * tokens * range * range
  | BINARY2 of index * tokens * float * range
  | BINARY3 of index * tokens * index * range

(* Exceptions *)
exception NotPossible
exception InvalidRange
exception IncompatibleRange
exception DivideByZero

(* Function to check whether given range is valid or not *)
let isInvalidRange (r:range): bool = match r with
    RANGE(INDICE(i1, j1), INDICE(i2, j2)) -> (i1 > i2) || (j1 > j2);;

(* Function to check whether two ranges are compatible or not i.e. same number of rows and columns *)
let isIncompatibleRange (r1:range) (r2:range): bool = match r1 with 
    RANGE(INDICE(a1, a2), INDICE(b1, b2)) -> match r2 with
        RANGE(INDICE(c1, c2), INDICE(d1, d2)) -> ((b1-a1) <> (d1-c1)) || ((b2-a2) <> (d2-c2));;

(* Backend *)
let rec full_count (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_count (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_count (s:sheet) (r:range) (i:index): sheet = s;;

let rec full_sum (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_sum (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_sum (s:sheet) (r:range) (i:index): sheet = s;;

let rec full_avg (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_avg (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_avg (s:sheet) (r:range) (i:index): sheet = s;;

let rec full_min (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_min (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_min (s:sheet) (r:range) (i:index): sheet = s;;

let rec full_max (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_max (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_max (s:sheet) (r:range) (i:index): sheet = s;;

let rec add_const (s:sheet) (r:range) (f:float) (i:index): sheet = s;;
let rec subt_const (s:sheet) (r:range) (f:float) (i:index): sheet = s;;
let rec mult_const (s:sheet) (r:range) (f:float) (i:index): sheet = s;;
let rec div_const (s:sheet) (r:range) (f:float) (i:index): sheet = s;;

let rec add_range (s:sheet) (r1:range) (r2:range) (i:index): sheet = s;;
let rec subt_range (s:sheet) (r1:range) (r2:range) (i:index): sheet = s;;
let rec mult_range (s:sheet) (r1:range) (r2:range) (i:index): sheet = s;;
let rec div_range (s:sheet) (r1:range) (r2:range) (i:index): sheet = s;;

let rec getValueAtIndex (s:sheet) (i:index): value =
    match s with
        [] -> UNDEFINED
      | x::xs -> match x with
                    [] -> UNDEFINED
                  | y::ys ->  match i with INDICE(i, j) ->
                                if(i = 0 && j = 0) then y
                                else if(i = 0) then getValueAtIndex (ys::xs) (INDICE(i, j-1))
                                else getValueAtIndex xs (INDICE(i-1, j))
    ;;

let rec fillRowWithUndefined (s:value list) (i1:int) (i2:int): value list =
    match s with
        [] -> []
      | x::xs -> if i1 > 0 then x::(fillRowWithUndefined xs (i1-1) (i2-1))
                 else if i2 < 0 then s
                 else UNDEFINED::fillRowWithUndefined xs 0 (i2-1);;


let rec fillRangeWithUndefined (s:sheet) (r:range): sheet =
    match s with
        [] -> []
      | x::xs -> match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then x::fillRangeWithUndefined xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2)))
          else if i2 < 0 then s
          else (fillRowWithUndefined x j1 j2)::fillRangeWithUndefined xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2)));;

(* Interpreter based on formulas returned by parser *)
let eval (s:sheet) (f:formula): sheet = match f with
    UNARY(i_, t, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match t with
          COUNT     -> full_count s r i_
        | ROWCOUNT  -> row_count s r i_
        | COLCOUNT  -> col_count s r i_
        | SUM       -> full_sum s r i_
        | ROWSUM    -> row_sum s r i_
        | COLSUM    -> col_sum s r i_
        | AVG       -> full_avg s r i_
        | ROWAVG    -> row_avg s r i_
        | COLAVG    -> col_avg s r i_
        | MIN       -> full_min s r i_
        | ROWMIN    -> row_min s r i_
        | COLMIN    -> col_min s r i_
        | MAX       -> full_max s r i_
        | ROWMAX    -> row_max s r i_
        | COLMAX    -> col_max s r i_
        | _         -> raise NotPossible
    )

  | BINARY1(i_, t, r1, r2) -> (
      if (isInvalidRange r1) || (isInvalidRange r2) then raise InvalidRange
      else if (isIncompatibleRange r1 r2) then raise IncompatibleRange
      else match t with
          ADD   -> add_range s r1 r2 i_
        | SUBT  -> subt_range s r1 r2 i_
        | MULT  -> mult_range s r1 r2 i_
        | DIV   -> div_range s r1 r2 i_
        | _     -> raise NotPossible
    )

  | BINARY2(i_, t, c, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match t with
          ADD   -> add_const s r c i_
        | SUBT  -> subt_const s r c i_
        | MULT  -> mult_const s r c i_
        | DIV   -> div_const s r c i_
        | _     -> raise NotPossible
    )

  | BINARY3(i_, t, i, r) -> (
      if (isInvalidRange r) then raise InvalidRange
      else match (getValueAtIndex s i) with
          UNDEFINED -> (
            match r with RANGE(INDICE(ir1, jr1), INDICE(ir2, jr2)) ->
            match i_ with INDICE(i0, j0) ->
              fillRangeWithUndefined s (RANGE(i_, INDICE(i0+ir2-ir1, j0+jr2-jr1)))  
          )
        | FLOAT(c)  -> match t with
                          ADD   -> add_const s r c i_
                        | SUBT  -> subt_const s r c i_
                        | MULT  -> mult_const s r c i_
                        | DIV   -> div_const s r c i_
                        | _     -> raise NotPossible
    )
;;
