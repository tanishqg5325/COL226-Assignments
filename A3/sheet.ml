type sheet = float list list
type index = INDICE of int * int
type range = RANGE of index * index

type token = COUNT | ROWCOUNT | COLCOUNT
           | SUM   | ROWSUM   | COLSUM
           | AVG   | ROWAVG   | COLAVG
           | MIN   | ROWMIN   | COLMIN
           | MAX   | ROWMAX   | COLMAX
           | ADD | SUBT | MULT | DIV

type formula = 
    UNARY of index * token * range
  | BINARY1 of index * token * range * range
  | BINARY2 of index * token * float * range
  | BINARY3 of index * token * range * float
  | BINARY4 of index * token * index * range
  | BINARY5 of index * token * range * index

exception NotPossible
exception InvalidRange
exception IncompatibleRange
exception DivideByZero

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

let eval (s:sheet) (f:formula): sheet = match f with
    UNARY(i_, t, r) -> (
      match t with
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
      match t with
          ADD   -> add_range s r1 r2 i_
        | SUBT  -> subt_range s r1 r2 i_
        | MULT  -> mult_range s r1 r2 i_
        | DIV   -> div_range s r1 r2 i_
        | _     -> raise NotPossible
    )
  
  | BINARY2(i_, t, c, r) -> (
      match t with
          ADD   -> s
        | SUBT  -> s
        | MULT  -> s
        | DIV   -> s
        | _     -> raise NotPossible
    )

  | BINARY3(i_, t, r, c) -> (
      match t with
          ADD   -> s
        | SUBT  -> s
        | MULT  -> s
        | DIV   -> s
        | _     -> raise NotPossible
    )

  | BINARY4(i_, t, i, r) -> (
      match t with
          ADD   -> s
        | SUBT  -> s
        | MULT  -> s
        | DIV   -> s
        | _     -> raise NotPossible
    )

  | BINARY5(i_, t, r, i) -> (
      match t with
          ADD   -> s
        | SUBT  -> s
        | MULT  -> s
        | DIV   -> s
        | _     -> raise NotPossible
    )
;;
