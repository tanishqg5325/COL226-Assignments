  type sheet = float list list
  type index = INDICE of int * int
  type range = RANGE of index * index

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
