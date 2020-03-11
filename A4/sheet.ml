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
exception RangeNotInSheet
exception CellNotInitialized

(* Function to check whether given range is valid or not *)
let isInvalidRange (r:range): bool = match r with
    RANGE(INDICE(i1, j1), INDICE(i2, j2)) -> (i1 > i2) || (j1 > j2);;

(* Function to check whether two ranges are compatible or not i.e. same number of rows and columns *)
let isIncompatibleRange (r1:range) (r2:range): bool = match r1 with 
    RANGE(INDICE(a1, a2), INDICE(b1, b2)) -> match r2 with
        RANGE(INDICE(c1, c2), INDICE(d1, d2)) -> ((b1-a1) <> (d1-c1)) || ((b2-a2) <> (d2-c2));;

let rec full_ans (s:sheet) (r:range) f (e:float): float =
  match s with
      [] -> (
        match r with RANGE(i, INDICE(i2, _)) ->
          if i2 < 0 then e else raise RangeNotInSheet
      )
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then full_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
          else if i2 < 0 then e
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
                match v with 
                    [] -> if i2 < 0 then e else raise RangeNotInSheet
                  | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                            else if i2 < 0 then e
                            else match y with
                                FLOAT(c) -> f c (full_row_ans ys 0 (i2-1))
                              | UNDEFINED -> raise CellNotInitialized
            in f (full_row_ans x j1 j2) (full_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e);;

let rec row_ans (s:sheet) (r:range) f (e:float): float list =
  match s with
      [] -> (
        match r with RANGE(i, INDICE(i2, _)) ->
          if i2 < 0 then [] else raise RangeNotInSheet
      )
    | x::xs ->
        match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
          if i1 > 0 then row_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
          else if i2 < 0 then []
          else
            let rec full_row_ans (v:value list) (i1:int) (i2:int): float =
              match v with
                  [] -> if i2 < 0 then e else raise RangeNotInSheet
                | y::ys -> if i1 > 0 then full_row_ans ys (i1-1) (i2-1)
                          else if i2 < 0 then e
                          else match y with
                              FLOAT(c) -> f c (full_row_ans ys 0 (i2-1))
                            | UNDEFINED -> raise CellNotInitialized
            in (full_row_ans x j1 j2)::row_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e;;

let rec mkzerov (n:int) (e:float): float list = if n = 0 then [] else e::mkzerov (n-1) e;;

let rec col_ans (s:sheet) (r:range) f (e:float): float list =
    match s with
        [] -> (
          match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
            if i2 < 0 then (mkzerov (j2-j1+1) e) else raise RangeNotInSheet
        )
      | x::xs ->
          match r with RANGE(INDICE(i1, j1), INDICE(i2, j2)) ->
            if i1 > 0 then col_ans xs (RANGE(INDICE(i1-1, j1), INDICE(i2-1, j2))) f e
            else if i2 < 0 then (mkzerov (j2-j1+1) e)
            else
              let rec merge (v1:value list) (v2:float list) (i1:int) (i2:int): float list =
                  if i1 > 0 then merge v1 v2 (i1-1) (i2-1)
                  else if i2 < 0 then []
                  else match v1 with 
                      [] -> raise RangeNotInSheet
                    | v::vs -> match v with
                          FLOAT(c) -> (f c (List.hd v2))::merge vs (List.tl v2) 0 (i2-1)
                        | UNDEFINED -> raise CellNotInitialized
              in merge x (col_ans xs (RANGE(INDICE(0, j1), INDICE(i2-1, j2))) f e) j1 j2;;

let rec expandSheet (s:sheet) (h:int) (l:int): sheet =
  let rec makeUndefinedRow (n:int): value list = if n = 0 then [] else UNDEFINED::makeUndefinedRow (n-1)
  in match s with
        [] -> if h = 0 then s else (makeUndefinedRow l)::expandSheet s (h-1) l
      | x::xs -> if l = (List.length x) then x::expandSheet xs (h-1) l
                 else (x @ (makeUndefinedRow (l-(List.length x))))::expandSheet xs (h-1) l;;

let rec writeCell (s:sheet) (i_:index) (c:float): sheet =
    match i_ with INDICE(i, j) ->
      match s with
          [] -> []
        | x::xs ->
            if i > 0 then x::writeCell xs (INDICE(i-1, j)) c
            else
              let rec colWriteCell (v:value list) (j:int): value list =
                if j = 0 then FLOAT(c)::(List.tl v)
                else (List.hd v)::colWriteCell (List.tl v) (j-1)
              in (colWriteCell x j)::xs;;

let rec writeRow (s:sheet) (i_:index) (c:float list): sheet =
  match i_ with INDICE(i, j) ->
    match s with
        [] -> []
      | x::xs ->
          if i > 0 then x::writeRow xs (INDICE(i-1, j)) c
          else
            let rec writeRowUtil (v:value list) (j:int) (c:float list): value list =
              if j > 0 then (List.hd v)::writeRowUtil (List.tl v) (j-1) c
              else if (List.length c) = 0 then v
              else FLOAT((List.hd c))::writeRowUtil (List.tl v) 0 (List.tl c)
            in (writeRowUtil x j c)::xs;;

let rec writeCol (s:sheet) (i_:index) (c:float list): sheet =
    match s with
        [] -> []
      | x::xs ->
          match i_ with INDICE(i, j) ->
            if i > 0 then x::writeCol xs (INDICE(i-1, j)) c
            else if (List.length c) = 0 then s
            else 
              let rec colWriteCell (v:value list) (j:int) (c:float): value list =
                if j = 0 then FLOAT(c)::(List.tl v)
                else (List.hd v)::colWriteCell (List.tl v) (j-1) c
              in (colWriteCell x j (List.hd c))::writeCol xs i_ (List.tl c);;

(* Backend *)
let rec full_count (s:sheet) (r:range) (i:index): sheet = s;;
let rec row_count (s:sheet) (r:range) (i:index): sheet = s;;
let rec col_count (s:sheet) (r:range) (i:index): sheet = s;;

let rec full_sum (s:sheet) (r:range) (i_:index): sheet = 
    let f a b = a +. b in 
    let ans = full_ans s r f 0. in
    match i_ with INDICE(i, j) ->
      if i >= List.length s || j >= List.length (List.hd s) then 
        let new_h = max (i+1) (List.length s) in
        let new_l = max (j+1) (List.length (List.hd s)) in
        writeCell (expandSheet s new_h new_l) i_ ans
      else writeCell s i_ ans;;

let rec row_sum (s:sheet) (r:range) (i_:index): sheet =
  let f a b = a +. b in 
    let ans = row_ans s r f 0. in
    match i_ with INDICE(i, j) ->
      match r with RANGE(INDICE(i1, _), INDICE(i2, _)) ->
      if i+i2-i1 >= List.length s || j >= List.length (List.hd s) then
        let new_h = max (i+i2-i1+1) (List.length s) in
        let new_l = max (j+1) (List.length (List.hd s)) in
        writeCol (expandSheet s new_h new_l) i_ ans
      else writeCol s i_ ans;;

let rec col_sum (s:sheet) (r:range) (i_:index): sheet =
  let f a b = a +. b in 
  let ans = col_ans s r f 0. in
  match i_ with INDICE(i, j) ->
    match r with RANGE(INDICE(_, j1), INDICE(_, j2)) ->
    if i >= List.length s || j+j2-j1 >= List.length (List.hd s) then
      let new_h = max (i+1) (List.length s) in
      let new_l = max (j+j2-j1+1) (List.length (List.hd s)) in
      writeRow (expandSheet s new_h new_l) i_ ans
    else writeRow s i_ ans;;

let rec full_avg (s:sheet) (r:range) (i_:index): sheet = s;;
let rec row_avg (s:sheet) (r:range) (i_:index): sheet = s;;
let rec col_avg (s:sheet) (r:range) (i_:index): sheet = s;;

let rec full_min (s:sheet) (r:range) (i_:index): sheet = s;;
let rec row_min (s:sheet) (r:range) (i_:index): sheet = s;;
let rec col_min (s:sheet) (r:range) (i_:index): sheet = s;;

let rec full_max (s:sheet) (r:range) (i_:index): sheet = s;;
let rec row_max (s:sheet) (r:range) (i_:index): sheet = s;;
let rec col_max (s:sheet) (r:range) (i_:index): sheet = s;;

let rec add_const (s:sheet) (r:range) (f:float) (i_:index): sheet = s;;
let rec subt_const (s:sheet) (r:range) (f:float) (i_:index): sheet = s;;
let rec mult_const (s:sheet) (r:range) (f:float) (i_:index): sheet = s;;
let rec div_const (s:sheet) (r:range) (f:float) (i_:index): sheet = s;;

let rec add_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet = s;;
let rec subt_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet = s;;
let rec mult_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet = s;;
let rec div_range (s:sheet) (r1:range) (r2:range) (i_:index): sheet = s;;

(* function which returns the value at given index in given sheet *)
let rec getValueAtIndex (s:sheet) (i:index): value =
    match s with
        [] -> UNDEFINED
      | x::xs -> match x with
                    [] -> UNDEFINED
                  | y::ys ->  match i with INDICE(i, j) ->
                                if(i = 0 && j = 0) then y
                                else if(i = 0) then getValueAtIndex (ys::xs) (INDICE(i, j-1))
                                else getValueAtIndex xs (INDICE(i-1, j));;

(* Function to fill cells between i1 and i2 of a row of a sheet with value = undefined *)
let rec fillRowWithUndefined (s:value list) (i1:int) (i2:int): value list =
    match s with
        [] -> []
      | x::xs -> if i1 > 0 then x::(fillRowWithUndefined xs (i1-1) (i2-1))
                 else if i2 < 0 then s
                 else UNDEFINED::fillRowWithUndefined xs 0 (i2-1);;


(* Function to fill given range in given sheet with value = undefined *)
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
