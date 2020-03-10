#directory "_build";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "sheet.cmo";;

open A2;;
open A3;;
open Sheet;;

let f0 = A3.main A2.read (Lexing.from_string "[0,3]:=SUM ([1, 4] : [4, 5]);\n");;
let f1 = A3.main A2.read (Lexing.from_string "[0,3]:=ADD ([1, 4] : [4, 6]) ([2, 5] : [5, 7]);");;
let f2 = A3.main A2.read (Lexing.from_string "[0,3]:=MULT 44.24 ([1, 4] : [4, 4]);");;
let f3 = A3.main A2.read (Lexing.from_string "[0,3]:=MULT ([1, 4] : [4, 6]) 10.5;");;
let f4 = A3.main A2.read (Lexing.from_string "[1,0] := MULT [0, 1] ([0, 0] : [0, 1]) ;");;
let f5 = A3.main A2.read (Lexing.from_string "[5,3] := MULT ([1, 4] :  [4, 6 ]  ) [  104  , 89 ];");;

let s = [[FLOAT(4.1); UNDEFINED]; [FLOAT(-10.9); FLOAT(1.7)]];;
eval s f0;;
eval s f1;;
eval s f2;;
eval s f3;;
eval s f4;;
eval s f5;;

let t = [[FLOAT(4.1); FLOAT(1.2); FLOAT(0.0)]; [FLOAT(-10.9); FLOAT(1.7); FLOAT(2.3)]; [FLOAT(-1.4); FLOAT(4.5); FLOAT(3.2)]];;
fillRangeWithUndefined t (RANGE(INDICE(0, 0), INDICE(1, 3)));;

let f a b = a +. b;;
full_ans t (RANGE(INDICE(0, 0), INDICE(2, 2))) max (-189.);;

writeCol (expandSheet t 4 3) (INDICE(1, 1)) [2.8; 50.];;

let fo = A3.main A2.read (Lexing.from_string "[2,3]:=SUM ([0, 0] : [2, 2]);\n");;
eval t fo;;
