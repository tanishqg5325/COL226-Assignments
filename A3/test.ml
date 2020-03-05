#directory "_build";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "sheet.cmo";;

open A2;;
open A3;;
open Sheet;;

A3.main A2.read (Lexing.from_string "[0,3]:=MULT ([1, 4] : [4, 6]) 10.0;");;
