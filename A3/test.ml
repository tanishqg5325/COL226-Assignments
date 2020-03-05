#directory "_build";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "sheet.cmo";;

open A2;;
open A3;;
open Sheet;;

let parser s = A3.main A2.read (Lexing.from_string s);;

parser "[0,5]:= COLCOUNT ([0,4]:[2,4]) 10.0;";;
