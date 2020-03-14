#directory "_build";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "sheet.cmo";;

open A2;;
open A3;;
open Sheet;;

let applyFormula (t:sheet) (s:string): sheet =
  eval t (A3.main A2.read (Lexing.from_string s));;

let s0 = [[FLOAT(1.0); FLOAT(10.0); FLOAT(100.0)]; [FLOAT(2.0); FLOAT(20.0); FLOAT(200.0)]; [FLOAT(3.0); FLOAT(30.0); FLOAT(300.0)]];;
let s1 = applyFormula s0 "[0, 3] := ADD ([0, 0] : [2, 0]) ([0, 1] : [2, 1]);";;
let s2 = applyFormula s1 "[0, 4] := MULT ([0, 0] : [2, 0]) ([0, 2] : [2, 2]);";;
let s3 = applyFormula s2 "[0, 5] := ADD ([0, 4] : [2, 4]) 10.0;";;
let s4 = applyFormula s3 "[0, 3] := DIV ([0, 4] : [2, 4]) [1, 1];";;
let s5 = applyFormula s4 "[3, 0] := SUM ([0, 0] : [2, 2]);";;
let s6 = applyFormula s5 "[3, 1] := COUNT ([3, 0] : [3, 4]);";;
