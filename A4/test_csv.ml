#directory "/home/tanishq/.opam/default/lib/csv";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "sheet.cmo";;
#load "csv.cma";;

open A2;;
open A3;;
open Sheet;;
open Csv;;

(* eval `opam env` *)

let applyFormula (t:sheet) (s:string): sheet =
  eval t (A3.main A2.read (Lexing.from_string s));;

let rec convertCsvtoSheet c = match c with
    [] -> []
  | x::xs ->
      let rec rowUtil c = match c with
          [] -> []
        | y::ys -> FLOAT((float_of_string y))::rowUtil ys
      in (rowUtil x)::convertCsvtoSheet xs;;

let rec getWidthofSheet s = match s with
    [] -> 0
  | x::xs -> max (List.length x) (getWidthofSheet xs);;

let s = convertCsvtoSheet (Csv.load "example.csv");;
let s0 = expandSheet s (List.length s) (getWidthofSheet s);;
let s1 = applyFormula s0 "[0, 3] := ADD ([0, 0] : [2, 0]) ([0, 1] : [2, 1]);";;
let s2 = applyFormula s1 "[0, 4] := MULT ([0, 0] : [2, 0]) ([0, 2] : [2, 2]);";;
let s3 = applyFormula s2 "[0, 5] := ADD ([0, 4] : [2, 4]) 10.0;";;
let s4 = applyFormula s3 "[0, 3] := DIV ([0, 4] : [2, 4]) [1, 1];";;
