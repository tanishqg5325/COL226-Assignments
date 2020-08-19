type var = V of string

type exp = Num of int | Bool of bool
         | Var of var
         | Plus of exp * exp | Times of exp * exp
         | CompWith0 of exp
         | IfThenElse of exp * exp * exp
         | Lambda of var * exp
         | Invoke of exp * exp

type opcode = NUM of int | BOOL of bool 
            | VAR of string 
            | PLUS | TIMES
            | COMP
            | COND of opcode list * opcode list
            | CLOS of string * opcode list | RET
            | INVOKE

type code = opcode list

type answer = N of int | B of bool | VClos of environment * var * code
and
environment = (var * answer) list

type stack = answer list
type dump = (stack * environment * code) list


exception BadState
exception VarNotFound


let rec lookup (e:environment) (v:var): answer = match e with
    [] -> raise VarNotFound
  | (x, a)::es -> if v = x then a else lookup es v
;;

let rec compile (e:exp): opcode list = match e with
    Num(n)                  -> [NUM(n)]
  | Bool(b)                 -> [BOOL(b)]
  | Var(V(s))               -> [VAR(s)]
  | Plus(e1, e2)            -> (compile e1) @ (compile e2) @ [PLUS]
  | Times(e1, e2)           -> (compile e1) @ (compile e2) @ [TIMES]
  | CompWith0(e')           -> (compile e') @ [COMP]
  | IfThenElse(e1, e2, e3)  -> (compile e1) @ [COND((compile e2), (compile e3))]
  | Lambda(V(x), e')        -> [CLOS(x, (compile e') @ [RET])]
  | Invoke(e1, e2)          -> (compile e1) @ (compile e2) @ [INVOKE]
;;

let rec execute (s:stack) (e:environment) (c:code) (d:dump): answer = match (s, c, d) with
    [a], [], []                                -> a
  | _, NUM(n)::c', _                           -> execute (N(n)::s) e c' d
  | _, BOOL(b)::c', _                          -> execute (B(b)::s) e c' d
  | _, VAR(x)::c', _                           -> execute ((lookup e (V(x)))::s) e c' d
  | (N(n2)::N(n1)::s'), PLUS::c', _            -> execute (N(n1+n2)::s') e c' d
  | (N(n2)::N(n1)::s'), TIMES::c', _           -> execute (N(n1*n2)::s') e c' d
  | (N(n)::s'), COMP::c', _                    -> if n >= 0 then execute (B(true)::s') e c' d else execute (B(false)::s') e c' d
  | (B(true)::s'), COND(c1, _)::c', _          -> execute s' e (c1 @ c') d
  | (B(false)::s'), COND(_, c2)::c', _         -> execute s' e (c2 @ c') d
  | _, CLOS(x, c1)::c', _                      -> execute ((VClos(e, V(x), c1))::s) e c' d
  | [a], [RET], (s', e', c')::d'               -> execute (a::s') e' c' d'
  | (a::(VClos(e', v, c1))::s'), INVOKE::c', _ -> execute [] ((v, a)::e') c1 ((s', e, c')::d)
  | _                                          -> raise BadState
;;

let secd (e:environment) (ex:exp): answer = execute [] e (compile ex) []
;;


secd [] (Num(9));;
secd [] (Bool(false));;
secd [(V "z", N(124))] (Var(V "z"));;
secd [(V "z", N(124))] (Var(V "x"));;
secd [(V "z", B(true))] (Var(V "z"));;

secd [] (Plus(Num(4), Num(7)));;
secd [] (Times(Num(4), Bool(false)));;
secd [(V "a", N(8)); (V "b", N(9))] (Plus(Num(3), Times(Var(V "a"), Var(V "b"))));;
secd [(V "a", N(8)); (V "b", N(9))] (Times(Num(3), Plus(Var(V "a"), Var(V "b"))));;

secd [] (CompWith0(Plus(Num(3), Num(-3))));;
secd [] (CompWith0(Plus(Num(3), Num(-5))));;
secd [(V "p", N(96))] (CompWith0(Times(Num(0), Var(V "p"))));;

secd [(V "a", N(1))] (IfThenElse(CompWith0(Var(V "a")), Num(5), Num(7)));;
secd [(V "a", N(-1))] (IfThenElse(CompWith0(Var(V "a")), Num(5), Num(7)));;
secd [(V "a", N(-6))] (IfThenElse(CompWith0(Var(V "a")), Var(V "a"), Num(0)));; (* Max(a, 0) *)

secd [] (Invoke(Lambda(V "x", Times(Var(V "x"), Num(5))), Plus(Num(2), Num(4))));;
let f = secd [(V "y", N(2))] (Lambda(V "x", Plus(Var(V "x"), Var(V "y"))));;
secd [(V "f", f); (V "y", N(8))] (Invoke(Var(V "f"), Var(V "y")));;
let sq = secd [] (Lambda(V "x", Times(Var(V "x"), Var(V "x"))));;
secd [(V "sq", sq)] (Invoke(Var(V "sq"), Num(35)));;
let max0 = secd [(V "sq", sq)] (Lambda(V "x", IfThenElse(CompWith0(Var(V "x")), Invoke(Var(V "sq"), Var(V "x")), Num(0))));;
secd [(V "max0", max0); (V "m", N(9))] (Times(Num(3), Invoke(Var(V "max0"), Var(V "m"))));;
