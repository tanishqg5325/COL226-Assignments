type var = V of string

type exp = Num of int | Bool of bool
         | Var of var
         | Plus of exp * exp | Times of exp * exp
         | CompWith0 of exp
         | Cond of exp * exp * exp
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
    Num(n)              -> [NUM(n)]
  | Bool(b)             -> [BOOL(b)]
  | Var(V(s))           -> [VAR(s)]
  | Plus(e1, e2)        -> (compile e1) @ (compile e2) @ [PLUS]
  | Times(e1, e2)       -> (compile e1) @ (compile e2) @ [TIMES]
  | CompWith0(e')       -> (compile e') @ [COMP]
  | Cond(e1, e2, e3)    -> (compile e1) @ [COND((compile e2), (compile e3))]
  | Lambda(V(x), e')    -> [CLOS(x, (compile e') @ [RET])]
  | Invoke(e1, e2)      -> (compile e1) @ (compile e2) @ [INVOKE]
;;

let rec execute (s:stack) (e:environment) (c:code) (d:dump): answer = match (s, c, d) with
    ([a], [], [])                                -> a
  | (_, NUM(n)::c', _)                           -> execute (N(n)::s) e c' d
  | (_, BOOL(b)::c', _)                          -> execute (B(b)::s) e c' d
  | (_, VAR(x)::c', _)                           -> execute ((lookup e (V(x)))::s) e c' d
  | ((N(n2)::N(n1)::s'), PLUS::c', _)            -> execute (N(n1+n2)::s') e c' d
  | ((N(n2)::N(n1)::s'), TIMES::c', _)           -> execute (N(n1*n2)::s') e c' d
  | ((N(n)::s'), COMP::c', _)                    -> if n = 0 then execute (B(true)::s') e c' d else execute (B(false)::s') e c' d
  | ((B(true)::s'), COND(c1, _)::c', _)          -> execute s' e (c1 @ c') d
  | ((B(false)::s'), COND(_, c2)::c', _)         -> execute s' e (c2 @ c') d
  | (_, CLOS(x, c1)::c', _)                      -> execute ((VClos(e, V(x), c1))::s) e c' d
  | ([a], [RET], (s', e', c')::d')               -> execute (a::s') e' c' d'
  | ((a::(VClos(e', v, c1))::s'), INVOKE::c', _) -> execute [] ((v, a)::e') c1 ((s', e, c')::d)
  | _                                            -> raise BadState
;;

let secd (e:environment) (ex:exp): answer = execute [] e (compile ex) []
;;


secd [] (Invoke(Lambda(V("x"), Times(Var(V("x")), Num(5))), Plus(Num(2), Num(4))));;
secd [(V("f"), VClos([(V("y"), N(2))], V("x"), (compile (Plus(Var(V("x")), Var(V("y"))))) @ [RET]))] (Invoke(Var(V("f")), Num(5)));;
let sq = secd [] (Lambda(V("x"), Times(Var(V("x")), Var(V("x")))));;
secd [(V("sq"), sq)] (Invoke(Var(V("sq")), Num(37)));;
