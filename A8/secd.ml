type var = V of string

type exp = Num of int | Bool of bool
         | Var of var
         | Plus of exp * exp | Times of exp * exp
         | Cond of exp * exp * exp
         | Lambda of var * exp
         | Invoke of exp * exp

type opcode = NUM of int | BOOL of bool 
            | VAR of string 
            | PLUS | TIMES
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
    Num(n) -> [NUM(n)]
  | Bool(b) -> [BOOL(b)]
  | Var(V(s)) -> [VAR(s)]
  | Plus(e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Times(e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
  | Cond(e1, e2, e3) -> (compile e1) @ [COND((compile e2), (compile e3))]
  | Lambda(V(x), e') -> [CLOS(x, (compile e') @ [RET])]
  | Invoke(e1, e2) -> (compile e1) @ (compile e2) @ [INVOKE]
;;


let rec execute (s:stack) (e:environment) (c:code) (d:dump): answer = match (s, e, c, d) with
    ([a], _, [], []) -> a
  | (_, _, NUM(n)::c', _) -> execute (N(n)::s) e c' d
  | (_, _, BOOL(b)::c', _) -> execute (B(b)::s) e c' d
  | (_, _, VAR(x)::c', _) -> execute ((lookup e (V(x)))::s) e c' d
  | ((N(n2)::N(n1)::s'), _, PLUS::c', _) -> execute (N(n1+n2)::s') e c' d
  | ((N(n2)::N(n1)::s'), _, TIMES::c', _) -> execute (N(n1*n2)::s') e c' d
  | ((B(true)::s'), _, COND(c1, _)::c', _) -> execute s' e (c1 @ c') d
  | ((B(false)::s'), _, COND(_, c2)::c', _) -> execute s' e (c2 @ c') d
  | (_, _, CLOS(x, c1)::c', _) -> execute ((VClos(e, V(x), c1))::s) e c' d
  | ([a], _, [RET], (s', e', c')::d') -> execute (a::s') e' c' d'
  | ((a::(VClos(e', v, c1))::s'), _, INVOKE::c', _) -> execute [] ((v, a)::e') c1 ((s', e, c')::d)
  | _ -> raise BadState
;;
