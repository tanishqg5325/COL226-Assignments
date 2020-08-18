type var = V of string

type exp = Num of int | Bool of bool
         | Var of var
         | Plus of exp * exp | Times of exp * exp
         | Cond of exp * exp * exp
         | Invoke of exp * exp
         | Lambda of var * exp

type answer = N of int | B of bool | VClos of environment * var * exp
and
environment = (var * answer) list

type opcode = NUM of int | BOOL of bool 
            | VAR of string 
            | PLUS | TIMES
            | COND of opcode list * opcode list
            | CLOS of string * opcode list | RET
            | APP

type stack = answer list
type code = opcode list
type dump = (stack * environment * code) list


let rec compile (e:exp): opcode list = match e with
    Num(n) -> [NUM(n)]
  | Bool(b) -> [BOOL(b)]
  | Var(V(s)) -> [VAR(s)]
  | Plus(e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Times(e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
  | Cond(e1, e2, e3) -> (compile e1) @ [COND((compile e2), (compile e3))]
  | Invoke(e1, e2) -> (compile e1) @ (compile e2) @ [APP]
  | Lambda(V(x), e') -> [CLOS(x, (compile e') @ [RET])]
;;


let rec execute (s:stack) (e:environment) (c:code) (d:dump): answer =

