type var = V of string

type exp = Num of int | Bool of bool
         | Var of var
         | Plus of exp * exp | Times of exp * exp
         | CompWith0 of exp
         | Cond of exp * exp * exp
         | Lambda of var * exp
         | Invoke of exp * exp

type table = (var * closure) list
and
closure = Clos of table * exp

type answer = N of int | B of bool | VClos of table * var * exp

type stack = closure list


exception BadState
exception VarNotFound


let rec lookup (t:table) (v:var): closure = match t with
    [] -> raise VarNotFound
  | (x, c)::ts -> if v = x then c else lookup ts v
;;

let add (c1: closure) (c2: closure): exp = match (c1, c2) with
    Clos(_, Num(n1)), Clos(_, Num(n2)) -> Num(n1 + n2)
  | _ -> raise BadState
;;

let mul (c1: closure) (c2: closure): exp = match (c1, c2) with
    Clos(_, Num(n1)), Clos(_, Num(n2)) -> Num(n1 * n2)
  | _ -> raise BadState
;;

let comp0 (c:closure): exp = match c with
    Clos(_, Num(n)) -> Bool(n = 0)
  | _ -> raise BadState
;;

let ite (c:closure): bool = match c with
    Clos(_, Bool(b)) -> b
  | _ -> raise BadState
;;

let rec execute (c:closure) (s:stack): closure = match (c, s) with
    Clos(_, Num(_)), _            -> c
  | Clos(_, Bool(_)), _           -> c
  | Clos(t, Var(v)), _            -> execute (lookup t v) s
  | Clos(t, Plus(e1, e2)), _      -> Clos(t, add (execute (Clos(t, e1)) s) (execute (Clos(t, e2)) s))
  | Clos(t, Times(e1, e2)), _     -> Clos(t, mul (execute (Clos(t, e1)) s) (execute (Clos(t, e2)) s))
  | Clos(t, CompWith0(e)), _      -> Clos(t, comp0 (execute (Clos(t, e)) s))
  | Clos(t, Cond(e1, e2, e3)), _  -> if ite (execute (Clos(t, e1)) s) then execute (Clos(t, e2)) s else execute (Clos(t, e3)) s
  | Clos(_, Lambda(_, _)), []     -> c
  | Clos(t, Lambda(v, e)), c'::s' -> execute (Clos((v, c')::t, e)) s'
  | Clos(t, Invoke(e1, e2)), _    -> execute (Clos(t, e1)) (Clos(t, e2)::s)
;;

let krivine (t:table) (e:exp): answer = match (execute (Clos(t, e)) []) with
    Clos(_, Num(n))               -> N(n)
  | Clos(_, Bool(b))              -> B(b)
  | Clos(t, Lambda(v, ex))        -> VClos(t, v, ex)
  | _                             -> raise BadState
;;
