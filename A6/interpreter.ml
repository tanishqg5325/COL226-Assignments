type variable = string
type symbol = string
type signature = (symbol * int) list
type term = V of variable | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list

exception NOT_UNIFIABLE
exception NotFound
exception InvalidProgram

let rec exists x y = match y with
    [] -> false
  | z::ys -> (x = z) || (exists x ys)
;;

let rec foldl f e l = match l with
    [] -> e
  | x::xs -> foldl f (f e x) xs
;;

let rec map f l = match l with
    [] -> []
  | x::xs -> (f x)::map f xs
;;

let rec combine l1 l2 = match l1 with
    [] -> []
  | x::xs -> (x, (List.hd l2))::combine xs (List.tl l2)
;;

let rec find_arity (x:symbol) (y:signature): int = match y with
    [] -> -1
  | z::ys -> if fst z = x then snd z else find_arity x ys
;;

let rec getSigTerm (sign:signature) (t:term): signature = match t with
    Node(s, l) -> let a = find_arity s sign in
                  if a = -1 then foldl getSigTerm ((s, List.length l)::sign) l
                  else if a <> List.length l then raise InvalidProgram
                  else foldl getSigTerm sign l
  | _ -> sign
;;

let getSigAtom (sign:signature) (A(s, l)): signature = getSigTerm sign (Node(s, l))
;;

let rec getSigProgram (prog:program) (sign:signature): signature = match prog with
    [] -> sign
  | (F(H(a)))::xs -> getSigProgram xs (getSigAtom sign a)
  | (R(H(a), B(l)))::xs ->  let f a = F(H(a)) in
                            getSigProgram ((map f (a::l)) @ xs) sign
;;

let rec wfterm (sign:signature) (t:term): bool =
  match t with
      V(v) -> true
    | Node(s, l) -> exists (s, List.length l) sign && foldl (&&) true (map (wfterm sign) l)
;;

let rec wfgoal (g:goal) (sign:signature): bool =
  match g with
      G([]) -> true
    | G(A(s, l)::xs) -> wfterm sign (Node(s, l)) && wfgoal (G(xs)) sign
;;

let rec subst (s:substitution) (t:term): term =
  match t with
      Node(s', l) -> Node(s', map (subst s) l)
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

let rec subst_atom (s:substitution) (A(s', l)) = A(s', map (subst s) l)
;;

let rec variableInTerm (v:variable) (t:term): bool =
  match t with
      V(x) -> x = v
    | Node(s, l) -> foldl (||) false (map (variableInTerm v) l)
;;

let compose (s1:substitution) (s2:substitution): substitution =
  let f s x = (fst x, subst s (snd x)) in (map (f s2) s1) @ s2
;;

let rec mgu_term (t1:term) (t2:term): substitution =
  match (t1, t2) with
      (V(x), V(y)) -> if x = y then []
                      else [(x, V(y))]
    | (V(x), Node(_, _)) -> if variableInTerm x t2 then raise NOT_UNIFIABLE
                            else [(x, t2)]
    | (Node(_, _), V(y)) -> if variableInTerm y t1 then raise NOT_UNIFIABLE
                            else [(y, t1)]
    | (Node(s1, l1), Node(s2, l2)) -> 
        if s1 <> s2 then raise NOT_UNIFIABLE
        else 
          let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
          foldl f [] (combine l1 l2)
;;

let mgu_atom (A(s1, l1)) (A(s2, l2)): substitution = mgu_term (Node(s1, l1)) (Node(s2, l2))
;;

let solve_atom_atom a1 a2 unif =
  try (true, compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2)))
  with NOT_UNIFIABLE -> (false, [])
;;

let rec solve_atom_clause prog a clause unif = match clause with
    F(H(a')) -> solve_atom_atom a a' unif
  | R(H(a'), B(al)) -> match (solve_atom_atom a a' unif) with
       (true, u) -> solve_goal prog (G(al)) u
      | _ -> (false, [])

and solve_goal (prog:program) (goal:goal) (unif:substitution) = match goal with
    G([]) -> (true, unif)
  | G(a::gs) ->
      let rec f prog' = match prog' with
          [] -> (false, [])
        | c::ps ->  match (solve_atom_clause prog a c unif) with
                        (true, u) -> (true, u)
                      | _ -> f ps
      in let ans = f prog in
      if fst ans = true then solve_goal prog (G(gs)) (snd ans)
      else (false, [])
;;
