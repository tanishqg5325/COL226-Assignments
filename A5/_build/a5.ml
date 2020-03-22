type variable = string
type symbol = string
type term = V of variable | Node of symbol * (term list)
type substitution = (variable * term) list

exception NOT_UNIFIABLE

(* Given an element x and a list y, checks whether x is present in y *)
let rec exists x y = match y with
    [] -> false
  | z::ys -> (x = z) || (exists x ys)
;;

(*
 * Given a signature consisting of symbols and their arities (>= 0) as a list of
 * (symbol, arity) pairs, checks whether the signature is a valid signature (no
 * repeated symbols, arities are non-negative etc.) 
 *)
let rec check_sig (signature:(symbol * int) list): bool =
  match signature with
      [] -> true
    | (s, arity)::xs ->
        let rec repeats (s:symbol) t: bool = match t with
            [] -> false
          | (s', _)::ys -> (s = s') || (repeats s ys)
        in (arity >= 0) && (not (repeats s xs)) && (check_sig xs)
;;

(*
 * Given a valid signature (checked using check_sig), wfterm checks that a given
 * term is well-formed according to the signature
 *)
let rec wfterm (signature:(symbol * int) list) (t:term): bool =
  match t with
      V(v) -> true
    | Node(s, l) -> exists (s, List.length l) signature && List.for_all (wfterm signature) l
;;

(* Given a well-formed term, return its height *)
let rec ht (t:term): int =
  match t with
      V(v) -> 0
    | Node(s, l) -> 1 + (List.fold_left max (-1) (List.map ht l))
;;

(* Given a well-formed term, return its size *)
let rec size (t:term): int =
  match t with
      V(v) -> 1
    | Node(s, l) -> 1 + (List.fold_left (+) 0 (List.map size l))
;;

(*
 * Given a well-formed term, return set (represented as a list with no duplicates)
 * of variables appearing in it
 *)
let rec vars (t:term): variable list =
  match t with
      V(v) -> [v]
    | Node(s, l) ->
        let rec union l1 l2 = match l1 with
            [] -> l2
          | x::xs -> if (exists x l2) then union xs l2
                     else x::(union xs l2)
        in List.fold_left union [] (List.map vars l)
;;

(*
 * Given a term t and a substitution s, applies the (Unique Homomorphic
 * Extension of) s to t
 *)
let rec subst (s:substitution) (t:term): term =
  match t with
      Node(s', l) -> Node(s', List.map (subst s) l)
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then subst xs (snd s')
                            else subst xs t
;;

(* Given a variable v, checks whether v is present in well-formed term t *)
let rec variableInTerm (v:variable) (t:term): bool =
  match t with
      V(x) -> x = v
    | Node(s, l) -> List.exists (variableInTerm v) l
;;

(*
 * Given two well-formed terms t1 and t2, returns their most general unifier,
 * if it exists and otherwise raises an exception NOT_UNIFIABLE
 *)
let rec mgu (t1:term) (t2:term): substitution =
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
          let f s tt = s @ (mgu (subst s (fst tt)) (subst s (snd tt))) in
          List.fold_left f [] (List.combine l1 l2)
;;
