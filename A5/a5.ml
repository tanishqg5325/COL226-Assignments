type variable = string
type symbol = string
type term = V of variable | Node of symbol * (term list)
type substitution = (variable * term) list

exception NOT_UNIFIABLE

let rec exists x y = match y with
    [] -> false
  | z::ys -> (x = z) || (exists x ys)
;;

let rec check_sig (signature:(symbol * int) list): bool =
  match signature with
      [] -> true
    | (s, arity)::xs ->
        let rec repeats (s:symbol) t: bool = match t with
            [] -> false
          | (s', _)::ys -> (s = s') || (repeats s ys)
        in (arity >= 0) && (not (repeats s xs)) && (check_sig xs)
;;

let rec wfterm (signature:(symbol * int) list) (t:term): bool =
  match t with
      V(v) -> true
    | Node(s, l) -> exists (s, List.length l) signature && List.for_all (wfterm signature) l
;;

let rec ht (t:term): int =
  match t with
      V(v) -> 0
    | Node(s, l) -> 1 + (List.fold_left max (-1) (List.map ht l))
;;

let rec size (t:term): int =
  match t with
      V(v) -> 1
    | Node(s, l) -> 1 + (List.fold_left (+) 0 (List.map size l))
;;

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

let rec subst (t:term) (s:substitution): term =
  let rec substOne (subst_vt:variable * term) (t_:term): term =
    match t_ with
        V(x) -> if x = fst subst_vt then snd subst_vt
                else t_
      | Node(s, l) -> Node(s, List.map (substOne subst_vt) l)
  in List.fold_right substOne s t
;;

let rec variableInTerm (v:variable) (t:term): bool =
  match t with
      V(x) -> x = v
    | Node(s, l) -> List.exists (variableInTerm v) l
;;

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
          let f s tt = (mgu (subst (fst tt) s) (subst (snd tt) s)) @ s in
          List.fold_left f [] (List.combine l1 l2)
;;
