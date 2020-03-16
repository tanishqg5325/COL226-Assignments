type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * (term list);;

exception NOT_UNIFIABLE

let rec check_sig (signature:(symbol * int) list): bool = match signature with
    [] -> true
  | (s, arity)::xs -> 
      let rec occurs (s:symbol) t: bool = match t with
          [] -> false
        | (s', arity)::ys -> (s = s') || (occurs s ys)
      in (arity >= 0) && (not (occurs s xs)) && (check_sig xs)
;;
