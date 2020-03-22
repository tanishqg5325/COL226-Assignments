let sgn = [("plus", 2); ("mult", 2); ("zero", 0); ("one", 0); ("two", 0); ("unary", 1)];;
check_sig sgn;;

let t = Node("plus", [Node("mult", [V("a"); Node("one", [])]); V("b")]);;
wfterm sgn t;;
ht t;;
size t;;
vars t;;
let s = [("b", Node("plus", [Node("one", []); V("a")])); ("a", Node("unary", [V("c")])); ("c", Node("two", []))];;
subst s t;;
let t1 = Node("plus", [Node("unary", [V("b")]); V("a")]);;
let t2 = Node("plus", [V("a"); Node("unary", [V("c")])]);;
mgu t1 t2;;
