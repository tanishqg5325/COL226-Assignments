let signat = [("0", 0); ("1", 0); ("double", 1); ("+", 2)];;
check_sig signat;;
let t1 = Node("+", [Node("0", [Node("0", [])]); Node("1",[])]);;
let t2 = Node("+",[Node("0",[]);Node("1",[])]);;
wfterm signat t1;;
wfterm signat t2;;
ht t2;;
size t2;;
vars t2;;
let t3 = Node("+",[V("x");Node("1",[])]);;
wfterm signat t3;;
ht t3;;
size t3;;
vars t3;; 
let t4 = Node("+",[V("x");Node("double",[V("y")])]);;
wfterm signat t4;;
ht t4;;
size t4;;
vars t4;;

let sub1 = [("x", V("c")); ("y", Node("+", [V("c");Node("1",[])]))];;
let sub2 = [("x", V("p")); ("c", t2)];;

subst sub1 t3;;
subst sub1 t4;;

mgu t3 (V("y"));;
mgu (V("y")) (V("x"));;
mgu (V("x")) (Node("0",[]));;

let t5 = Node("+",[V("p"); Node("double", [Node("double", [V("z")])])]);;
mgu t4 t5;;

mgu (Node("+",[V("x");V("y")])) (Node("+",[V("z");V("x")]));;
mgu (Node("+",[V("x");Node("0",[])])) (Node("+",[Node("0",[]);V("x")]));;
mgu (Node("+",[Node("double",[V("x")]);Node("double",[Node("0",[])])])) (Node("+",[V("y");V("y")]));;
let a1 = Node("+",[Node("a",[]);V("x");Node("double",[Node("double",[V("z")])])]);;
let a2 = Node("+",[V("z");Node("double",[V("y")]);Node("double",[V("y")])]);;
let ss = mgu a1 a2;;
subst ss a1 = subst ss a2;;

mgu (Node("+",[V("x");Node("0",[])])) (Node("+",[Node("1",[]);V("x")]));;
mgu (V("x")) t3;;
mgu (Node("+",[V("z");V("y")])) (Node("*",[V("y");V("x")]));;