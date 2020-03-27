type variable = string
type constant = string
type symbol = string
type term = V of variable | C of constant | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = P of clause list
type goal = G of atom list

