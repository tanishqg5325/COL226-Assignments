len([], 0).
len([_ | Y], Z) :- len(Y, W), Z is W + 1.

findnth([X | _], 1, X).
findnth([_ | Y], I, Z) :- J is I-1, findnth(Y, J, Z).

append([], L2, L2).
append([X | Xs], L2, [X | L]) :- append(Xs, L2, L).

isPresent(X, [X | _]) :- !.
isPresent(X, [_ | Z]) :- isPresent(X, Z).

intersection([], L2).
intersection([(X, _) | Xs], L2) :- \+ isPresent((X, _), L2), intersection(Xs, L2).

find([], _, _) :- fail.
find([(X, Y) | _], X, Y) :- !.
find([_ | Xs], X, Y) :- find(Xs, X, Y).


type(typeVar(_)).

type(tbool).
type(tint).

type(arrow(T1, T2)) :- type(T1), type(T2).

type(cartesian([])).
type(cartesian([T1 | Tn])) :- type(T1), type(cartesian(Tn)).


hastype(Gamma, var(X), T) :- find(Gamma, var(X), T).

hastype(Gamma, N, tint) :- integer(N).
hastype(Gamma, true, tbool).
hastype(Gamma, false, tbool).

hastype(Gamma, abs(E), tint) :- hastype(Gamma, E, tint).
hastype(Gamma, add(E1, E2), tint) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, sub(E1, E2), tint) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, mul(E1, E2), tint) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, div(E1, E2), tint) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, mod(E1, E2), tint) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).

hastype(Gamma, not(E), tbool) :- hastype(Gamma, E, tbool).
hastype(Gamma, and(E1, E2), tbool) :- hastype(Gamma, E1, tbool), hastype(Gamma, E2, tbool).
hastype(Gamma, or(E1, E2), tbool) :- hastype(Gamma, E1, tbool), hastype(Gamma, E2, tbool).

hastype(Gamma, less(E1, E2), tbool) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, greater(E1, E2), tbool) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, less_eq(E1, E2), tbool) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, greater_eq(E1, E2), tbool) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).
hastype(Gamma, not_eq(E1, E2), tbool) :- hastype(Gamma, E1, tint), hastype(Gamma, E2, tint).

hastype(Gamma, eq(E1, E2), tbool) :- hastype(Gamma, E1, T), hastype(Gamma, E2, T).

hastype(Gamma, if_then_else(E1, E2, E3), T) :- hastype(Gamma, E1, tbool), hastype(Gamma, E2, T), hastype(Gamma, E3, T).

hastype(Gamma, let_in_end(D, E), T) :- typeElaborates(Gamma, D, Gamma_), append(Gamma_, Gamma, Gamma__), hastype(Gamma__, E, T).

hastype(Gamma, lambda(var(X), E), arrow(T1, T2)) :- hastype([(var(X), T1) | Gamma], E, T2).

hastype(Gamma, func(E1, E2), T2) :- hastype(Gamma, E1, arrow(T1, T2)), hastype(Gamma, E2, T1).

hastype(Gamma, tuple([]), cartesian([])).
hastype(Gamma, tuple([E1 | En]), cartesian([T1 | Tn])) :- hastype(Gamma, E1, T1), hastype(Gamma, tuple(En), cartesian(Tn)).

hastype(Gamma, proj(I, tuple(E)), T) :- 1 =< I, len(E, L), I =< L, hastype(Gamma, tuple(E), cartesian(TT)), findnth(TT, I, T).


typeElaborates(Gamma, def(var(X), E), [(var(X), T)]) :- hastype(Gamma, E, T).

typeElaborates(Gamma, sequential(D1, D2), Gamma_) :- 
	typeElaborates(Gamma, D1, Gamma1), 
	append(Gamma1, Gamma, Gamma_tmp), 
	typeElaborates(Gamma_tmp, D2, Gamma2), 
	append(Gamma2, Gamma1, Gamma_).

typeElaborates(Gamma, parallel(D1, D2), Gamma_) :- 
	typeElaborates(Gamma, D1, Gamma1), 
	typeElaborates(Gamma, D2, Gamma2), 
	intersection(Gamma1, Gamma2), 
	append(Gamma2, Gamma1, Gamma_).

typeElaborates(Gamma, local(D1, D2), Gamma_) :- 
	typeElaborates(Gamma, D1, Gamma1), 
	append(Gamma1, Gamma, Gamma_tmp), 
	typeElaborates(Gamma_tmp, D2, Gamma_). 

