isPresent(X, [X | _]) :- !.
isPresent(X, [_ | Z]) :- isPresent(X, Z).

len([], 0).
len([_ | Y], Z) :- len(Y, W), Z is W + 1.

find([X | _], 1, X).
find([_ | Y], I, Z) :- J is I-1, find(Y, J, Z).

append([], L2, L2).
append([X | Xs], L2, [X | L]) :- append(Xs, L2, L).

intersection([], L2).
intersection([(X, _) | Xs], L2) :- \+ isPresent((X, _), L2), intersection(Xs, L2).


type(typeVar(_)).

type(tint).
type(tbool).

type(arrow(T1, T2)) :- type(T1), type(T2).

type([T1, T2]) :- type(T1), type(T2).
type([H | T]) :- type(H), type(T).


hastype(Gamma, var(X), T) :- isPresent((var(X), T), Gamma).

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

hastype(Gamma, func(E1, E2), T2) :- hastype(Gamma, E1, arrow(T1, T2)), hastype(Gamma, E2, T1).

hastype(Gamma, [], []).
hastype(Gamma, [E1 | En], [T1 | Tn]) :- hastype(Gamma, E1, T1), hastype(Gamma, En, Tn).

hastype(Gamma, proj(I, E), T) :- 1 =< I, len(E, L), I =< L, hastype(Gamma, E, TT), find(TT, I, T).


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

