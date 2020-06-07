isPresent(X, [X | Z]).
isPresent(X, [Y | Z]) :- isPresent(X, Z).

len([], 0).
len([X | Y], Z) :- len(Y, W), Z is W + 1.

find([X | Y], 1, X).
find([X | Y], I, Z) :- J is I-1, find(Y, J, Z).


type(typeVar(X)).

type(tint).
type(tbool).

type(arrow(T1, T2)) :- type(T1), type(T2).

type([T1, T2]) :- type(T1), type(T2).
type([H | T]) :- type(H), type(T).


hastype(Gamma, variable(X), T) :- isPresent((variable(X), T), Gamma).

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

hastype(Gamma, func(E1, E2), T2) :- hastype(Gamma, E1, arrow(T1, T2)), hastype(Gamma, E2, T1).

hastype(Gamma, [], []).
hastype(Gamma, [E1 | En], [T1 | Tn]) :- hastype(Gamma, E1, T1), hastype(Gamma, En, Tn).

hastype(Gamma, proj(I, E), T) :- 1 =< I, len(E, L), I =< L, hastype(Gamma, E, TT), find(TT, I, T).
