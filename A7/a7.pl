isPresent(X, [X | Z]).
isPresent(X, [Y | Z]) :- isPresent(X, Z).

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

hastype(Gamma, add(X, Y), tint) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).
hastype(Gamma, sub(X, Y), tint) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).
hastype(Gamma, mul(X, Y), tint) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).
hastype(Gamma, div(X, Y), tint) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).

hastype(Gamma, and(X, Y), tbool) :- hastype(Gamma, X, tbool), hastype(Gamma, Y, tbool).
hastype(Gamma, or(X, Y), tbool) :- hastype(Gamma, X, bool), hastype(Gamma, Y, tbool).
hastype(Gamma, not(X), tbool) :- hastype(Gamma, X, tbool).

hastype(Gamma, less(X, Y), tbool) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).
hastype(Gamma, greater(X, Y), tbool) :- hastype(Gamma, X, tint), hastype(Gamma, Y, tint).

hastype(Gamma, eq(X, Y), tbool) :- hastype(Gamma, X, T), hastype(Gamma, Y, T).
