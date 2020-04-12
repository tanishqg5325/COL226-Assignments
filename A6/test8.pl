max(X, X, X).
max(X, Y, Z) :- X > Y, Z = X.
max(X, Y, Z) :- Y > X, Z = Y.

fact(0, 1).
fact(X, Y) :-
        X > 0,
        Z = X - 1,
        fact(Z, W),
        Y =  W * X.

eval(X) :- X = 4 + 5 * 4 / 2.
