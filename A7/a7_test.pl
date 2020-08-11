hastype([(var(x), tint), (var(y), tbool)], var(y), T).

hastype([], 0, T).
hastype([], 56, T).
hastype([], -123, T).
hastype([], false, T).

hastype([(var(x), tint), (var(y), tint)], abs(add(mul(var(x), 4), div(9, var(y)))), T).
hastype([(var(x), tint), (var(y), tint)], sub(mul(6, 7), mod(var(x), var(y))), T).

hastype([(var(x), tbool), (var(y), tbool)], not(and(or(var(x), false), and(true, var(y)))), T).

hastype([], less(4, 3), T).
hastype([], and(less_eq(67, 23), greater_eq(2, mul(4, 5))), T).
hastype([(var(a), tint), (var(b), tint)], or(greater(4, var(b)), not_eq(89, var(a))), T).

hastype([(var(a), tint), (var(b), tbool)], eq(tuple([var(b), mod(mul(4, 3), mod(5, 2))]), tuple([and(true, less(56, 12)), var(a)])), T).
hastype([(var(a), tint), (var(b), tbool)], eq(tuple([var(a), mod(mul(4, 3), mod(5, 2))]), tuple([and(true, less(56, 12)), var(b)])), T). % false

hastype([(var(c), tint)], if_then_else(and(less_eq(var(c), 5), greater_eq(var(c), -5)), add(var(c), 2), sub(var(c), 2)), T).

typeElaborates([], def(var(y), tuple([2, false])), Gamma_).
typeElaborates([(var(x), tint)], sequential(def(var(z), 6), def(var(y), sub(var(z), var(x)))), Gamma_).
typeElaborates([], parallel(def(var(x), false), def(var(y), tuple([true, 0]))), Gamma_).
typeElaborates([], parallel(sequential(def(var(x), -7), def(var(y), 78)), def(var(y), true)), Gamma_). % false
typeElaborates([], sequential(parallel(def(var(x), 3), def(var(y), false)), def(var(y), -4)), Gamma_).
typeElaborates([], local(def(var(x), 9), def(var(y), eq(-8, var(x)))), Gamma_).

hastype([], let_in_end(def(var(x), 3), mul(var(x), 2)), T).
hastype([(var(x), tint)], let_in_end(def(var(y), 0), mod(var(y), var(x))), T).

hastype([], tuple([32, false, tuple([true, 9, 5])]), T).

hastype([(var(x), tint)], proj(5, tuple([6, false, var(x), let_in_end(def(var(y), 1), div(var(y), 8))])), T).
hastype([], proj(5, tuple([false, tuple([9]), true, mod(9, 4), eq(tuple([2, true]), tuple([5, false]))])), T).
