
:- module(typing, [infer/3]).

assoc(Y, [(X , _) | Rest], V) :- X \= Y, !, assoc(Y, Rest, V).
assoc(X, [(X , V) | _], V).

add_assoc(X, A, V, [[X | V] | A]).

infer(_, int(_), int).
infer(_, bool(_), bool).
infer(TyEnv, var(X), Z) :- assoc(X, TyEnv, Z).

infer(TyEnv, if(Cond, Then, Else), Z) :-
    infer(TyEnv, Cond, bool),
    infer(TyEnv, Then, Z),
    infer(TyEnv, Else, Z).

infer(TyEnv, binop(Op, X, Y), Z) :-
    infer(TyEnv, X, TX),
    infer(TyEnv, Y, TY),
    infer_prim(Op, TX, TY, Z).

infer_prim(add, int, int, int).
infer_prim(sub, int, int, int).
infer_prim(mul, int, int, int).
infer_prim(div, int, int, int).
infer_prim(lt, int, int, bool).
infer_prim(land, bool, bool, bool).
infer_prim(lor, bool, bool, bool).

