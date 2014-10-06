
:- module(typing, [infer/3]).

assoc(Y, [(X, _) | Rest], V) :- X \= Y, !, assoc(Y, Rest, V).
assoc(X, [(X, V) | _], V).

add_assoc(X, V, A, [(X, V) | A]).

infer(_, int(_), int).
infer(_, bool(_), bool).
infer(TyEnv, var(X), T) :- assoc(X, TyEnv, T).

infer(TyEnv, if(Cond, Then, Else), T) :-
    infer(TyEnv, Cond, bool),
    infer(TyEnv, Then, T),
    infer(TyEnv, Else, T).

infer(TyEnv, let(Id, E1, E2), T) :-
    infer(TyEnv, E1, T0),
    add_assoc(Id, T0, TyEnv, NTyEnv),
    infer(NTyEnv, E2, T).

infer(TyEnv, fun(Id, Body), arrow(T1, T2)) :-
    add_assoc(Id, T1, TyEnv, TyEnv0),
    infer(TyEnv0, Body, T2).

infer(TyEnv, app(E1, E2), T2) :-
    infer(TyEnv, E1, arrow(T1, T2)),
    infer(TyEnv, E2, T1).

infer(TyEnv, binop(Op, X, Y), T) :-
    infer(TyEnv, X, TX),
    infer(TyEnv, Y, TY),
    infer_prim(Op, TX, TY, T).

infer_prim(add, int, int, int).
infer_prim(sub, int, int, int).
infer_prim(mul, int, int, int).
infer_prim(div, int, int, int).
infer_prim(lt, int, int, bool).
infer_prim(land, bool, bool, bool).
infer_prim(lor, bool, bool, bool).

