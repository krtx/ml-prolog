
:- module(typing, [infer/3]).

:- use_module(utility).
:- use_module(typing_aux).

:- set_prolog_flag(occurs_check, true). % prohibit recursive types

infer(TyEnv, var(X), Ty) :-
    assoc(X, TyEnv, (Bound, Ty0)),
    instanciate(Bound, Ty0, Ty1), Ty = Ty1, !.

infer(_, int(_), int) :- !.
infer(_, bool(_), bool) :- !.

infer(TyEnv, if(Cond, Then, Else), Ty) :-
    infer(TyEnv, Cond, bool),
    infer(TyEnv, Then, Ty),
    infer(TyEnv, Else, Ty), !.

infer(TyEnv, let(Id, E1, E2), T) :-
    infer(TyEnv, E1, Ty0),
    closure(TyEnv, Ty0, Frees),
    add_assoc(Id, (Frees, Ty0), TyEnv, NTyEnv),
    infer(NTyEnv, E2, T), !.

infer(TyEnv, fun(Id, Body), arrow(T1, T2)) :-
    add_assoc(Id, ([], T1), TyEnv, TyEnv0),
    infer(TyEnv0, Body, T2), !.

infer(TyEnv, app(E1, E2), T2) :-
    infer(TyEnv, E1, arrow(T1, T2)),
    infer(TyEnv, E2, T1), !.

infer(TyEnv, binop(Op, X, Y), T) :-
    infer(TyEnv, X, TX),
    infer(TyEnv, Y, TY),
    infer_prim(Op, TX, TY, T), !.

infer_prim(add, int, int, int).
infer_prim(sub, int, int, int).
infer_prim(mul, int, int, int).
infer_prim(div, int, int, int).
infer_prim(lt, int, int, bool).
infer_prim(land, bool, bool, bool).
infer_prim(lor, bool, bool, bool).

