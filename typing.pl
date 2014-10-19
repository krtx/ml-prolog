
:- module(typing, [infer/3]).

:- use_module(typing_aux).

:- op(700, xfx, in).

in(X, [Y | Rest]) :- X == Y, !; in(X, Rest).

assoc(Y, [(X, _) | Rest], V) :- X \== Y, !, assoc(Y, Rest, V).
assoc(X, [(X, V) | _], V).

add_assoc(X, V, A, [(X, V) | A]).

zip([], [], []).
zip([X | Xs], [Y | Ys], [(X, Y) | Zs]) :- zip(Xs, Ys, Zs).

fresh_vars(Bound, Map) :- copy_term(Bound, New), zip(Bound, New, Map).

instanciate(Bound, Map, X, Y) :-
    (var(X), !, (X in Bound, !, assoc(X, Map, Y);
                 X = Y)
    );
    (X = arrow(X0, X1), !, Y = arrow(Y0, Y1),
     instanciate(Bound, Map, X0, Y0), instanciate(Bound, Map, X1, Y1));
    X = Y.

%% instanciate the type Ty 
instanciate(Bound, Ty, Ty0) :-
    fresh_vars(Bound, Map), instanciate(Bound, Map, Ty, Ty0).

infer(TyEnv, var(X), Ty) :-
    assoc(X, TyEnv, (Bound, Ty0)),
    instanciate(Bound, Ty0, Ty).

infer(_, int(_), int).
infer(_, bool(_), bool).

infer(TyEnv, if(Cond, Then, Else), Ty) :-
    infer(TyEnv, Cond, bool),
    infer(TyEnv, Then, Ty),
    infer(TyEnv, Else, Ty).

infer(TyEnv, let(Id, E1, E2), T) :-
    infer(TyEnv, E1, Ty0),
    closure(TyEnv, Ty0, Frees),
    add_assoc(Id, (Frees, Ty0), TyEnv, NTyEnv),
    infer(NTyEnv, E2, T).

infer(TyEnv, fun(Id, Body), arrow(T1, T2)) :-
    add_assoc(Id, ([], T1), TyEnv, TyEnv0),
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

