
:- module(eval, [eval/3]).

:- use_module(utility).

eval(_Env, int(X), int(X)).
eval(_Env, bool(X), bool(X)).
eval(Env, var(X), Z) :- assoc(X, Env, Z).
eval(Env, if(Cond, Then, Else), E) :-
    eval(Env, Cond, bool(C)),
    (
        (C = true, eval(Env, Then, E));
        (C = false, eval(Env, Else, E))
    ).

eval(Env, let(Id, E1, E2), V) :-
    eval(Env, E1, V1),
    add_assoc(Id, V1, Env, NEnv),
    eval(NEnv, E2, V).

eval(Env, fun(Id, E), proc(Env, Id, E)).

eval(Env, app(E1, E2), V) :-
    eval(Env, E1, proc(NEnv0, Id, Body)),
    eval(Env, E2, Arg),
    add_assoc(Id, Arg, NEnv0, NEnv),
    eval(NEnv, Body, V).

eval(Env, binop(Op, X, Y), Z) :-
    eval(Env, X, A),
    eval(Env, Y, B),
    apply_prim(Op, A, B, Z).

apply_prim(add, int(A), int(B), int(Z)) :- Z is A + B.
apply_prim(sub, int(A), int(B), int(Z)) :- Z is A - B.
apply_prim(mul, int(A), int(B), int(Z)) :- Z is A * B.
apply_prim(div, int(A), int(B), int(Z)) :- Z is A / B.
apply_prim(lt, int(A), int(B), bool(Z)) :- (A < B, !, Z = true); Z = false.
apply_prim(land, bool(A), bool(B), bool(Z)) :-
    (A = true, B = true, !, Z = true); Z = false.
apply_prim(lor, bool(A), bool(B), bool(Z)) :-
    (A = false, B = false, !, Z = false); Z = true.

