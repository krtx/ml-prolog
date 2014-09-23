
:- cd('/Users/kinoshita/work/prolog/ml/').

:- use_module(parser).
:- use_module(typing).

assoc(Y, [(X , _) | Rest], V) :- X \= Y, !, assoc(Y, Rest, V).
assoc(X, [(X , V) | _], V).

add_assoc(X, A, V, [[X | V] | A]).

eval(_Env, int(X), int(X)).
eval(_Env, bool(X), bool(X)).
eval(Env, var(X), Z) :- assoc(X, Env, Z).
eval(Env, if(Cond, Then, Else), E) :-
    eval(Env, Cond, C),
    (
        (C = true, eval(Env, Then, E));
        (C = false, eval(Env, Else, E))
    ).

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

initial_env([(`i`, int(1)),
             (`ii`, int(2)),
             (`iii`, int(3)),
             (`iv`, int(4)),
             (`v`, int(5)),
             (`x`, int(10))]).

main(Input, Output) :-
    initial_env(E),
    parser(Input, Ast),
    eval(E, Ast, Output).
