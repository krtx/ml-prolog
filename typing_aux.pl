
:- module(typing_aux, [instanciate/3, closure/3]).

:- use_module(library(apply)).
:- use_module(utility).

hd_arrow(arrow(X, _), X).
tl_arrow(arrow(_, X), X).

extract_tysc((_, TySc), TySc).

freevar_ty(Ty, Free) :- term_variables(Ty, Free).

freevar_tysc((Bound, Ty), Free) :-
    term_variables(Ty, Free0), diff(Free0, Bound, Free).

freevar_env(TyEnv, Free) :-
    maplist(extract_tysc, TyEnv, TyEnv0),
    maplist(freevar_tysc, TyEnv0, Frees),
    foldl(append, Frees, [], Free).

%% collect variables that
%% - appear in Ty
%% - appear in TyEnv but not bound in type scheme
closure(TyEnv, Ty, Frees) :-
    freevar_env(TyEnv, FTE),
    freevar_ty(Ty, FT),
    diff(FT, FTE, Frees).

instanciate(Bound, Map, X, Y) :-
    (var(X), !, (X in Bound, !, assoc(X, Map, Y);
                 X = Y)
    );
    (X = arrow(X0, X1), !, Y = arrow(Y0, Y1),
     instanciate(Bound, Map, X0, Y0), instanciate(Bound, Map, X1, Y1));
    X = Y.

%% replace variables in Ty that also appear in Bound with fresh variables
%% instanciate([X], arrow(X, Y), arrow(Z, Y))
instanciate(Bound, Ty, Ty0) :-
    copy_term(Bound, New),
    zip(Bound, New, Map),
    instanciate(Bound, Map, Ty, Ty0).

