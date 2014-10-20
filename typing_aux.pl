
:- module(typing_aux, [closure/3]).

:- op(700, xfx, in).

in(X, [Y | Rest]) :- X == Y, !; in(X, Rest).

remove_duplicates([], []).
remove_duplicates([X | Rest], Z) :-
    (X in Rest, !, remove_duplicates(Rest, Z));
    (remove_duplicates(Rest, Z0), Z = [X | Z0]).

diff([], _, []).
diff([X | Rest], L, Z) :-
    (X in L, !, diff(Rest, L, Z));
    (diff(Rest, L, Z0), Z = [X | Z0]).

%% Bound に含まれる型変数は除外する
freevar_tysc((Bound, Ty), Free) :-
    freevar_tysc_((Bound, Ty), Z), remove_duplicates(Z, Free).

freevar_tysc_((Bound, Ty), Z) :-
    (var(Ty), !, ((Ty in Bound, !, Z = []); Z = [Ty]));
    (atom(Ty), !, Z = []);
    (hd_arrow(Ty, Hd), tl_arrow(Ty, Tl),
     freevar_tysc_((Bound, Hd), Hz), freevar_tysc_((Bound, Tl), Tz),
     append(Hz, Tz, Z)).

hd_arrow(arrow(X, _), X).
tl_arrow(arrow(_, X), X).

freevar_ty(Ty, Free) :- freevar_tysc(([], Ty), Free).

freety_env(TyEnv, Free) :- freety_env_(TyEnv, Z), remove_duplicates(Z, Free).

freety_env_([(_, TySc) | Rest], Z) :-
    freevar_tysc(TySc, F0), freety_env_(Rest, F1), append(F0, F1, Z).
freety_env_([], []).

closure(TyEnv, Ty, Frees) :-
    freety_env(TyEnv, FTE),
    freevar_ty(Ty, FT),
    diff(FT, FTE, Frees).

:- begin_tests(typing_aux).

test(in) :- 1 in [1, 2, 3].
test(in) :- 1 in [2, 1, 3].
test(in) :- not(1 in [2, 3, 4]).
test(in) :- X in [X, Y, Z].
test(in) :- X in [Y, X, Z].
test(in) :- not(X in [Y, Z, W]).

test(remove_duplicates) :- remove_duplicates([1,1,2,2,3,3], [1,2,3]).
test(remove_duplicates) :- remove_duplicates([1,2,3,1,2,3], [1,2,3]).
test(remove_duplicates) :- remove_duplicates([X,X,Y,Y,Z,Z], Z), Z == [X,Y,Z].
test(remove_duplicates) :- remove_duplicates([X,Y,Z,X,Y,Z], Z), Z == [X,Y,Z].

test(diff) :- diff([1,2,3], [2,3,4], [1]).
test(diff) :- diff([1,2,3], [4,5,6], [1,2,3]).
test(diff) :- diff([X,Y,Z], [Y,Z,W], Z), Z == [X].
test(diff) :- diff([X,Y,Z], [W,P,Q], Z), Z == [X,Y,Z].

test(freevar_tysc) :- freevar_tysc(([X], arrow(X, Y)), Z), Z == [Y].
test(freevar_tysc) :- freevar_tysc(([X,Y], arrow(X, Y)), Z), Z == [].
test(freevar_tysc) :- freevar_tysc(([X], arrow(X, arrow(int, Y))), Z), Z == [Y].

test(freety_env) :-
    freety_env([(_, ([X], arrow(X, X))), (_, ([], arrow(X, X)))], Z),
    Z == [X].

test(freety_env) :-
    freety_env([(_, ([X], arrow(X, X))), (_, ([], arrow(X, Y)))], Z),
    Z == [X, Y].

test(closure) :-
    closure([(_, ([X], arrow(X, X))), (_, ([], arrow(B, Y)))],
            arrow(arrow(A, B), C),
            Z),
    Z == [A, C].

:- end_tests(typing_aux).
