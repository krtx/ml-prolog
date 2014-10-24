
:- module(utility, [op(700, xfx, in), (in)/2,
                    remove_duplicates/2,
                    diff/3,
                    assoc/3,
                    add_assoc/4,
                    zip/3]).

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

assoc(Y, [(X, _) | Rest], V) :- X \== Y, !, assoc(Y, Rest, V).
assoc(X, [(X, V) | _], V).

add_assoc(X, V, A, [(X, V) | A]).

zip([], [], []).
zip([X | Xs], [Y | Ys], [(X, Y) | Zs]) :- zip(Xs, Ys, Zs).
