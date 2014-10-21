
:- begin_tests(typing_aux).

:- load_files(typing_aux, [imports([freevar_tysc/2,
                                    freevar_env/2,
                                    closure/3,
                                    instanciate/3
             ])]).

test(freevar_tysc) :- freevar_tysc(([X], arrow(X, Y)), Z), Z == [Y].
test(freevar_tysc) :- freevar_tysc(([X,Y], arrow(X, Y)), Z), Z == [].
test(freevar_tysc) :- freevar_tysc(([X], arrow(X, arrow(int, Y))), Z), Z == [Y].

test(freevar_env) :-
    freevar_env([(_, ([X], arrow(X, X))), (_, ([], arrow(X, X)))], Z),
    Z == [X].

test(freevar_env) :-
    freevar_env([(_, ([X], arrow(X, X))), (_, ([], arrow(X, Y)))], Z),
    Z == [X, Y].

test(closure) :-
    closure([(_, ([X], arrow(X, X))), (_, ([], arrow(B, Y)))],
            arrow(arrow(A, B), C),
            Z),
    Z == [A, C].

test(instanciate) :-
    instanciate([X], arrow(X, Y), arrow(P, Q)),
    X \== P,
    Y == Q.

:- end_tests(typing_aux).
