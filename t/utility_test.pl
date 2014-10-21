
:- begin_tests(utility).

:- use_module(utility).

:- style_check(-singleton).

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

test(assoc) :- assoc(X, [(Y, 10), (X, 20), (Z, 30)], 20).
test(assoc) :- not(assoc(X, [(Y, 10), (Q, 20), (Z, 30)], W)).

test(zip) :- zip([a,b,c,d], [p,q,r,s], [(a,p), (b,q), (c,r), (d,s)]).
test(zip) :- zip([1, 2, 3], [X, Y, Z], [(1, P), (2, Q), (3, R)]),
             X == P, Y == Q, Z == R.

:- end_tests(utility).
