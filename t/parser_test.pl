
:- begin_tests(parser).
:- use_module(parser).

test(parser) :- parser(`123`, int(123)).

test(parser) :-
    parser(`x * x`, binop(mul, var([120]), var([120]))).

test(parser) :-
    parser(`10 + 20 * 30 + 40`, binop(add, binop(add, int(10), binop(mul, int(20), int(30))), int(40))).

test(parser) :-
    parser(`x x * x`, binop(mul, app(var([120]), var([120])), var([120]))).

test(parser) :-
    parser(`x x * x * x * x`, binop(mul, binop(mul, binop(mul, app(var([120]), var([120])), var([120])), var([120])), var([120]))).

test(parser) :-
    parser(`fun x -> x * x`, fun([120], binop(mul, var([120]), var([120])))).

test(parser) :-
    parser(`x + fun x -> x * x`, binop(add, var([120]), fun([120], binop(mul, var([120]), var([120]))))).

test(parser) :-
    parser(`x + fun x -> x x * x`, binop(add, var([120]), fun([120], binop(mul, app(var([120]), var([120])), var([120]))))).

test(parser, fail) :-
    parser(`let f = fun x -> fun y -> x y`, _).

:- end_tests(parser).
