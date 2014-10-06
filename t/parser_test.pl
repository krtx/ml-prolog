
:- begin_tests(parser).
:- use_module(parser).

test(parser, all(X == [int(123)])) :-
    parser(`123`, X).

test(parser, all(X == [binop(mul, var([120]), var([120]))])) :-
    parser(`x * x`, X).

test(parser, all(X == [binop(add, binop(add, int(10), binop(mul, int(20), int(30))), int(40))])) :-
    parser(`10 + 20 * 30 + 40`, X).

test(parser, all(X == [binop(mul, app(var([120]), var([120])), var([120]))])) :-
    parser(`x x * x`, X).

test(parser, all(X == [binop(mul, binop(mul, binop(mul, app(var([120]), var([120])), var([120])), var([120])), var([120]))])) :-
    parser(`x x * x * x * x`, X).

test(parser, all(X == [fun([120], binop(mul, var([120]), var([120])))])) :-
    parser(`fun x -> x * x`, X).

test(parser, all(X == [binop(add, var([120]), fun([120], binop(mul, var([120]), var([120]))))])) :-
    parser(`x + fun x -> x * x`, X).

test(parser, all(X == [binop(add, var([120]), fun([120], binop(mul, app(var([120]), var([120])), var([120]))))])) :-
    parser(`x + fun x -> x x * x`, X).

test(parser, fail) :-
    parser(`let f = fun x -> fun y -> x y`, X).

:- end_tests(parser).
