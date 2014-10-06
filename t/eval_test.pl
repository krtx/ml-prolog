
:- begin_tests(eval).
:- use_module(eval).
:- use_module(parser).

test(eval) :-
    parser(`123`, A), eval([], A, int(123)), !.

test(eval) :-
    parser(`(fun x -> x) 10`, A), eval([], A, int(10)), !.

test(eval) :-
    parser(`let x = 2 in (fun x -> x * x) 4`, A), eval([], A, int(16)), !.

:- end_tests(eval).
