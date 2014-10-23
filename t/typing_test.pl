
:- begin_tests(typing).

:- use_module(parser).
:- use_module(typing).

:- style_check(-singleton).

infer_type(Program, Type) :- parser(Program, Ast), infer([], Ast, Type).

test(add, true(T = int)) :- infer_type(`1 + 2`, T).

test(mult, true(T = int)) :- infer_type(`-2 * 2`, T).

test(lt, true(T = bool)) :- infer_type(`1 < 2`, T).

test(fun, true(T =@= arrow(A, A))) :- infer_type(`fun x -> x`, T).

test(fun, true(T =@= arrow(A, arrow(B, A)))) :-
    infer_type(`fun x -> fun y -> x`, T).

test(fun, true(T =@= arrow(A, arrow(B, B)))) :-
    infer_type(`fun x -> fun y -> y`, T).

test(fun_app, true(T = int)) :-
    infer_type(`(fun x -> x + 1) 2 + (fun x -> x + -1) 3`, T).

test(fun, true(T =@= arrow(arrow(A, B), arrow(arrow(B, C), arrow(A, C))))) :-
    infer_type(`fun f -> fun g -> fun x -> g (f x)`, T).

test(fun,
     true(T =@= arrow(arrow(A, arrow(B, C)),
                      arrow(arrow(A, B),
                            arrow(A, C))))) :-
    infer_type(`fun x -> fun y -> fun z -> x z (y z)`, T).

test(fun_let, true(T =@= arrow(int, int))) :-
    infer_type(`fun x -> let y = x + 1 in x`, T).

test(fun_let, true(T =@= arrow(int, int))) :-
    infer_type(`fun x -> let y = x + 1 in y`, T).

test(fun_if,
     true(T =@= arrow(bool,
                      arrow(arrow(bool, bool),
                            arrow(bool, bool))))) :-
    infer_type(`fun b -> fun x -> if x b then x else (fun x -> b)`, T).

test(fun_if, true(T =@= arrow(bool, bool))) :-
    infer_type(`fun x -> if true then x else (if x then true else false)`, T).

test(fun_if, true(T =@= arrow(bool, arrow(bool, bool)))) :-
    infer_type(`fun x -> fun y -> if x then x else y`, T).

test(fun, true(T =@= arrow(arrow(A, B),
                           arrow(A, B)))) :-
    infer_type(`fun x -> fun y -> x y`, T).

test(fun, true(T =@= arrow(arrow(A, B),
                           arrow(arrow(arrow(A, B), A),
                                 B)))) :-
    infer_type(`fun x -> fun y -> x (y x)`, T).

test(fun, true(T =@= arrow(arrow(A, arrow(A, B)),
                           arrow(arrow(arrow(A, arrow(A, B)), A),
                                 B)))) :-
    infer_type(`fun x -> fun y -> x (y x) (y x)`, T).

test(fun, true(T =@= arrow(arrow(arrow(arrow(A, B), A), arrow(B, C)),
                           arrow(arrow(A, B),
                                 arrow(arrow(arrow(arrow(arrow(A, B), A),
                                                   arrow(B, C)),
                                             arrow(arrow(A, B), A)),
                                       C))))) :-
    infer_type(`fun x -> fun y -> fun z -> x (z x) (y (z x y))`, T).

test(let_fun, true(T =@= arrow(arrow(arrow(A, A), B), B))) :-
    infer_type(`let id = fun x -> x in let f = fun y -> id (y id) in f`, T).

test(let_fun, true(T =@= arrow(arrow(arrow(A, arrow(B, A)), C),
                               arrow(D, arrow(E, C))))) :-
    infer_type(`let k = fun x -> fun y -> x in let k1 = fun x -> fun y -> k (x k) in k1`, T).

test(let_fun,
     true(T =@=
            arrow(arrow(arrow(arrow(A, arrow(B, C)),
                              arrow(arrow(A, B), arrow(A, C))),
                        arrow(D, arrow(E, F))),
                  arrow(arrow(arrow(arrow(G, (arrow(H, I))),
                                    arrow(arrow(G, H), arrow(G, I))),
                              arrow(D, E)),
                        arrow(arrow(arrow(arrow(J, arrow(K, L)),
                                          arrow(arrow(J, K), arrow(J, L))),
                                    D),
                              F)))
    )) :-
    infer_type(`let s = fun x -> fun y -> fun z -> x z (y z) in let s1 = fun x -> fun y -> fun z -> x s (z s) (y s (z s)) in s1`, T).

test(let_fun,
     true(T =@= arrow(A, arrow(arrow(arrow(A, arrow(B, C)), arrow(D, B)),
                               arrow(arrow(A, arrow(B, C)),
                                     arrow(D, C)))))) :-
    infer_type(`let g = fun h -> fun t -> fun f -> fun x -> f h (t f x) in g`, T).

test(skk, true(T =@= arrow(A, A))) :-
    infer_type(`let s = fun x -> fun y -> fun z -> x z (y z) in let k = fun x -> fun y -> x in let k' = fun x -> fun y -> x in s k k'`, T).

test(skk, true(T =@= arrow(A, arrow(B, B)))) :-
    infer_type(`let s = fun x -> fun y -> fun z -> x z (y z) in let k' = fun x -> fun y -> y in s k' k'`, T). % '.

test(fun_let, true(T =@= arrow(arrow(A, arrow(arrow(A, A), bool)),
                               arrow(A, arrow(arrow(A, A), A))))) :-
    infer_type(`fun x -> fun y -> fun z -> let b = x y z in if b then z y else y`, T).

test(pair, true(T =@= int)) :-
    infer_type(`let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (pair 1 100)`, T).

test(pair, true(T =@= int)) :-
    infer_type(`let pair = fun x1 -> fun x2 -> fun y -> y x1 x2 in let proj1 = fun p -> p (fun x1 -> fun x2 -> x1) in let proj2 = fun p -> p (fun x1 -> fun x2 -> x2) in proj1 (proj2 (pair 10 (pair 20 30)))`, T).

test(let_fun, true(T =@= int)) :-
    infer_type(`let f = fun x -> x in if f true then f 1 else f 2`, T).

test(let_fun, true(T =@= int)) :-
    infer_type(`let f = fun x -> 3 in f true + f 4`, T).

test(fun_let, true(T =@= arrow(bool, arrow(A, A)))) :-
    infer_type(`fun b -> let f = fun x -> x in let g = fun y -> y in if b then f g else g f`, T).

test(fun_let, true(T =@= arrow(bool, arrow(A, arrow(arrow(A, arrow(arrow(arrow(A, B), B), C)), C))))) :-
    infer_type(`fun b -> fun f -> let g1 = fun x -> x f in let g2 = fun x -> x f in fun z -> if b then g1 z g2 else g2 z g1`, T).

test(add, fail) :-
    infer_type(`1 + true`, _).

test(add, fail) :-
    infer_type(`2 + (fun x -> x)`, _).

test(mult, fail) :-
    infer_type(`-2 * false`, _).

test(fun, fail) :-
    infer_type(`fun x -> x x`, _).

test(let_fun, fail) :-
    infer_type(`let f = fun x -> fun g -> g (x x g) in f f`, _).

test(fun, fail) :-
    infer_type(`let g = fun f -> fun x -> f x (f x). in g`, _).

test(fun, fail) :-
    infer_type(`let g = fun f -> fun x -> f x (x f). in g`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> x y + y x`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> x y + x`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> if x y then x else y`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> if x y then (fun z -> if y z then z else x). else (fun x -> x).`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> fun z -> let b = x y z in if b then z y else z x`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> fun z -> if x y then z x else y z`, _).

test(fun, fail) :-
    infer_type(`fun x -> if x then 1 else x`, _).

test(fun, fail) :-
    infer_type(`(fun x -> x + 1). true`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> y (x (y x).).`, _).

test(fun, fail) :-
    infer_type(`(fun f -> fun x -> f (f x).). (fun x -> fun y -> x).`, _).

test(fun, fail) :-
    infer_type(`fun x -> fun y -> y (x (fun z1 -> fun z2 -> z1).). (x (fun z -> z).).`, _).

test(fun, fail) :-
    infer_type(`fun b -> fun f -> let g1 = fun x -> f x in let g2 = fun x -> f x in if b then g1 g2 else g2 g1`, _).

:- end_tests(typing).
