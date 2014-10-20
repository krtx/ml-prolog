
:- module(parser, [parser/2]).

:- use_module(library(dcg/basics)).

reserved(`true`).
reserved(`false`).
reserved(`if`).
reserved(`then`).
reserved(`else`).
reserved(`let`).
reserved(`in`).
reserved(`fun`).

boolean(true) --> "true".
boolean(false) --> "false".

term(int(X)) --> integer(X).
term(bool(X)) --> boolean(X).

term(X) --> "(", blanks, expression(X), blanks, ")".

term(if(Cond, Then, Else)) -->
    "if", blanks,
    expression(Cond), blanks,
    "then", blanks,
    expression(Then), blanks,
    "else", blanks,
    expression(Else).

term(let(Id, E1, E2)) -->
    "let", blanks,
    variable(Id), blanks,
    "=", blanks,
    expression(E1), blanks,
    "in", blanks,
    expression(E2).

term(fun(Id, E)) -->
    "fun", blanks,
    variable(Id), blanks,
    "->", blanks,
    expression(E).

term(var(X)) --> variable(X).

variable(Z) -->
    [X], {code_type(X, alpha)}, !, variable_(Y),
    {Z = [X | Y], not(reserved(Z))}, !.

variable_([X | Y]) --> [X], {code_type(X, csym); [X] = `'`}, !, variable_(Y). %% '
variable_([]) --> [].


%% Expression

addf(X, Y, binop(add, X, Y)).
subf(X, Y, binop(sub, X, Y)).
mulf(X, Y, binop(mul, X, Y)).
divf(X, Y, binop(div, X, Y)).
ltf(X, Y, binop(lt, X, Y)).
landf(X, Y, binop(land, X, Y)).
lorf(X, Y, binop(lor, X, Y)).

ops([
           [("*", mulf), ("/", divf)],
           [("+", addf), ("-", subf)],
           [("<", ltf)],
           [("&&", landf), ("||", lorf)]
   ]).

fold_left(_, A, [], A).
fold_left(F, A, [X | Xs], C) :-
    B =.. [F, A, X],
    fold_left(F, B, Xs, C).

expression(X) -->
    {ops(S), fold_left(make_parser, application, S, P)},
    call(P, X).

%% expression(X) --> application(X), !.

application(Y) --> term(X), application_aux(X, Y).

application_aux(X, W) -->
    blank,
    blanks,
    term(Y), !,
    application_aux(app(X, Y), W).
application_aux(X, X) --> [].

make_parser(Term, Ops, Z) --> call(Term, X), aux(Term, Ops, X, Z).

aux(Term, Ops, X, A) -->
    blanks,
    choice(Ops, Cstr),
    blanks,
    call(Term, Z), !,
    blanks,
    aux(Term, Ops, W, A),
    {call(Cstr, X, Z, W)}.

aux(_, _, X, X) --> [].

choice([(Chr, Cstr) | _], Cstr) --> Chr, !.
choice([_ | Rest], Cstr) --> choice(Rest, Cstr).

parser(X, Y) :- phrase(expression(Y), X).

