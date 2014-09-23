
:- module(parser, [parser/2]).

:- use_module(library(dcg/basics)).

reserved(`true`).
reserved(`false`).
reserved(`if`).
reserved(`then`).
reserved(`else`).

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

term(var(X)) --> variable(X), {not(reserved(X))}.

variable([X | Y]) --> [X], {code_type(X, alpha)}, variable_(Y).

variable_([]) --> [].
variable_([X | Y]) --> [X], {code_type(X, csym); [X] = `'`}, variable_(Y). %% '

%% Expression
%% refered to mParser (https://bitbucket.org/cakeplus/mparser)

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
    {ops(S), fold_left(make_parser, term, S, P)},
    call(P, X).

make_parser(Term, _Ops, X) --> call(Term, X).
make_parser(Term, Ops, Z) --> call(Term, X), blanks, aux(Term, Ops, X, Z).

aux(Term, Ops, X, A) -->
    choice(Ops, Cstr), blanks,
    call(Term, Y),
    {call(Cstr, X, Y, A)}.

aux(Term, Ops, X, A) -->
    choice(Ops, Cstr), blanks,
    call(Term, Z), blanks,
    aux(Term, Ops, W, A),
    {call(Cstr, X, Z, W)}.

choice([(Chr, Cstr) | _], Cstr) --> Chr.
choice([_ | Rest], Cstr) --> choice(Rest, Cstr).

parser(X, Y) :- phrase(expression(Y), X).
