
%% :- cd('/Users/kinoshita/work/prolog/ml/').

:- use_module(parser).
:- use_module(typing).
:- use_module(eval).

initial_env([(`i`, int(1)),
             (`ii`, int(2)),
             (`iii`, int(3)),
             (`iv`, int(4)),
             (`v`, int(5)),
             (`x`, int(10))]).

initial_tyenv([(`i`, int),
               (`ii`, int),
               (`iii`, int),
               (`iv`, int),
               (`v`, int),
               (`x`, int)]).

main(Input, Output, T) :-
    initial_env(E),
    initial_tyenv(TE),
    parser(Input, Ast),
    infer(TE, Ast, T),
    eval(E, Ast, Output).

