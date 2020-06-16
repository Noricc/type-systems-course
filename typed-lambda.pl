% The typed lambda calculus. Project 3 of type systems course

% SYNTAX
term(true). % term(true) doesn't finish immediately because other rules can match
term(false).
term(if(T1, T2, T3)) :- term(T1), term(T2), term(T3).
term(X) :- integer(X).
term(pred(T)) :- term(T).
term(succ(T)) :- term(T).
term(iszero(T)) :- term(T).
term(X) :- atom(X).
term(lambda(X, Type, Term)) :- term(Term).
term(app(T1, T2)) :- term(T1), term(T2).

value(true).
value(false).
value(X) :- numeric_value(X).
value(lambda(_, _, _)).

numeric_value(0).
numeric_value(succ(X)) :- numeric_value(X).

type(boolT).
type(natT).
type(fun(T1, T2)) :- type(T1), type(T2).

%% Evaluation rules
