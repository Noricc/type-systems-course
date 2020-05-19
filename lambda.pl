% Implementation of the untyped lambda calculus.

% SYNTAX
ident(X) :- atom(X).

term(lambda(X, T)) :- ident(X), term(T).
term(app(T1, T2)) :- term(T1), term(T2).
term(X) :- ident(X).

% Parenthesis not needed here

% VALUES
value(lambda(X, T)) :- ident(X), term(T).

% EVALUATION
eval(app(T1, T2), app(T11, T2)) :- eval(T1, T11).
eval(app(T1, T2), app(T1, T21)) :- eval(T2, T21).
eval(lambda(X, T1), lambda(X, T11)) :- eval(T1, T11).
eval(app(lambda(X, T1), T2), Result) :- substitute(X, T2, T1, Result).

% Implicit rule?
eval(X, X) :- value(X).

% SUBSTITUTION
substitute(X, S, X, S) :- atom(X).
substitute(X, _, Y, Y) :- atom(X), dif(X, Y).
substitute(X, S, lambda(Y, T1), lambda(Y, T1)) :- X = Y.
substitute(X, S, lambda(Y, T1), lambda(Y, _sub)) :- dif(X, Y), substitute(X, S, T1, _sub).
substitute(X, S, app(T1, T2), app(T11, T21)) :- substitute(X, S, T1, T11),
                                                substitute(X, S, T2, T22).

% FREE VARIABLES
fv(X, [X]) :- atom(X).
fv(lambda(X, T1), R) :- fv(T1, _free), delete(_free, X, R).
fv(app(T1, T2), R) :- fv(T1, _fv1), fv(T2, _fv2), union(_fv1, _fv2, R).
