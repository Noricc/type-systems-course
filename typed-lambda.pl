% The typed lambda calculus. Project 3 of type systems course

% SYNTAX
% It's usually a good idea to order the rules
% so that the most generally matching are tried last.
term(true). % term(true) doesn't finish immediately because other rules can match
term(false).
term(if(T1, T2, T3)) :- term(T1), term(T2), term(T3).
term(pred(T)) :- term(T).
term(succ(T)) :- term(T).
term(iszero(T)) :- term(T).
term(lambda(X, Type, Term)) :- term(Term).
term(app(T1, T2)) :- term(T1), term(T2).
term(X) :- integer(X).
term(X) :- atom(X).

value(true).
value(false).
value(lambda(_, _, _)).
value(X) :- numeric_value(X).

numeric_value(0).
numeric_value(succ(X)) :- numeric_value(X).

type(boolT).
type(natT).
type(fun(T1, T2)) :- type(T1), type(T2).

% Evaluation rules

% Computation rules
eval(if(true, T1, _), T1).
eval(if(false, _, T2), T2).
eval(iszero(0), true).
eval(iszero(succ(_)), false).
eval(pred(0), 0).
eval(pred(succ(NV)), NV).

eval(if(T1, T2, T3), if(T11, T2, T3)) :- eval(T1, T11).
eval(iszero(T), iszero(T1)) :- eval(T, T1).
eval(succ(T), succ(T1)) :- eval(T, T1).
eval(pred(T), pred(T1)) :- eval(T, T1).

eval(app(lambda(X, _, T1), V), R) :- value(V), substitute(X, V, T1, R).
eval(app(T1, T2), app(T11, T2)) :- eval(T1, T11).
eval(app(V1, T2), app(V1, T21)) :- value(V1), term(T2), eval(T2, T21).
% eval(V, V) :- value(V).

% Substitution
substitute(_, _, true, true).
substitute(_, _, false, false).
substitute(X, V, if(T1, T2, T3), if(T11, T21, T31)) :- substitute(X, V, T1, T11),
                                                       substitute(X, V, T2, T21),
                                                       substitute(X, V, T3, T31).

substitute(X, V, pred(T), pred(T1)) :- substitute(X, V, T, T1).
substitute(X, V, succ(T), succ(T1)) :- substitute(X, V, T, T1).
substitute(X, V, iszero(T), iszero(T1)) :- substitute(X, V, T, T1).
substitute(X, V, lambda(Y, Type, T), lambda(Y, Type, T)) :- X = Y.
substitute(X, V, lambda(Y, Type, T), lambda(Y, Type, T1)) :- free_vars(lambda(Y, Type, T), FreeVars),
                                                             member(X, FreeVars),
                                                             substitute(X, V, T, T1).

substitute(X, V, app(T, T1), app(T21, T22)) :- substitute(X, V, T, T21),
                                               substitute(X, V, T1, T22).

substitute(X, V, T, T) :- integer(T).
substitute(X, V, Var, V) :- atom(Var), X = Var.
substitute(X, V, Var, Var) :- atom(Var).

free_vars(true, []).
free_vars(false, []).
free_vars(X, [X]) :- atom(X).
free_vars(if(T1, T2, T3), FreeVars) :- free_vars(T3, FV3),
                                       free_vars(T2, FV2),
                                       free_vars(T1, FV1),
                                       union(FV1, FV2, FV21),
                                       union(FV21, FV3, FreeVars).
free_vars(iszero(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(pred(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(succ(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(lambda(Y, T, T1), FreeVars) :- free_vars(T1, FVs),
                                         subtract(FVs, [Y], FreeVars).
free_vars(app(T1, T2), FreeVars) :- free_vars(T1, FV1),
                                    free_vars(T2, FV2),
                                    union(FV1, FV2, FreeVars).




bigstep(T, V) :- eval(T, V).
bigstep(T, V) :- eval(T, T1), bigstep(T1, V).


% TESTS
% eval(app(lambda(x, _, 0), succ(0)), R). R = 0;
