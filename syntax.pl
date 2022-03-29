% SYNTAX
% Where we try out prolog DCG parsing

% To allow using string notation....

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(tabling)).

char(C) --> [C], {code_type(C, alpha)}.

varname([C]) --> char(C).
varname([C|Cs]) --> char(C), varname(Cs).

% TERMS
primary(true) --> "true".
primary(false) --> "false".
primary(zero) --> "0".
primary(variable(V)) --> varname(V).
primary(T) --> "(", term(T), ")".

builtinfunction(pred) --> "pred".
builtinfunction(succ) --> "succ".
builtinfunction(iszero) --> "iszero".

abstraction(X, T, Body) --> "\\", primary(variable(X)), ":", type(T), ".", term(Body).

:- table application/4.
application(T, T1) --> function(T), " ", primary(T1).
application(app(T, T1), T2) --> application(T, T1), " ", primary(T2).

function(T) --> builtinfunction(T).
function(T) --> primary(T).

if(Cond, Then, Else) --> "if ", term(Cond), " then ", term(Then), " else ", term(Else).

term(app(F, X)) --> application(F, X).
term(lambda(X, T, Body)) --> abstraction(X, T, Body).
term(if(Cond, Then, Else)) --> if(Cond, Then, Else).
term(T) --> primary(T).

% Left recursive: see https://github.com/Anniepoo/swipldcgtut/blob/master/dcgcourse.adoc#1-definite-clause-grammars


% VALUES
value(true) --> "true".
value(false) --> "false".
value(Number) --> numbervalue(Number).
value(lambda(X, T, Body)) --> "\\", term(variable(X)), ":", type(T), ".", term(Body).

numbervalue(zero) --> "0".
numbervalue(succ(N)) --> "succ", "(", numbervalue(N), ")".

type(boolT) --> "Bool".
type(natT) --> "Nat".
type(T) --> "(", type(T), ")".
type(funT(T1, T2)) --> type(T1), "->", type(T2).
