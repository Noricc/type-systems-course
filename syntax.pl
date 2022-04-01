% SYNTAX
% Where we try out prolog DCG parsing

% To allow using string notation....

:- set_prolog_flag(double_quotes, chars).

% Basic stuff
char(C) --> [C], {code_type(C, alpha)}.

varname([C]) --> char(C).
varname([C|Cs]) --> char(C), varname(Cs).

digit(N) --> [C], { code_type(C, digit), atom_number(C, N) }.

% Copied from S-Overflow
nat(N)   --> digit(D), nat(D,N).
nat(N,N) --> [].
nat(A,N) --> digit(D), { A1 is A*10 + D }, nat(A1,N).

symbol_num(zero, 0).
symbol_num(succ(S), N) :- integer(N),
                          N > 0,
                          N1 is (N - 1),
                          symbol_num(S, N1).

% TERMS
primary(true) --> "true".
primary(false) --> "false".
primary(zero) --> "0".
primary(succ(S)) --> nat(N), { symbol_num(succ(S), N) }.
primary(variable(V)) --> varname(V).
primary(T) --> "(", term(T), ")".

builtinfunction(pred) --> "pred".
builtinfunction(succ) --> "succ".
builtinfunction(iszero) --> "iszero".

abstraction(X, T, Body) --> "\\", varname(X), ":", type(T), ".", term(Body).

% Left recursive: see https://github.com/Anniepoo/swipldcgtut/blob/master/dcgcourse.adoc#1-definite-clause-grammars
% We try to use tabling to fix the left-recursion
% :- table application/4.
application([F, X|Xs]) --> fun(F), " ", primary(X), arguments(Xs).

fun(F) --> builtinfunction(F), !. % We cut so we don't try to match "pred" with a variable.
fun(F) --> primary(F).

arguments([]) --> [].
arguments([T|Ts]) --> " ", primary(T), arguments(Ts).

if(Cond, Then, Else) --> "if ", term(Cond), " then ", term(Then), " else ", term(Else).

% Terms
term(lambda(X, T, Body)) --> abstraction(X, T, Body).
term(app(T, X)) --> application(Args), { left_assoc(app(T, X), Args) }.
term(if(Cond, Then, Else)) --> if(Cond, Then, Else).
term(T) --> primary(T).

% Left associativity of application
bind(T, X, app(X, T)).
left_assoc(T, [X|Xs]) :- foldl(bind, Xs, X, T).

% VALUES
value(true) --> "true".
value(false) --> "false".
value(number(N)) --> numbervalue(N).
value(lambda(X, T, Body)) --> "\\", term(variable(X)), ":", type(T), ".", term(Body).

numbervalue(zero) --> "0".
numbervalue(succ(N)) --> "succ", "(", numbervalue(N), ")".

type(boolT) --> "Bool".
type(natT) --> "Nat".
type(T) --> "(", type(T), ")".
type(funT(T1, T2)) --> type(T1), "->", type(T2).

:- begin_tests(parser).
:- set_prolog_flag(double_quotes, chars).
test(varname_x) :- phrase(varname([x]), "x").
test(varname_xyz) :- phrase(varname([x, y, z]), "xyz").

test(primary_true) :- phrase(primary(true), "true").
test(primary_false) :- phrase(primary(false), "false").
test(primary_zero) :- phrase(primary(zero), "0").
test(primary_variable) :- phrase(primary(variable([x])), "x").
test(primary_int) :- phrase(primary(int(1)), "1").

test(builtin_pred) :- phrase(builtinfunction(pred), "pred").
test(builtin_succ) :- phrase(builtinfunction(succ), "succ").
test(builtin_iszero) :- phrase(builtinfunction(iszero), "iszero").

test(identity_function) :- phrase(abstraction([x], natT, variable([x])), "\\x:Nat.x").
test(identity_function2) :- phrase(abstraction([x], natT, variable([x])), "\\x:Nat.(x)").

test(application_in_abstraction) :- phrase(abstraction([x], natT,
                                                       app(variable([s, n, d]),
                                                           variable([x]))),
                                           "\\x:Nat.snd x").


test(left_assoc_2) :- left_assoc(app(f, g), [f, g]).
test(left_assoc_3) :- left_assoc(app(app(f, g), h), [f, g, h]).
test(left_assoc_4) :- left_assoc(app(app(app(f, g), h), i), [f, g, h, i]).

test(function_application) :- phrase(term(app(variable([f]), variable([g]))), "f g").
test(function_application_left_assoc) :-
    phrase(term(Ls), "(f g) h"),
    phrase(term(Ls), "f g h").

test(function_application_identity_zero) :-
    phrase(application([lambda([x], natT, variable([x])), zero]),
           "(\\x:Nat.x) 0").

test(term_application_identity_zero) :-
    phrase(term(app(lambda([x], natT, variable([x])), zero)),
           "(\\x:Nat.x) 0").

test(term_application_identity_true) :-
    phrase(term(app(lambda([x], natT, variable([x])), true)),
           "(\\x:Nat.x) true").

test(term_application_id_one) :-
    phrase(term(app(lambda([x],natT,app(variable([s,n,d]),
                                        variable([x]))),
                    int(1))),
           "(\\x:Nat.snd x) 1").

:- end_tests(parser).
