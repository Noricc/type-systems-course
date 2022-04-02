% SYNTAX
% Where we try out prolog DCG parsing

% To allow using string notation....
:- use_module(library(tabling)).

:- set_prolog_flag(double_quotes, chars).

% Basic stuff
char(C) --> [C], {code_type(C, alpha)}.

varname([C]) --> char(C).
varname([C|Cs]) --> char(C), varname(Cs).

variable(pred) --> "pred".
variable(succ) --> "succ".
variable(iszero) --> "iszero".
variable(fst) --> "fst".
variable(snd) --> "snd".
variable(variable(V)) --> varname(Cs), { atom_chars(V, Cs) }.

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

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
primary(V) --> variable(V).
primary(T) --> "(", term(T), ")".
primary(pair(T1, T2)) --> pair(T1, T2).

abstraction(X, T, Body) --> "\\", variable(X), ":", type(T), ".", term(Body).

% Left recursive: see https://github.com/Anniepoo/swipldcgtut/blob/master/dcgcourse.adoc#1-definite-clause-grammars
% We try to use tabling to fix the left-recursion
% :- table application/4.
application([F, X|Xs]) --> primary(F), " ", primary(X), arguments(Xs).

arguments([]) --> [].
arguments([T|Ts]) --> " ", primary(T), arguments(Ts).

if(Cond, Then, Else) --> "if ", term(Cond), " then ", term(Then), " else ", term(Else).

pair(T1, T2) --> "{", term(T1), ", ", term(T2), "}".

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

:- table type/3.
type(boolT) --> "Bool".
type(natT) --> "Nat".
type(funT(T1, T2)) --> type(T1), "->", type(T2).
type(T) --> "(", type(T), ")".

:- begin_tests(parser).
:- set_prolog_flag(double_quotes, chars).
test(varname_x) :- phrase(varname([x]), "x").
test(varname_xyz) :- phrase(varname([x, y, z]), "xyz").

test(variable_xyz) :- phrase(variable(variable(xyz)), "xyz").

test(primary_true) :- phrase(primary(true), "true").
test(primary_false) :- phrase(primary(false), "false").
test(primary_zero) :- phrase(primary(zero), "0").
test(primary_variable) :- phrase(primary(variable(x)), "x").
test(primary_int) :- phrase(primary(succ(zero)), "1").

test(identity_function) :- phrase(abstraction(variable(x), natT, variable(x)), "\\x:Nat.x").
test(identity_function2) :- phrase(abstraction(variable(x), natT, variable(x)), "\\x:Nat.(x)").

test(application_in_abstraction) :- phrase(abstraction(variable(x),
                                                       natT,
                                                       app(snd,
                                                           variable(x))),
                                           "\\x:Nat.snd x").


test(left_assoc_2) :- left_assoc(app(f, g), [f, g]).
test(left_assoc_3) :- left_assoc(app(app(f, g), h), [f, g, h]).
test(left_assoc_4) :- left_assoc(app(app(app(f, g), h), i), [f, g, h, i]).

test(function_application) :- phrase(term(app(variable(f), variable(g))), "f g").
test(function_application_left_assoc) :-
    phrase(term(Ls), "(f g) h"),
    phrase(term(Ls), "f g h").

test(function_application_identity_zero) :-
    phrase(application([lambda(variable(x), natT, variable(x)), zero]),
           "(\\x:Nat.x) 0").

test(term_application_identity_zero) :-
    phrase(term(app(lambda(variable(x), natT, variable(x)), zero)),
           "(\\x:Nat.x) 0").

test(term_application_identity_true) :-
    phrase(term(app(lambda(variable(x), natT, variable(x)), true)),
           "(\\x:Nat.x) true").

test(term_application_id_one) :-
    phrase(term(app(lambda(variable(x),natT,
                           app(snd, variable(x))),
                    succ(zero))),
           "(\\x:Nat.snd x) 1").

test(complex_term) :-
    phrase(term(lambda(variable(x),funT(_,_), _)), "(\\x:Nat->Bool.(\\y:Nat.(x y)))").

test(complex_term) :-
    phrase(term(_), "(\\x:Nat->Bool.(\\y:Nat.(x y))) (\\x:Nat.(iszero x)) 0").

test(pair) :-
    phrase(term(pair(zero, false)),
           "{0, false}").
test(pair1) :-
    phrase(term(pair(pair(variable(x), variable(y)), variable(z))),
           "{{x, y}, z}").

test(pairargs) :-
    phrase(term(app(fst, pair(variable(x),
                              variable(y)))),
           "fst {x, y}").


:- end_tests(parser).
