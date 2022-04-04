% SYNTAX
% Where we try out prolog DCG parsing

% To allow using string notation....
:- use_module(library(tabling)).

:- set_prolog_flag(double_quotes, chars).

% Basic stuff
char(C) --> [C], {code_type(C, alpha)}.

varname([C|Cs]) --> char(C), varname(Cs).
varname([C]) --> char(C).

variable(V) --> varname(Cs), { atom_chars(V, Cs) }.

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
nat(A,N) --> digit(D), { A1 is A*10 + D }, nat(A1,N).
nat(N,N) --> [].

symbol_num(zero, 0).
symbol_num(succ(S), N) :- integer(N),
                          N > 0,
                          N1 is (N - 1),
                          symbol_num(S, N1).

keyword(letrec) --> "letrec".
keyword(let) --> "let".
keyword(pred) --> "pred".
keyword(succ) --> "succ".
keyword(iszero) --> "iszero".
keyword(fst) --> "fst".
keyword(snd) --> "snd".
keyword(if) --> "if".
keyword(then) --> "then".
keyword(else) --> "else".
keyword(true) --> "true".
keyword(false) --> "false".
keyword(case) --> "case".
keyword(as) --> "as".
keyword(inl) --> "inl".
keyword(inr) --> "inr".
keyword(fix) --> "fix".
keyword(of) --> "of".
keyword(in) --> "in".


sign(lparen) --> "(".
sign(rparen) --> ")".
sign(lcurly) --> "{".
sign(rcurly) --> "}".
sign(lambda) --> "\\".
sign(:) --> ":".
sign(pipe) --> "|".
sign(=) --> "=".
sign(.) --> ".".
sign(,) --> ",".
sign('=>') --> "=>".
sign('->') --> "->".
sign(+) --> "+".
sign(*) --> "*".

symbol(S) --> keyword(S).
symbol(S) --> sign(S).
symbol(number(N)) --> nat(N).
symbol(variable(V)) --> variable(V).

whitespace() --> " ", whitespace().
whitespace() --> " ".
whitespace() --> [].

lexer([]) --> [].
lexer([S]) --> symbol(S).
lexer([S|Ss]) --> symbol(S), whitespace(), lexer(Ss).

:- begin_tests(lexer).
:- set_prolog_flag(double_quotes, chars).

test(number) :- phrase(lexer([number(123)]),
                       "123").

test(variable) :- phrase(lexer([variable(xyz)]),
                         "xyz").

test(keyword) :- phrase(lexer([true, false, true]),
                        "true false true").

test(lambda) :- phrase(sign(lambda), "\\").
:- end_tests(lexer).

% TERMS
% We express them with tokens
primary(true) --> [true].
primary(false) --> [false].
primary(zero) --> [number(0)].
primary(succ(S)) --> [number(N)], { symbol_num(succ(S), N) }.
primary(variable(V)) --> [variable(V)].
primary(pair(T1, T2)) --> pair(T1, T2).
primary(T) --> [lparen], term(T), [rparen].

% Terms
term(iszero(T)) --> [iszero], term(T).
term(pred(T)) --> [pred], term(T).
term(succ(T)) --> [succ], term(T).
term(fst(T)) --> [fst], term(T).
term(snd(T)) --> [snd], term(T).
term(lambda(X, T, Body)) --> abstraction(X, T, Body).
term(fix(Term)) --> [fix], term(Term).
term(inject_left(Term, Type)) --> [inl], term(Term), [as], type(Type).
term(inject_right(Term, Type)) --> [inr], term(Term), [as], type(Type).
term(if(Cond, Then, Else)) --> if(Cond, Then, Else).
term(case(Term, LeftX, LeftTerm, RightX, RightTerm)) --> [case], term(Term), [of],
                                                         case_left(LeftX, LeftTerm), [pipe],
                                                         case_right(RightX, RightTerm).
term(app(lambda(X, T, T2),
         fix(lambda(X, T, T1)))) --> [letrec], [variable(X)], [:], type(T), [=], term(T1), [in], term(T2).
term(app(lambda(X, T, T2), T1)) --> [let], [variable(X)], [:], type(T), [=], term(T1), [in], term(T2).
term(app(T, X)) --> application(Args), { left_assoc(app(T, X), Args) }.
term(T) --> primary(T).

abstraction(X, T, Body) --> [lambda], [variable(X)], [:], type(T), [.], term(Body).

application([F, X|Xs]) --> primary(F), primary(X), arguments(Xs).

arguments([]) --> [].
arguments([T|Ts]) --> primary(T), arguments(Ts).

if(Cond, Then, Else) --> [if], primary(Cond), [then], term(Then), [else], term(Else).

pair(T1, T2) --> [lcurly], term(T1), [,], term(T2), [rcurly].

case_left(X, Term) -->  [inl], [variable(X)], ['=>'], term(Term).
case_right(X, Term) --> [inr], [variable(X)], ['=>'], term(Term).

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
type(boolT) --> [variable('Bool')].
type(natT) --> [variable('Nat')].
type(funT(T1, T2)) --> type(T1), ['->'], type(T2).
type(sumT(T1, T2)) --> type(T1), [+], type(T2).
type(pairT(T1, T2)) --> type(T1), [*], type(T2).
type(T) --> [lparen], type(T), [rparen].

parse(Ast, String) :- phrase(lexer(Symbols), String),
                      phrase(term(Ast), Symbols), !.

:- begin_tests(parser).
:- set_prolog_flag(double_quotes, chars).
test(varname_x) :- phrase(varname([x]), "x").
test(varname_xyz) :- phrase(varname([x, y, z]), "xyz").

test(variable_xyz) :- phrase(variable(xyz), "xyz").

test(primary_true) :- parse(true, "true").
test(primary_false) :- parse(false, "false").
test(primary_zero) :- parse(zero, "0").
test(primary_variable) :- parse(variable(x), "x").
test(primary_int) :- parse(succ(zero), "1").

test(identity_function) :- parse(lambda(x, natT, variable(x)), "\\ x : Nat . x").
test(identity_function2) :- parse(lambda(x, natT, variable(x)), "\\ x : Nat . (x)").

test(application_in_abstraction) :- parse(lambda(x,
                                                 natT,
                                                 app(variable(snd),
                                                     variable(x))),
                                           "\\x:Nat.snd x").


test(left_assoc_2) :- left_assoc(app(f, g), [f, g]).
test(left_assoc_3) :- left_assoc(app(app(f, g), h), [f, g, h]).
test(left_assoc_4) :- left_assoc(app(app(app(f, g), h), i), [f, g, h, i]).

test(function_application) :- parse(app(variable(f), variable(g)), "f g").
test(function_application_left_assoc) :-
    parse(Term, "(f g) h"),
    parse(Term, "f g h").

test(function_application_identity_zero) :-
    phrase(lexer(Text), "(\\x:Nat.x) 0"),
    phrase(application([lambda(x, natT, variable(x)), zero]), Text).

test(term_application_identity_zero) :-
    parse(app(lambda(x, natT, variable(x)), zero),
           "(\\x:Nat.x) 0").

test(term_application_identity_true) :-
    parse(app(lambda(x, natT, variable(x)), true),
           "(\\x:Nat.x) true").

test(term_application_id_one) :-
    parse(app(lambda(x,natT,
                     app(variable(snd),
                         variable(x))),
              succ(zero)),
           "(\\x:Nat.snd x) 1").

test(complex_term) :-
    parse(lambda(x, funT(natT,boolT),
                 lambda(y,natT, app(variable(x), variable(y)))), "(\\x:Nat->Bool.(\\y:Nat.(x y)))").

test(complex_term) :-
    parse(_, "(\\x:Nat->Bool.(\\y:Nat.(x y))) (\\x:Nat.(iszero x)) 0").

test(nested_if) :-
    parse(if(true, false, if(false, true, false)),
          "if true then false else if false then true else false").

test(nested_if2) :-
    parse(if(true, if(false, true, false), true),
          "if true then if false then true else false else true").

test(pair) :-
    parse(pair(zero, false),
          "{0, false}").

test(pair1) :-
    parse(pair(pair(variable(x), variable(y)), variable(z)),
          "{{x, y}, z}").

test(pairargs) :-
    parse(app(variable(fst), pair(variable(x),
                                  variable(y))),
          "fst {x, y}").

test(case) :-
    parse(case(variable(x),
               x, succ(zero),
               y, zero),
          "case x of inl x => 1 | inr y => 0").

test(inject_left) :-
    parse(inject_left(variable(x), sumT(natT,boolT)),
          "inl x as Nat + Bool").

test(inject_left_2) :-
    parse(inject_left(succ(_), sumT(natT, boolT)),
          "inl 12 as Nat + Bool").

test(inject_right) :-
    parse(inject_right(variable(x), sumT(boolT, natT)),
          "inr x as Bool + Nat").

test(case_inject) :-
    parse(X, "case inl 0 as Nat of inl x => x | inr y => y"),
    X = case(inject_left(zero, natT),
             x, variable(x),
             y, variable(y)).


test(let) :-
    parse(app(lambda(x,
                     natT,
                     app(variable(iszero), variable(x))),
              succ(succ(succ(zero)))),
          "let x:Nat = 3 in iszero x").

test(fix) :-
    parse(fix(app(variable(f), variable(g))),
          "fix (f g)").

test(iseven) :-
    parse(_,
          "\\ie:Nat->Bool.\\x:Nat.if (iszero x) then true else if (iszero (pred x)) then false else ie (pred (pred x))").

test(letrec) :-
    parse(T, "letrec x : Nat = y in x"),
    parse(T, "let x : Nat = fix (\\x : Nat . y) in x").

:- end_tests(parser).
