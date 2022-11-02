% The typed lambda calculus. Project 3, 4, 5 of type systems course

:- [syntax].

% VALUES
value(true).
value(false).
value(lambda(_, _, _)).
value(pair(V1, V2)) :- value(V1), value(V2).
value(X) :- numeric_value(X).

numeric_value(zero).
numeric_value(succ(X)) :- numeric_value(X).


% EVALUATION

% What is a redex?
redex(app(lambda(_, _, _), _)).

% Computation rules
eval(app(lambda(X, T1), T2), R) :-
    value(T2),
    substitute(X, T2, T1, R).
eval(app(lambda(X, _, T1), T2), R) :-
    eval(app(lambda(X, T1), T2), R).

% Can only evaluate the left side if the argument is a value!
eval(app(T1, T2), app(T11, T2)) :- value(T2), eval(T1, T11).
% If argument is not a value, we evaluate it.
eval(app(T1, T2), app(T1, T22)) :- eval(T2, T22).

eval(if(true, T1, _), T1).
eval(if(false, _, T2), T2).
% Numbers are a language construct
eval(iszero(zero), true).
eval(iszero(succ(_)), false).
eval(pred(zero), 0).
eval(pred(succ(NV)), NV) :- numeric_value(NV).

% For if-statements, we evaluate the condition first
eval(if(T1, T2, T3), if(T11, T2, T3)) :- eval(T1, T11).
% This works like normal functions, we evaluate the argument first
eval(iszero(T), iszero(T1)) :- eval(T, T1).
eval(succ(T), succ(T1)) :- eval(T, T1).
eval(pred(T), pred(T1)) :- eval(T, T1).


% PAIRS (Project 3)
eval(fst(pair(V1, _)), V1) :- value(V1).
eval(snd(pair(_, V2)), V2) :- value(V2).

eval(fst(T), T1) :- eval(T, T1).
eval(snd(T), T1) :- eval(T, T1).

eval(pair(T1, T2), pair(T11, T2)) :- eval(T1, T11).
eval(pair(V1, T2), pair(V1, T22)) :- value(V1), eval(T2, T22).

% SUM TYPES (Project 4)
% Eval rules for sum types
eval(case(inject_left(V0, _),
          XLeft, TLeft,
          _, _), Result) :-
    substitute(XLeft, V0, TLeft, Result).

eval(case(inject_right(V0, _),
          _, _,
          XRight, TRight), Result) :-
  substitute(XRight, V0, TRight, Result).

eval(case(T,
          XLeft, TLeft,
          XRight, TRight),
     case(T1,
          XLeft, TLeft,
          XRight, TRight)) :-
    eval(T, T1).


eval(inject_left(Term1, _), inject_left(Term2, _)) :- eval(Term1, Term2).
eval(inject_right(Term1, _), inject_right(Term2, _)) :- eval(Term1, Term2).


% Fix operator (Project 4)
eval(fix(lambda(X, Ty, Body)), T1) :- substitute(X,
                                                fix(lambda(X, Ty, Body)),
                                                Body,
                                                T1).
eval(fix(T), fix(T1)) :- eval(T, T1).

eval(Val, Val) :- value(Val).

% eval(V, V) :- value(V).

:- begin_tests(evaluator).
:- set_prolog_flag(double_quotes, chars).

test(eval0) :- parse(T, "(\\x:Nat.x) 0"),
               eval(T, zero).

test(eval1) :- parse(T, "(\\x:Nat.iszero 0) 0"),
               eval(T, iszero(zero)).

test(eval2) :- parse(T, "(\\y:Nat.(\\x:Nat.iszero x) y) 0"),
               parse(T1, "(\\x:Nat.iszero x) 0"),
               eval(T, T1).

test(eval_case_1) :- parse(T, "case inl 3 as Nat of inl x => x | inr y => y"),
                     parse(Response, "3"),
                     eval(T, Response).


test(eval_case_2) :- parse(T, "case inr 3 as Nat of inl x => x | inr y => y"),
                     parse(Response, "3"),
                     eval(T, Response).

test(eval_fix) :- parse(T, "fix (\\x:Nat.succ x)"),
                  eval(T, succ(T)).

:- end_tests(evaluator).

% Substitution
substitute(_, _, true, true).
substitute(_, _, false, false).
substitute(_, _, iszero, iszero).
substitute(_, _, pred, pred).
substitute(_, _, succ, succ).

substitute(X, V, if(T1, T2, T3), if(T11, T21, T31)) :- substitute(X, V, T1, T11),
                                                       substitute(X, V, T2, T21),
                                                       substitute(X, V, T3, T31).

substitute(X, V, pred(T), pred(T1)) :- substitute(X, V, T, T1).
substitute(X, V, succ(T), succ(T1)) :- substitute(X, V, T, T1).
substitute(X, V, iszero(T), iszero(T1)) :- substitute(X, V, T, T1).
substitute(X, _, lambda(Y, Type, T), lambda(Y, Type, T)) :- X = Y.
substitute(X, V, lambda(Y, Type, T), lambda(Y, Type, T1)) :- free_vars(lambda(Y, Type, T), FreeVars),
                                                             member(X, FreeVars),
                                                             substitute(X, V, T, T1).

substitute(X, V, app(T, T1), app(T21, T22)) :- substitute(X, V, T, T21),
                                               substitute(X, V, T1, T22).

substitute(X, V, variable(X), V).
% Not sure the X =/= Y is needed.
substitute(X, _, variable(Y), variable(Y)) :- X \= Y.
substitute(X, V, fix(T), fix(T1)) :- substitute(X, V, T, T1).

substitute(_, _, T, T) :- value(T).

:- begin_tests(substitute).
% Mortal Kombat woo
test(sub0) :- substitute(y, zero, variable(x), variable(x)).

test(sub1) :- substitute(y, zero, variable(y), zero).

test(sub2) :- substitute(x, zero, app(iszero, zero),
                         app(iszero, zero)).

test(sub3) :- substitute(x, zero, zero, zero).

test(sub4) :-
    substitute(f,
               fix(lambda(f, Ty, Lambda)),
               lambda(y, T, app(variable(f), variable(y))),
               lambda(y, T, app(fix(lambda(f, Ty, Lambda)), variable(y)))).
:- end_tests(substitute).


free_vars(true, []).
free_vars(false, []).
free_vars(zero, []).
free_vars(variable(X), [X]).
free_vars(if(T1, T2, T3), FreeVars) :- free_vars(T3, FV3),
                                       free_vars(T2, FV2),
                                       free_vars(T1, FV1),
                                       union(FV1, FV2, FV21),
                                       union(FV21, FV3, FreeVars).
free_vars(iszero(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(pred(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(succ(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(lambda(Y, _, T1), FreeVars) :- free_vars(T1, FVs),
                                         subtract(FVs, [Y], FreeVars).
free_vars(app(T1, T2), FreeVars) :- free_vars(T1, FV1),
                                    free_vars(T2, FV2),
                                    union(FV1, FV2, FreeVars).
free_vars(fix(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(fst(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(snd(T), FreeVars) :- free_vars(T, FreeVars).
free_vars(pair(T1, T2), FreeVars) :- free_vars(T1, FreeVars1),
                                     free_vars(T2, FreeVars2),
                                     union(FreeVars1, FreeVars2, FreeVars).


:- begin_tests(free_vars).
test(free_vars0) :-
    free_vars(lambda(x, natT, app(variable(x), variable(y))),
              [y]).

test(free_vars1) :-
    free_vars(lambda(x, natT, variable(x)),
              []).

:- end_tests(free_vars).

% TYPES
% Build-in functions
typing(_, true, boolT).
typing(_, false, boolT).
typing(_, zero, natT).
% C is the context
typing(C, succ(T), natT) :- typing(C, T, natT).
typing(C, pred(T), natT) :- typing(C, T, natT).
typing(C, iszero(T), boolT) :- typing(C, T, natT).
typing(C, fst(T), Type) :- typing(C, T, pairT(Type, _)).
typing(C, snd(T), Type) :- typing(C, T, pairT(_, Type)).

typing(Ctxt, if(T1, T2, T3), T) :- typing(Ctxt, T1, boolT),
                                   typing(Ctxt, T2, T),
                                   typing(Ctxt, T3, T).


% Functions
typing(Ctxt, lambda(X, Type, Term), funT(Type, Type2)) :-
    append([[X, Type]], Ctxt, Ctxt1), % I add the type of input to the context
    typing(Ctxt1, Term, Type2). % and I can type the body with this new context

typing(Ctxt, lambda(X, Body), funT(Type1, Type2)) :-
    typing(Ctxt, lambda(X, Type1, Body), funT(Type1, Type2)).

% Function application
% Normal types
typing(Ctxt, app(T1, T2), T12) :- typing(Ctxt, T1, funT(ArgType, T12)),
                                  typing(Ctxt, T2, ArgType).

% Type schemes
typing(Ctxt, app(T1, T2), T12) :- typing(Ctxt, T1, funT(FreshArgType, ResultType)), % Create fresh type variables
                                  typing(Ctxt, T2, ArgType),
                                  unifiable(FreshArgType, ArgType, _), % Type of arguments has same shape
                                  unifiable(ResultType, T12, _). % Type of result have same shape

% Pairs
typing(Ctxt, pair(Term1, Term2), pairT(Type1, Type2)) :- typing(Ctxt, Term1, Type1),
                                                         typing(Ctxt, Term2, Type2).


typing(Ctxt, inject_left(Term, sumT(T1, T2)), sumT(T1, T2)) :-
    typing(Ctxt, Term, T1).
typing(Ctxt, inject_right(Term, sumT(T1, T2)), sumT(T1, T2)) :-
    typing(Ctxt, Term, T2).

typing(Ctxt, case(Term,
                  LeftX, LeftTerm,
                  RightX, RightTerm), T) :-
    typing(Ctxt, Term, sumT(TLeft, TRight)),
    typing([[LeftX, TLeft]|Ctxt], LeftTerm, T),
    typing([[RightX, TRight]|Ctxt], RightTerm, T).

% Typing of Fix
typing(Ctxt, fix(Term), Type) :- typing(Ctxt, Term, funT(Type, Type)).

% Variables
typing(Ctxt, variable(X), T) :- member([X, T], Ctxt).

:- begin_tests(typing).
:- set_prolog_flag(double_quotes, chars).

test(type_if) :- parse(T, "if true then false else true"),
                 typing([], T, boolT).

test(type_if2, [fail]) :- parse(T, "if true then false else 0"),
                          typing([], T, _).

test(type_if3) :- parse(T, "if f 0 then x else y"),
                  typing([[f, funT(natT, boolT)],
                          [x, Ty],
                          [y, Ty]],
                         T,
                         Ty).


test(type_pair) :- parse(T, "{false, x}"),
                   typing([[x, natT]], T, pairT(boolT, natT)).

test(type_fst) :- parse(T, "fst {false, x}"),
                  typing([[x, natT]], T, boolT).

test(type_snd) :- parse(T, "snd {false, x}"),
                  typing([[x, natT]], T, natT).

test(type_fun) :- parse(T, "\\x:Nat.iszero x"),
                  typing([],T, funT(natT,boolT)).

test(type_injection_left) :- parse(T, "inl 12 as Nat + Bool"),
                             typing([], T, sumT(natT, boolT)).

test(type_injection_right) :- parse(T, "inr true as Nat + Bool"),
                              typing([], T, sumT(natT, boolT)).

test(type_case_left) :- parse(T, "case node of inl x => 1 | inr y => y"),
                        typing([[node, sumT(_, natT)]], T,
                               natT).

test(type_case_right) :- parse(T, "case node of inl x => x | inr y => 0"),
                         typing([[node, sumT(natT, _)]], T,
                                natT).

test(type_var) :- parse(T, "x"),
                  typing([[x, natT]], T, natT).

test(fixpoint_f) :-
    parse(T, "fix f"),
    typing([[f, funT(funT(natT, natT), funT(natT, natT))]],
           T,
           funT(natT, natT)).

test(fixpoint_factorial) :- parse(T, "letrec fact : Nat -> Nat = \\x:Nat . if iszero x then 1 else multiply x (fact (pred x)) in fact 3"),
                            typing([[multiply, funT(natT, funT(natT, natT))]],
                                   T,
                                   natT).

test(type_lambda) :-
    parse(T, "\\b . if b then true else false"),
    typing([], T, funT(boolT, boolT)).

test(type_polymorphism) :-
    parse(T, "let double = (\\f . \\ x . f (f x)) in if (double (\\x . if x then false else true) false) then double (\\x . succ x) 0 else 0"),
    typing([], T, natT).

test(type_scheme) :-
    parse(T, "(\\x:Nat.iszero x) 5"), typing([], T, boolT).

:- end_tests(typing).


% BIG STEP EVALUATION

bigstep(T, V) :- eval(T, V), value(V), writeln(T).
bigstep(T, V) :- eval(T, T1), writeln(T), bigstep(T1, V).

evaluate(T, V) :- typing([], T, _),
                  bigstep(T, V).

typeof(Term, Type) :- typing([], Term, Type).

:- begin_tests(evaluate).
:- set_prolog_flag(double_quotes, chars).

test(iseven0) :-
    parse(T,
          "letrec iseven : Nat -> Bool = \\x:Nat. if iszero x then true else if iszero (pred x) then false else iseven (pred (pred x)) in iseven 0"),
    evaluate(T, true).

test(iseven2) :-
    parse(T,
          "letrec iseven : Nat -> Bool = \\x:Nat. if iszero x then true else if iszero (pred x) then false else iseven (pred (pred x)) in iseven 2"),
    evaluate(T, true).

test(iseven7) :-
    parse(T,
          "letrec iseven : Nat -> Bool = \\x:Nat. if iszero x then true else if iszero (pred x) then false else iseven (pred (pred x)) in iseven 7"),
    evaluate(T, false).
:- end_tests(evaluate).

% TESTS
% eval(app(lambda("x", _, 0), succ(0)), R). R = 0;
% typing([], app(lambda("x", boolT, "x"), true), T).
% eval(pair(true, iszero(0)), T).
% eval(pair(iszero(0), iszero(succ(0))), T).
% typing(L, snd(pair(false, "x")), natT).
