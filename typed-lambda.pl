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
term(lambda(X, T, Term)) :- variable(X), type(T), term(Term).
term(app(T1, T2)) :- term(T1), term(T2).
term(pair(T1, T2)) :- term(T1), term(T2).
term(fst(T)) :- term(T).
term(snd(T)) :- term(T).
% "inject left" and "inject right"
% I don't understand what "inject" means here.
term(inl(Term, Type)) :- term(Term), type(Type).
term(inr(Term, Type)) :- term(Term), type(Type).
term(case(Term, LeftX, LeftTerm, RightX, RightTerm)) :- term(Term),
                                                        variable(LeftX), term(LeftTerm),
                                                        variable(RightX), term(RightTerm).
term(X) :- integer(X).
term(X) :- variable(X).

variable(X) :- atom(X).

% TYPES
type(boolT).
type(natT).
type(funT(T1, T2)) :- type(T1), type(T2).
type(pairT(T1, T2)) :-  type(T1), type(T2).
type(sum(T1, T2)) :- type(T1), type(T2).

:- begin_tests(term).
test(term1) :- term(app(lambda(x,natT,iszero(x)), 0)).
test(term2) :- term(lambda(x,funT(natT,boolT),
                           lambda(y,natT,app(x, y)))).

% (\x : Nat->Bool. (\y: Nat.(x y))) (\x : Nat.(iszero x)) 0
test(term3) :- term(app(app(lambda(x,funT(natT,boolT),
                                   lambda(y,natT, app(x, y))),
                            lambda(x,natT,iszero(x))),0)).
:- end_tests(term).


desugar(let(X, Type, Term1, Term2),
        app(lambda(X, Type, Term2), Term1)).

% VALUES
value(true).
value(false).
value(lambda(_, _, _)).
value(pair(V1, V2)) :- value(V1), value(V2).
value(X) :- numeric_value(X).

numeric_value(0).
numeric_value(succ(X)) :- numeric_value(X).


% Evaluation rules

% Computation rules
eval(if(true, T1, _), T1).
eval(if(false, _, T2), T2).
eval(iszero(0), true).
eval(iszero(succ(NV)), false) :- numeric_value(NV).
eval(pred(0), 0).
eval(pred(succ(NV)), NV) :- numeric_value(NV).

eval(if(T1, T2, T3), if(T11, T2, T3)) :- eval(T1, T11).
eval(iszero(T), iszero(T1)) :- eval(T, T1).
eval(succ(T), succ(T1)) :- eval(T, T1).
eval(pred(T), pred(T1)) :- eval(T, T1).

eval(app(lambda(X, _, T1), V), R) :- value(V), substitute(X, V, T1, R).
eval(app(T1, T2), app(T11, T2)) :- eval(T1, T11).
eval(app(V1, T2), app(V1, T21)) :- value(V1), term(T2), eval(T2, T21).

eval(fst(pair(V1, _)), V1) :- value(V1).
eval(snd(pair(_, V2)), V2) :- value(V2).

eval(fst(T), T1) :- eval(T, T1).
eval(snd(T), T1) :- eval(T, T1).

eval(pair(T1, T2), pair(T11, T2)) :- eval(T1, T11).
eval(pair(V1, T2), pair(V1, T22)) :- value(V1), eval(T2, T22).

% Eval rules for sum types
eval(case(T, XLeft, TLeft, XRight, TRight),
     case(T1, XLeft, TLeft, XRight, TRight)) :-
    % Left
    eval(case(inl(V0), XLeft, TLeft, XRight, TRight), Result1),
    substitute(XLeft, V0, TLeft, Result1),
    eval(case(inr(V1), XLeft, TLeft, XRight, TRight), Result2),
    substitute(XRight, V1, TRight, Result2),
    eval(T, T1).

eval(inl(Term1, _), inl(Term2, _)) :- eval(Term1, Term2).
eval(inr(Term1, _), inr(Term2, _)) :- eval(Term1, Term2).

% eval(V, V) :- value(V).

:- begin_tests(evaluator).
test(eval0) :- eval(app(lambda(x,natT,x), 0), 0).
test(eval1) :- eval(app(lambda(x,natT,iszero(0)), 0),
                    iszero(0)).
test(eval2) :- eval(app(lambda(y,natT,
                               app(lambda(x,natT,iszero(x)), y)),
                        0),
                    app(lambda(x,natT,iszero(x)),
                        0)).

:- end_tests(evaluator).

% Substitution
substitute(_, _, true, true).
substitute(_, _, false, false).
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

substitute(_, _, T, T) :- integer(T).

substitute(X, V, X, V) :- variable(X).
% If the replaced variable is not v1, we leave it as is.
substitute(X, _, V1, V1) :- X \== V1.

:- begin_tests(substitute).
% Mortal Kombat woo
test(sub0) :- substitute(y, 0, x, x).

test(sub1) :- substitute(y, 0, y, 0).
:- end_tests(substitute).


free_vars(true, []).
free_vars(false, []).
free_vars(X, [X]) :- variable(X).
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


bigstep(T, V) :- eval(T, V), value(V).
bigstep(T, V) :- eval(T, T1), bigstep(T1, V).


% TYPES
typing(_, true, boolT).
typing(_, false, boolT).
typing(_, 0, natT).
typing(Ctxt, pred(T), natT) :- typing(Ctxt, T, natT).
typing(Ctxt, succ(T), natT) :- typing(Ctxt, T, natT).
typing(Ctxt, iszero(T), boolT) :- typing(Ctxt, T, natT).
typing(Ctxt, if(T1, T2, T3), T) :- typing(Ctxt, T1, boolT),
                                   typing(Ctxt, T2, T),
                                   typing(Ctxt, T3, T).


% Functions
typing(Ctxt, lambda(X, Type, Term),
       funT(Type, Type2)) :- append([[X, Type]], Ctxt, Ctxt1), % I add the type of input to the context
                            typing(Ctxt1, Term, Type2). % and I can type the body with this new context
% Function application
typing(Ctxt, app(T1, T2), T12) :- typing(Ctxt, T1, funT(T11, T12)),
                                  typing(Ctxt, T2, T11). % type of argument match


% Pairs
typing(Ctxt, pair(T1, T2), pairT(TT1, TT2)) :- typing(Ctxt, T1, TT1),
                                               typing(Ctxt, T2, TT2).

typing(Ctxt, fst(T), T1) :- typing(Ctxt, T, pairT(T1, _)).

typing(Ctxt, snd(T), T1) :- typing(Ctxt, T, pairT(_, T1)).

% Variables
typing(Ctxt, X, T) :- member([X, T], Ctxt), variable(X), type(T).

:- begin_tests(typing).
test(type_fst) :- typing([], fst(pair(false, x)), boolT).
test(type_snt) :- typing([[x, natT]], snd(pair(false, x)), natT).

:- end_tests(typing).

% TESTS
% eval(app(lambda("x", _, 0), succ(0)), R). R = 0;
% typing([], app(lambda("x", boolT, "x"), true), T).
% eval(pair(true, iszero(0)), T).
% eval(pair(iszero(0), iszero(succ(0))), T).
% typing(L, snd(pair(false, "x")), natT).
