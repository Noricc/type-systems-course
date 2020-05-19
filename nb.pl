% This is an implementation of the NB language.
% In Prolog
% See https://git.cs.lth.se/sde/type-systems-course/-/blob/master/fos2019.github.io/project1.md

% TERMS
const(term).
term(true).
term(false).
% Numeric literals are terms
term(X) :- integer(X).
% Compound terms
term(succ(X)) :- term(X).
term(pred(X)) :- term(X).
term(iszero(X)) :- term(X).
% term(succ(5)) returns true, but term(succ(true)) allows for backtracking and I don't know why.

term(ifthenelse(X, Y, Z)) :- term(X), term(Y), term(Z).

% VALUES
value(true).
value(false).

% Had to add this rule, it was implicit
value(X) :- num_value(X).

% NUMERIC VALUES
num_value(0).
num_value(succ(X)) :- num_value(X).

% SMALL STEP SEMANTICS, The Reduce Relation.
% here "reduce" maps to the "arrow" in small step semantics
% The left hand side is the expression, the right hand side the reduction.
reduce(ifthenelse(true, T1, _), T1).
reduce(ifthenelse(false, _, T2), T2).

% Reduction based on values.
reduce(iszero(0), true).
reduce(iszero(succ(NV)), false) :- num_value(NV).

reduce(pred(0), 0).
% Interestingly, this rule allows for pred(succ(false)) to be evaluated to false.
% You can find this bug by querying reduce(X, false).
% (Try to do that in Scala!)
reduce(pred(succ(X)), X).

% Now we have premisses, which go to the right hand side of the rule.
reduce(ifthenelse(T1, T2, T3), ifthenelse(T11, T2, T3)) :- reduce(T1, T11).

reduce(iszero(T), iszero(T1)) :- reduce(T, T1).

reduce(pred(T), pred(T1)) :- reduce(T, T1).

reduce(succ(T), succ(T1)) :- reduce(T, T1).

% BIG STEP SEMANTICS
% Same as previously, we convert the arrow to "eval"
% and pass the syntactic forms as arguments.
% The premises are on the right side, the consequent on the left side of the :-

% B-VALUE
eval(X, X) :- value(X).
% eval(X, Y) :- X = Y, value(X). % There is an implicit rule here, that we talk about values

% B-IFTRUE
eval(ifthenelse(T1, T2, _), V2) :- eval(T1, true), eval(T2, V2).

% B-IFFALSE
eval(ifthenelse(T1, _, T3), V3) :- eval(T1, false), eval(T3, V3).

% B-SUCC
% There is an implicit rule in the spec, that NV1 is a numeric value
eval(succ(T1), succ(NV1)) :- eval(T1, NV1), num_value(NV1).

% B-PREDZERO
eval(pred(T1), 0) :- eval(T1, 0).

% B-PREDSUCC
eval(pred(T1), NV1) :- eval(T1, succ(NV1)), num_value(NV1).

% B-ISZEROZERO
eval(iszero(T1), true) :- eval(T1, 0).

% B-ISZEROSUCC
eval(iszero(T1), false) :- eval(T1, succ(NV1)), num_value(NV1).


% TESTS
test1 :- eval(pred(succ(succ(succ(false)))), _). % FALSE

test2(X) :- eval(ifthenelse(iszero(pred(pred(succ(succ(0))))),ifthenelse(iszero(0),true, false), false), X).
