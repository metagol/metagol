:- use_module('../metagol').

%% tell metagol to use the BK
body_pred(copy1/2).
body_pred(skip1/2).

%% metarules
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
metarule([P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).

%% background knowledge
copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
skip1([_|RestIn]/Out,RestIn/Out).

a :-
  Pos = [
   f(['a','b','c']/['a','a','b','b','c','c'],_/[]),
   f(['a','a','c']/['a','a','a','a','c','c'],_/[]),
   f(['a','c']/['a','a','c','c'],_/[])
  ],
  learn(Pos,[]).

:- time(a).