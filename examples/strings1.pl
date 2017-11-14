:- use_module('../metagol').

%% tell metagol to use the BK
prim(copy1/2).
prim(skip1/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
copy1([A|T1]/[A|T2],[A|T1]/T2).
skip1([_|T1]/Out,T1/Out).

a :-
  Pos = [
   f(['a','b','c']/['a','a','b','b','c','c'],_/[]),
   f(['a','a','c']/['a','a','a','a','c','c'],_/[]),
   f(['a','c']/['a','a','c','c'],_/[])
  ],
  learn(Pos,[]).