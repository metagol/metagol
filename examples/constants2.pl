:- use_module('../metagol').

%% tell metagol to use the BK
body_pred(num/1).

%% metarules
metarule([P,Q,A], [P,A,B], [[Q,A],[Q,B]]).
metarule([P,Q,B], [P,A,B], [[Q,A],[Q,B]]).

%% background knowledge
num(X):-
  between(0,inf,X).


:-
  Pos = [
   q(1,2),
   q(1,3),
   q(1,4),
   q(1,1),
   q(2,2),
   q(4,4)
  ],
  Neg = [
   q(2,4),
   q(3,4),
   q(3,1)
  ],
  learn(Pos,Neg).