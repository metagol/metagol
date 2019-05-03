:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(succ/2).

%% metarules
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(inverse, [P,Q], [P,A,B], [[Q,B,A]]).
metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

:-
  Pos = [
    target(1,0),
    target(2,1),
    target(3,2),
    target(4,3),
    target(5,4),
    target(6,5),
    target(7,6),
    target(8,7),
    target(9,8),
    target(10,9)
  ],
  Neg = [
    target(1,3),
    target(1,7),
    target(2,2),
    target(2,8),
    target(3,1),
    target(3,9),
    target(4,0),
    target(4,10),
    target(5,5),
    target(5,6)
  ],
  learn(Pos,Neg).