:- use_module('../metagol').

%% metagol settings
metagol:unfold_program.

%% tell Metagol to use the BK
prim(succ/2).

%% metarules
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

:-
  Pos = [
    target(1,3),
    target(2,5),
    target(3,7),
    target(4,10),
    target(5,9),
    target(6,8),
    target(7,9),
    target(8,10),
    target(9,10)
  ],
  Neg = [
    target(3,1),
    target(7,1),
    target(2,2),
    target(8,2),
    target(4,3),
    target(9,3),
    target(4,0),
    target(10,4),
    target(5,5),
    target(6,5)
  ],
  learn(Pos,Neg).