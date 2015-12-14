:- use_module('../metagol').

metarule([P,A],([P,A,_B]:-[])).
metarule([P,B],([P,_A,B]:-[])).

a :-
  Pos = [
    p(1,2),
    p(1,3),
    p(1,4),
    p(1,1),
    p(2,2),
    p(4,4)
    ],
  Neg = [
    p(2,4),
    p(3,4),
    p(3,1)
  ],
  learn(Pos,Neg,H),
  pprint(H).
