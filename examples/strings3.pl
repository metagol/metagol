:- use_module('../metagol').

%% metagol settings
metagol:functional.

%% tell metagol to use the BK
prim(copy1/2).
prim(skip1/2).
prim(write1/3).
prim(next_empty/1).

%% metarules
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]).
metarule([P,Q,X], [P,A,B], [[Q,A,B,X]]).
metarule([P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).

%% background knowledge
copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
skip1([_|RestIn]/Out,RestIn/Out).
write1(In/[H|RestOut],In/RestOut,H).
next_empty([_]/_).

func_test(Atom1,Atom2,Condition):-
  Atom1 = [P,In/B,_/[]],
  Atom2 = [P,In/Z,_/[]],
  Condition = (Z = B).

:-
  Pos = [
    f(['a','b','c']/['a','b','c','d'],_/[]),
    f(['a','a','c']/['a','a','c','d'],_/[]),
    f(['a','c']/['a','c','d'],_/[])
  ],
  learn(Pos,[]).