:- use_module('../metagol').

%% FIRST-ORDER BACKGROUND KNOWLEDGE
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
father(steve,amy).
father(steve,andy).
father(andy,spongebob).

offspring(A,B):- mother(B,A).
offspring(A,B):- father(B,A).

%% PREDICATES TO BE USED IN THE LEARNING
prim(offspring/2).
prim(mother/2).
prim(father/2).

%% METARULES
metarule(identity,[P,Q],([P,A,B]:-[[Q,A,B]]),PS):-
  member(Q/2,PS).

metarule(chain,[P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]),PS):-
  member(Q/2,PS),
  member(R/2,PS).

%% LEARNING
a :-
  Pos = [
    grandparent(ann,amelia),
    grandparent(steve,amelia),
    grandparent(ann,spongebob),
    grandparent(steve,spongebob)
  ],
  Neg = [
    grandparent(amy,amelia)
  ],
  learn(grandparent,Pos,Neg,H),
  pprint(H).