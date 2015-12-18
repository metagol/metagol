:- use_module('../metagol').

%% FIRST-ORDER BACKGROUND KNOWLEDGE
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).
father(spongebob,sally).


%% PREDICATES TO BE USED IN THE LEARNING
prim(mother/2).
prim(father/2).

%% METARULES
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% LEARNING PARENT, THEN GRANDPARENT, THEN GREAT-GRANDPARENT
a:-
  T1 = [
    parent(ann,andy),
    parent(steve,andy),
    parent(ann,amy),
    parent(ann,andy)
  ]/[],

  T2 = [
    grandparent(steve,amelia),
    grandparent(ann,amelia),
    grandparent(linda,amelia),
    grandparent(ann,spongebob)
  ]/[],

  T3 = [
    great_grandparent(ann,sally),
    great_grandparent(steve,sally)
  ]/[],

  learn_seq([T1,T2,T3],H),
  pprint(H).
