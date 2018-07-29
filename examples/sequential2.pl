:- use_module('../metagol').

%% tell metagol to use BK
prim(mother/2).
prim(father/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).
father(spongebob,sally).


%% learn parent, then grandparent, then great-grandparent
:-
  Pos1 = [
    parent(ann,andy),
    parent(steve,andy),
    parent(ann,amy),
    parent(ann,andy)
  ],
  learn(Pos1,[],Prog1),
  pprint(Prog1),
  metagol:assert_prims(Prog1),
  metagol:assert_program(Prog1),

  Pos2 = [
    grandparent(steve,amelia),
    grandparent(ann,amelia),
    grandparent(linda,amelia),
    grandparent(ann,spongebob)
  ],
  learn(Pos2,[],Prog2),
  pprint(Prog2),
  metagol:assert_prims(Prog2),
  metagol:assert_program(Prog2),

  Pos3 = [
    great_grandparent(ann,sally),
    great_grandparent(steve,sally)
  ]/

  learn(Pos3,[],Prog3),
  pprint(Prog3).