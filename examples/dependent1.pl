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
married(ann,steve).
married(steve,ann).
married(amy,gavin).
married(gavin,amy).
offspring(A,B):- mother(B,A).
offspring(A,B):- father(B,A).

%% PREDICATES TO BE USED IN THE LEARNING
prim(offspring/2).
prim(married/2).
prim(mother/2).
prim(father/2).

%% METARULES
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% SEQUENTIAL LEARNING
a :-
  Seq = [
    ([parent(ann,andy),parent(steve,andy),parent(ann,amy)],[]),
    ([grandparent(ann,amelia),grandparent(steve,amelia)],[])
  ],
  learn_seq(Seq,H),
  pprint(H).