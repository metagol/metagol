:- use_module('../metagol').

%% FIRST-ORDER BACKGROUND KNOWLEDGE
parent(elizabeth_second,prince_charles).
parent(prince_philip,prince_charles).
parent(prince_charles,prince_william).
parent(prince_charles,prince_harry).
parent(princess_diana,prince_william).
parent(princess_diana,prince_harry).
parent(prince_william,prince_george).

%% PREDICATES TO BE USED IN THE LEARNING
prim(parent/2).

%% METARULES
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% LEARNING
a :-
  Pos = [
    great_grandparent(elizabeth_second,prince_george),
    great_grandparent(prince_philip,prince_george)
    ],
  Neg = [
    great_grandparent(prince_charles,prince_william)
  ],
  learn(great_grandparent,Pos,Neg,H),
  pprint(H).