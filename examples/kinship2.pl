:- use_module('../metagol').

%% tell metagol to use the BK
prim(parent/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

%% background knowledge
parent(queen_victoria,edward_seventh).
parent(edward_seventh,george_fifth).
parent(george_fifth,george_sixth).
parent(george_sixth,elizabeth_second).
parent(queen_mother,elizabeth_second).
parent(elizabeth_second,prince_charles).
parent(prince_philip,prince_charles).
parent(prince_charles,prince_william).
parent(prince_charles,prince_harry).
parent(princess_diana,prince_william).
parent(princess_diana,prince_harry).
parent(prince_william,prince_george).

a :-
  Pos = [
    ancestor(elizabeth_second,prince_charles),
    ancestor(george_sixth,prince_harry),
    ancestor(queen_mother,prince_william)
  ],
  learn(Pos,[]).