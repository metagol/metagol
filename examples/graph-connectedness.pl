:- use_module('../metagol').

%% tell Metagol to use the BK
prim(edge/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
edge(a,b). 
edge(b,c). 
edge(c,d). 
edge(b,a). 

a :-
 Pos = [
    target(a,b), 
    target(b,c), 
    target(c,d), 
    target(b,a), 
    target(a,c),
    target(a,d), 
    target(a,a), 
    target(b,d), 
    target(b,a), 
    target(b,b)
  ],
  Neg = [
  ],
  learn(Pos,Neg).