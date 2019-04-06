:- use_module('../metagol').

%% tell Metagol to use the BK
prim(edge/2).
prim(colour/2).
prim(red/1).
prim(green/1).

%% metarules
metarule([P,Q,R],([P,A]:-[[Q,A,B],[R,B]])).

%% background knowledge
edge(a,b). 
edge(b,a). 
edge(c,d). 
edge(c,e). 
edge(d,e).
colour(a,red).
colour(b,green). 
colour(c,red).
colour(d,red). 
colour(e,green).
red(red).
green(green).

a :-
 Pos = [
    target(b),
    target(c)
  ],
  Neg = [
    target(a),
    target(d),
    target(e)
  ],
  learn(Pos,Neg).