:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(edge/2).
body_pred(colour/2).

%% metarules
metarule([P,Q,R], [P,A], [[Q,A,B],[R,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,B,C]]).

%% background knowledge
edge(a,b).
edge(b,c).
edge(b,d).
edge(c,e).
edge(e,f).
colour(a,green).
colour(b,red).
colour(c,green).
colour(d,green).
colour(e,red).
colour(f,red).

:-
 Pos = [
    target(e)
  ],
  Neg = [
    target(a),
    target(b),
    target(c),
    target(d),
    target(f)
  ],
  learn(Pos,Neg).