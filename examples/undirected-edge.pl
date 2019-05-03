:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(edge/2).

%% metarules
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q], [P,A,B], [[Q,B,A]]).

%% background knowledge
edge(a,b).
edge(b,d).
edge(c,c).

:-
 Pos = [
    target(a,b),
    target(b,a),
    target(b,d),
    target(d,b),
    target(c,c)
  ],
  Neg = [
  ],
  learn(Pos,Neg).