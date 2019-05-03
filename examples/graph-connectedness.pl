:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(edge/2).

%% metarules
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
metarule(tailrec, [P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).

%% background knowledge
edge(a,b).
edge(b,c).
edge(c,d).
edge(b,a).

:-
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