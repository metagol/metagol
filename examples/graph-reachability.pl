:- use_module('../metagol').

metagol:max_clauses(2).

%% background knowledge
edge(a, b).
edge(b, c).
edge(c, a).
edge(a, d).

prim(edge/2).

metarule([P, Q], ([P, A, B]:-[[Q, A, B]])).
metarule([P, Q], ([P, A, B]:-[[Q, A, C], [P, C, B]])).

a :-
  Pos = [p(a, b), p(a, c), p(a, a)],
  Neg = [],
  learn(Pos,Neg,Prog),
  pprint(Prog).