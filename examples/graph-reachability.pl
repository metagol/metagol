:- use_module('../metagol').

%% metagol settings
metagol:max_clauses(2).

%% tell metagol to use the BK
body_pred(edge/2).

%% metarules
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q], [P,A,B], [[Q,A,C], [P,C,B]]).

%% background knowledge
edge(a, b).
edge(b, c).
edge(c, a).
edge(a, d).



:-
  Pos = [p(a, b), p(a, c), p(a, a)],
  learn(Pos,[]).