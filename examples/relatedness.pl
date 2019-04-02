:- use_module('../metagol').

%% tell Metagol to use the BK
prim(parent/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q],([P,A,B]:-[[Q,B,A]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
parent(a,b).
parent(a,c).
parent(c,e).
parent(c,f).
parent(d,c).
parent(g,h).

a :-
 Pos = [
    target(a,b),
    target(a,c),
    target(a,e),
    target(a,f),
    target(f,a),
    target(a,a),
    target(d,b),
    target(h,g)
  ],
  Neg = [
    target(g,a),
    target(a,h),
    target(e,g),
    target(g,b)
  ],
  learn(Pos,Neg).