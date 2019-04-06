:- use_module('../metagol').

%% tell Metagol to use the BK
prim(father/2).
prim(mother/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
mother(i,a).
mother(c,f).
mother(c,g).
mother(f,h).
father(a,b).
father(a,c).
father(b,d).
father(b,e).

a :-
 Pos = [
    target(i,b),
    target(i,c),
    target(a,d),
    target(a,e),
    target(a,f),
    target(a,g),
    target(c,h)
  ],
  Neg = [
    target(a,b),
    target(b,c),
    target(c,d),
    target(d,e),
    target(e,f),
    target(f,g),
    target(g,h),
    target(h,i)
  ],
  learn(Pos,Neg).