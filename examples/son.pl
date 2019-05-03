:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(father/2).
body_pred(brother/2).
body_pred(sister/2).

%% metarules
metarule([P,Q,R], [P,A,B], [[Q,B,A],[R,A]]).
metarule([P,Q,R], [P,A], [[Q,A,B]]).

%% background knowledge
father(a,b).
father(a,c).
father(d,e).
father(d,f).
father(g,h).
father(g,i).
brother(b,c).
brother(c,b).
brother(e,f).
sister(f,c).
sister(h,i).
sister(i,h).

:-
 Pos = [
    target(b,a),
    target(c,a),
    target(e,d)
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