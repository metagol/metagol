:- use_module('../metagol').

%% tell Metagol to use the BK
body_pred(edge/2).
body_pred(colour/2).
body_pred(red/1).
body_pred(green/1).

%% metarules
metarule([P,Q,R], [P,A], [[Q,A,B],[R,B]]).

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

a:-
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

%% body_pred(mother/2).
%% body_pred(father/2).

%% body_pred(shoe/1).

%% %% metarules
%% metarule([P,Q], [P,A,B], [[Q,A,B]]).
%% metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).