:- use_module('../metagol').

metagol:max_clauses(6).

%% preds that metagol can use in the body of a clause
body_pred(mother/2).
body_pred(father/2).
body_pred(shoe/1).

head_pred(boo/1).
head_pred(boo/1).

%% metarules
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

%% background knowledge
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).


%% learn grandparent by inventing parent
a:-
  Pos = [
    grandparent(ann,amelia),
    grandparent(steve,amelia),
    grandparent(ann,spongebob),
    grandparent(steve,spongebob),
    grandparent(linda,amelia)
  ],
  Neg = [grandparent(amy,amelia)],
  learn(Pos,Neg).

%% example of a failure
b :-
  Pos = [grandparent(ann,amelia)],
  Neg = [grandparent(ann,amelia)],
  (learn(Pos,Neg) -> false; writeln('failed to learn a theory')).

%% :-
  %% time(a).