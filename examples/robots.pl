:- use_module('../metagol').
%% Examples taken from these papers:
%% A. Cropper and S.H. Muggleton. Can predicate invention compensate for incomplete background knowledge? SCAI 2015.
%% A. Cropper and S.H. Muggleton. Logical minimisation of meta-rules within meta-interpretive learning. ILP 2014.

%% metagol settings
metagol:functional.
metagol:max_clauses(6).

%% tell metagol to use the BK
body_pred(move_left/2).
body_pred(move_right/2).
body_pred(move_forwards/2).
body_pred(move_backwards/2).
body_pred(grab_ball/2).
body_pred(drop_ball/2).

%% metarules
metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
metarule(precon, [P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
metarule(postcon, [P,Q,R], [P,A,B], [[Q,A,B],[R,B]]).
metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

%% functional check
func_test(Atom1,Atom2,Condition):-
  Atom1 = [P,A,B],
  Atom2 = [P,A,Z],
  Condition = (Z = B).

%% robot learning to move a ball to a specific position
a :-
  Pos = [f(world((1/1),(1/1),false),world((3/3),(3/3),false))],
  learn(Pos,[]).

b :-
  Pos = [f(world((1/1),(1/1),false),world((5/5),(5/5),false))],
  learn(Pos,[]).

c :-
  Pos = [f(world((1/1),(1/1),false),world((6/6),(6/6),false))],
  learn(Pos,[]).

d :-
  Pos = [f(world((1/1),(1/1),false),world((7/7),(7/7),false))],
  learn(Pos,[]).

%% background knowledge
max_right(6).
max_forwards(6).

grab_ball(world(Pos,Pos,false),world(Pos,Pos,true)).

drop_ball(world(Pos,Pos,true),world(Pos,Pos,false)).

move_left(world(X1/Y1,Bpos,false),world(X2/Y1,Bpos,false)):-
  X1 > 0,
  X2 is X1-1.

move_left(world(X1/Y1,_,true),world(X2/Y1,X2/Y1,true)):-
  X1 > 0,
  X2 is X1-1.

move_right(world(X1/Y1,Bpos,false),world(X2/Y1,Bpos,false)):-
  max_right(MAXRIGHT),
  X1 < MAXRIGHT,
  X2 is X1+1.

move_right(world(X1/Y1,_,true),world(X2/Y1,X2/Y1,true)):-
  max_right(MAXRIGHT),
  X1 < MAXRIGHT,
  X2 is X1+1.

move_backwards(world(X1/Y1,Bpos,false),world(X1/Y2,Bpos,false)):-
  Y1 > 0,
  Y2 is Y1-1.

move_backwards(world(X1/Y1,_,true),world(X1/Y2,X1/Y2,true)):-
  Y1 > 0,
  Y2 is Y1-1.

move_forwards(world(X1/Y1,Bpos,false),world(X1/Y2,Bpos,false)):-
  max_forwards(MAXFORWARDS),
  Y1 < MAXFORWARDS,
  Y2 is Y1+1.

move_forwards(world(X1/Y1,_,true),world(X1/Y2,X1/Y2,true)):-
  max_forwards(MAXFORWARDS),
  Y1 < MAXFORWARDS,
  Y2 is Y1+1.