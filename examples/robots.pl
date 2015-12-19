%% Learning robot strategies
%% Example taken from the following papers:
%% * A. Cropper and S.H. Muggleton. Learning efficient logical robot strategies involving composable objects. Proceedings of the Twenty-Fourth International Joint Conference on Artificial Intelligence, IJCAI 2015.
%% * A. Cropper and S.H. Muggleton. Logical minimisation of meta-rules within meta-interpretive learning. In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.
%% * A. Cropper and S.H. Muggleton. Can predicate invention compensate for incomplete background knowledge?. In Proceedings of the 13th Scandinavian Conference on Artificial Intelligence, pages 27-36. IOS Press, 2015.

:- use_module('../metagol').

%% METAGOL SETTINGS
metagol:functional. % force functional solution

%% PREDICATES TO BE USED IN THE LEARNING
prim(move_left/2).
prim(move_right/2).
prim(move_forwards/2).
prim(move_backwards/2).
prim(grab_ball/2).
prim(drop_ball/2).

%% METARULES
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% CUSTOM FUNCTIONAL CHECK
func_test(Atom,PS,G):-
  Atom = [P,A,B],
  Actual = [P,A,Z],
  \+ (metagol:prove_deduce([Actual],PS,G),Z \= B).

%% ROBOT LEARNING TO MOVE A BALL TO A SPECIFIC POSITION
a :-
  Pos = [f(world((1/1),(1/1),false),world((3/3),(3/3),false))],
  learn(Pos,[],H),
  pprint(H).

b :-
  Pos = [f(world((1/1),(1/1),false),world((5/5),(5/5),false))],
  learn(Pos,[],H),
  pprint(H).

c :-
  Pos = [f(world((1/1),(1/1),false),world((6/6),(6/6),false))],
  learn(Pos,[],H),
  pprint(H).

%% FIRST-ORDER BACKGROUND KNOWLEDGE
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
