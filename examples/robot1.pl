%% Learning robot sorting algorithms
%% Example taken from the following paper:
%% * A. Cropper and S.H. Muggleton. Logical minimisation of meta-rules within meta-interpretive learning. In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.
%% * A. Cropper and S.H. Muggleton. Can predicate invention compensate for incomplete background knowledge?. In Proceedings of the 13th Scandinavian Conference on Artificial Intelligence, pages 27-36. IOS Press, 2015.

:- use_module('../metagol').

metagol:functional.

%% PREDICATES TO BE USED IN THE LEARNING
prim(move_left/2).
prim(move_right/2).
prim(move_forwards/2).
prim(move_backwards/2).
prim(grab_ball/2).
prim(drop_ball/2).

%% METARULES
metarule(identity,[P,Q],([P,A,B]:-[[Q,A,B]]),PS):-
  member(Q/2,PS).

metarule(chain,[P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]]),PS):-
  member(Q/2,PS),
  member(R/2,PS).

%% LEARNING
a :-
    Pos = [
            f(
            world((1/1),(1/1),false,0),
            world((5/5),(5/5),false,_)
            )
          ],
    learn(f,Pos,[],H),
    pprint(H).

b :-
    Pos = [
            f(
            world((1/1),(1/1),false,0),
            world((3/3),(3/3),false,_)
            )
          ],
    metagolo_linear(f,Pos,[],H),
    pprint(H).

%% FIRST-ORDER BACKGROUND KNOWLEDGE
max_right(6).
max_forwards(6).
energy_bound(100).

grab_ball(world(Pos,Pos,false,E   ),
          world(Pos,Pos,true ,Enew)):-
    increment_energy(E,Enew,1).

drop_ball(world(Pos,Pos,true ,E   ),
          world(Pos,Pos,false,Enew)):-
    increment_energy(E,Enew,1).

move_left(world(X1/Y1,Bpos,false,E   ),
          world(X2/Y1,Bpos,false,Enew)):-
    X1 > 0,
    increment_energy(E,Enew,1),
    X2 is X1-1.

move_left(world(X1/Y1,_    ,true,E   ),
          world(X2/Y1,X2/Y1,true,Enew)):-
    X1 > 0,
    increment_energy(E,Enew,1),
    X2 is X1-1.


move_right(world(X1/Y1,Bpos,false,E   ),
           world(X2/Y1,Bpos,false,Enew)):-
    max_right(MAXRIGHT),
    X1 < MAXRIGHT,
    increment_energy(E,Enew,1),
    X2 is X1+1.

move_right(world(X1/Y1,_    ,true,E   ),
           world(X2/Y1,X2/Y1,true,Enew)):-
    max_right(MAXRIGHT),
    X1 < MAXRIGHT,
    increment_energy(E,Enew,1),
    X2 is X1+1.


move_backwards(world(X1/Y1,Bpos,false,E   ),
               world(X1/Y2,Bpos,false,Enew)):-
    Y1 > 0,
    increment_energy(E,Enew,1),
    Y2 is Y1-1.

move_backwards(world(X1/Y1,_    ,true,E   ),
               world(X1/Y2,X1/Y2,true,Enew)):-
    Y1 > 0,
    increment_energy(E,Enew,1),
    Y2 is Y1-1.


move_forwards(world(X1/Y1,Bpos,false,E   ),
              world(X1/Y2,Bpos,false,Enew)):-
    max_forwards(MAXFORWARDS),
    Y1 < MAXFORWARDS,
    increment_energy(E,Enew,1),
    Y2 is Y1+1.

move_forwards(world(X1/Y1,_    ,true,E   ),
              world(X1/Y2,X1/Y2,true,Enew)):-
    max_forwards(MAXFORWARDS),
    Y1 < MAXFORWARDS,
    increment_energy(E,Enew,1),
    Y2 is Y1+1.

%% ENERGY COSTS
increment_energy(E1,E2,Amount):-
  energy_bound(Bound),
  E2 is E1 + Amount,
  E2 =< Bound.

resource_complexity(world(_,_,_,E),E).