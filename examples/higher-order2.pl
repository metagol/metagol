:-['../metagol'].

prim(reflector/1).
prim(observer/1).
prim(unobstructed_path/2).
prim(light_path/2).


metagol:max_clauses(1).

metarule([P,A],([P,A]:-[])).

a:-
  learn([bright(r,observer)],[],G),
  pprint(G).

b:-
  learn([bright(r,observer),bright(r,observer)],[],G),
  pprint(G).

%% light_source(light).
observer(observer).
reflector(r).

unobstructed(light,r).
%% unobstructed(light,observer).
unobstructed(r,observer).

% Bright R observed by O in image
background(([bright,R,O] :- [[light_source,L], [light_path,L,R],
  [reflector,R], [light_path,R,O], [observer,O]])).

bright(R,O) :- light_source(L), light_path(L,R), reflector(R),
  light_path(R,O), observer(O).

light_path(X,X).
light_path(X,Y) :- unobstructed(X,Z), light_path(Z,Y).
