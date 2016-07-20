:-['../metagol'].

prim(light_source/1).
prim(observer/1).
prim(convex/1).
prim(reflector/1).
prim(unobstructed/2).
prim(light_path/2).
prim(contains/2).
prim(brighter/2).
% prim(clock_angle1/3).   % Abducible

% metagol:functional.
metagol:max_clauses(10).

%% target(bright). % name of target predicate
%% max_time(600000). % 10 min timeout

metarule([P,A],([P,A]:-[])).
%% metarule_init(1,[P,A],([P,A]:-[])).
metarule([P,A,B],([P,A,B]:-[])).
%% metarule_init(2,[P,A,B],([P,A,B]:-[])).
metarule([P,A,B,C],([P,A,B,C]:-[])).
%% metarule_init(3,[P,A,B,C],([P,A,B,C]:-[])).

%% metarule([A, B], ([A, B]:-[])).

%% user:metarule(Name,MetaSub,(Atom :- Body),Sig2),
%% metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
% metarule(chain,[P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
% metarule(tailrec,[P,Q,R],([P,A,B]:-[[Q,A,C],@term_gt(A,C),[P,C,B]])).
% metarule(parallel,[P,Q,R],([P,A,B]:-[[Q,A,B],[R,A,B]])).
% metarule(project,[P,Q,R],([P,A,A]:-[[Q,A]])).
% metarule(euclid,[P,Q,R],([P,A]:-[[Q,A,B],[R,A,B]])).
% metarule(independent,[P,Q,R],([P,A,B]:-[[Q,A],[R,B]])).

a:-
  learn([clock_angle(obj1,obj2,11)],[],G),
  pprint(G).

light_source(light).
observer(observer).
reflector(obj2).
convex(obj1).
% clock_angle1(obj1,light,11).

contains(obj1,obj2).
brighter(obj2,obj1).

%% unobstructed(light,obj2).
%% unobstructed(light,observer).
unobstructed(obj2,observer).

% Bright R observed by O in image

interpreted(clock_angle/3).
interpreted(highlight/2).

%% background(([map,[],[],_F]:- [])).
background(([clock_angle,O,H,A] :- [[highlight,O,H], [convex,O],
  [light_source,L], [clock_angle1,O,L,A]])).
background(([highlight,X,Y] :- [[contains,X,Y], [brighter,Y,X],
  [light_source,L], [light_path,L,R], [reflector,R],
  [light_path,R,O], [observer,O]])).

% clock_angle(O,H,A) :- highlight(O,H), convex(O), light_source(L),
% clock_angle(O,L,A).

% highlight(X,Y) :- contains(X,Y), brighter(Y,X),
% light_source(L), light_path(L,R), reflector(R),
%   light_path(R,O), observer(O).

light_path(X,X).
light_path(X,Y) :- unobstructed(X,Z), light_path(Z,Y).
