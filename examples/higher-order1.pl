:-['../metagol'].

%% background knowledge
my_succ(A,B):-
  integer(A),
  succ(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).

%% metarules
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% tell metagol to use the BK
prim(my_succ/2).
interpreted(map/3).

a:-
  learn([f([1,2,3],[2,3,4])],[],Prog),
  pprint(Prog).