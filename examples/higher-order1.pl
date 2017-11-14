:-['../metagol'].

%% tell metagol to use the BK
interpreted(map/3).

%% metarules
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
my_succ(A,B):-
  integer(A),
  succ(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).

a:-
  learn([f([1,2,3],[2,3,4])],[]).