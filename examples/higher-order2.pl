:-['../metagol'].

%% background knowledge
my_double(A,B):-integer(A),B is A*2.
my_succ(A,B):-integer(A),(ground(B)->integer(B);true),succ(A,B).
my_length(A,B):-A=[_|_],length(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).

%% tell metagol to use the BK
prim(my_succ/2).
prim(my_double/2).
prim(my_length/2).
interpreted(map/3).

%% metarules
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

a:-
  A=[[a],[a,a],[a,a,a],[a,a,a,a]],
  B=[2,4,6,8],
  learn([f(A,B)],[],Prog),
  pprint(Prog).