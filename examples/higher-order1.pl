:-['../metagol'].

%% background knowledge
my_succ(A,B):-
  integer(A),
  succ(A,B).

%% metarules
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])):-
    prim(F/2).

%% tell metagol to use the compiled BK
prim(my_succ/2).

%% interpreted BK
interpreted_bk([map,[],[],_],[]).
interpreted_bk([map,[A|As],[B|Bs],F],[[F,A,B],[map,As,Bs,F]]).

a:-
  learn([f([1,2,3],[2,3,4])],[],Prog),
  pprint(Prog).