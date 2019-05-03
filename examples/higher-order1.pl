:-['../metagol'].

empty([]).
head([H|_],H).
tail([_|T],T).

%% background knowledge
my_succ(A,B):-
  integer(A),
  succ(A,B).

%% metarules
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,F], [P,A,B], [[Q,A,B,F]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

%% allow metagol to use my_succ in the body of a clause
body_pred(my_succ/2).

%% interpreted BK
ibk([map,[],[],_],[]).
ibk([map,[A|As],[B|Bs],F],[[F,A,B],[map,As,Bs,F]]).

a:-
    Pos = [
        f([1,2,3],[3,4,5]),
        f([10,12,33,3,2,1],[12,14,35,5,4,3])
    ],
    learn(Pos,[]).
:-
    time(a).