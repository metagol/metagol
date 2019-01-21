:-['../metagol'].

%% metarules
metarule([P,Q],([P,A]:-[[Q,A]])).
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% background knowledge
divisible5(X):-0 is X mod 5.
divisible2(X):-0 is X mod 2.

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).

%% tell metagol to use the BK
prim(divisible2/1).
prim(divisible5/1).

interpreted_bk([filter,[],[],_],[]).
interpreted_bk([filter,[A|T1],[A|T2],F],[[F,A],[filter,T1,T2,F]]).
interpreted_bk([filter,[_|T1],T2,F],[[filter,T1,T2,F]]).

a:-
  A = [1,2,3,4,5,6,7,8,9,10],
  B = [2,4,5,6,8,10],
  learn([f(A,B)],[]).