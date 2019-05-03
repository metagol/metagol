:- use_module('../metagol').

%% background knowledge
my_double(A,B):-
    integer(A),
    (ground(B)->integer(B);true),
    B is A*2.
my_succ(A,B):-
    integer(A),
    (ground(B)->integer(B);true),
    succ(A,B).
my_length(A,B):-
    is_list(A),
    (ground(B)->integer(B);true),
    length(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).

%% allow metagol to use my_succ in the body of a clause
body_pred(my_succ/2).
body_pred(my_double/2).
body_pred(my_length/2).

%% metarules
metarule([P,Q,F], [P,A,B], [[Q,A,B,F]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

%% interpreted BK
ibk([map,[],[],_],[]).
ibk([map,[A|As],[B|Bs],F],[[F,A,B],[map,As,Bs,F]]).

a:-
  A=[[a],[a,a],[a,a,a],[a,a,a,a]],
  B=[3,5,7,9],
  learn([f(A,B)],[]).

:-
    time(a).