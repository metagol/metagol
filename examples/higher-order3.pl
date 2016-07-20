:-['../metagol'].

metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

prim(my_succ/2).
prim(my_double/2).
prim(my_length/2).

my_double(A,B):-integer(A),B is A*2.
my_succ(A,B):-
  integer(A),
  (ground(B)->integer(B);true),
  succ(A,B).

my_length(A,B):-A=[_|_],length(A,B).

background(([map,[],[],_F]:- [])).
background(([map,[A|As],[B|Bs],F]:- [[F,A,B],[map,As,Bs,F]])).


a:-
  A=[[a],[a,a],[a,a,a],[a,a,a,a]],
  B=[2,4,6,8],
  learn([f(A,B)],[],G),
  writeln('---'),
  pprint(G).

b:-
  A=[[a],[a,a],[a,a,a],[a,a,a,a]],
  B=[2,4,6,8],
  learn([f(A,B),f(A,B)],[],G),
  writeln('---'),
  pprint(G).