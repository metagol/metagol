:-['../metagol'].

metarule([P,Q],([P,A]:-[[Q,A]])).
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

prim(divisible2/1).
prim(divisible5/1).

divisible5(X):-0 is X mod 5.
divisible2(X):-0 is X mod 2.

background(([filter,[],[],_F]:- [])).
background(([filter,[A|As],[A|Bs],F]:- [[F,A],[filter,As,Bs,F]])).
background(([filter,[_|As],[B|Bs],F]:- [[filter,As,[B|Bs],F]])). % A\=B. needed?


a:-
  A = [1,2,3,4,5,6,7,8,9,10],
  B = [2,4,5,6,8,10],
  learn([f(A,B)],[],G),
  writeln('---'),
  pprint(G).

b:-
  A = [1,2,3,4,5,6,7,8,9,10],
  B = [2,4,5,6,8,10],
  learn([f(A,B),f(A,B)],[],G),
  writeln('---'),
  pprint(G).



%% f1(A):-divisible2(A).
%% f1(A):-divisible5(A).

%% %% f(A,B):- filter(f1,A,B).
%% %% f1(A):- divisible2(A).
%% %% f1(A):- divisible5(A).


%% %% f(A,B):- filter(f1,A,B).
%% %% f1(A):- divisible2(A).
%% %% f1(A):- divisible5(A).