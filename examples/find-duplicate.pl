:- use_module('../metagol').

metagol:functional.
metagol:max_clauses(4).

metarule(dident,[P,Q,R],([P,A,B]:-[[Q,A,B],[R,A,B]])):-freeze(Q,freeze(R,Q\=R)).
metarule(chain,[P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

prim(mergesort/2).
prim(tail/2).
prim(head/2).
prim(element/2).

mergesort([H|T],B):-
    A=[H|T],
    msort(A,B),
    A\=B.

head([H|_],H).
tail([_|T],T).
element([X|_T],X).
element([_|T],X):-element(T,X).

%% background knowledge

a :-
    Pos = [
        f([1,3,3,4,2,5],3),
        f([6,4,2,5,3,5,1],5),
        f([7,3,4,2,1,5,6,7,8],7),
        f([6,5,7,8,4,2,1,3,7],7),
        f([14,4,13,6,12,1,9,2,10,8,15,5,7,14,3,11],14)
    ],
    learn(Pos,[],Prog),
    pprint(Prog).

func_test(Atom,PS,G):-
  Atom = [P,A,B],
  Actual = [P,A,Z],
  \+ (metagol :prove_deduce([Actual],PS,G),Z \= B).