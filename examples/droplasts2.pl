:- use_module('../metagol').
metagol:functional.

func_test(Atom1,Atom2,Condition):-
  Atom1 = [P,A,B],
  Atom2 = [P,A,Z],
  Condition = (Z = B).

even(X):-
    number(X),
    0 is X mod 2.
odd(X):-
    number(X),
    1 is X mod 2.

my_double(A,B):-
    integer(A),
    B is A*2.
my_succ(A,B):-
    integer(A),
    (ground(B)->integer(B);true),
    succ(A,B).
my_prec(A,B):-
    integer(A),
    (ground(B)->integer(B);true),
    succ(B,A).
my_length(A,B):-
    is_list(A),
    (nonvar(B)->integer(B);true),
    length(A,B).
my_last(A,B):-
    last(A,B).
my_head([H|_],H).
my_tail([_|T],T).
my_reverse(A,B):-
    reverse(A,B).
my_droplast([_],[]).
my_droplast([H|T1],[H|T2]):-
    my_droplast(T1,T2).
my_msort(A,B):-
    (nonvar(A) -> is_list(A)),
    (nonvar(B) -> is_list(B)),
    msort(A,B).

empty([]).
not_empty([_|_]).
body_pred(my_succ/2).
body_pred(my_prec/2).
body_pred(my_double/2).
body_pred(my_length/2).
body_pred(my_last/2).
body_pred(my_head/2).
body_pred(my_tail/2).
body_pred(my_reverse/2).
body_pred(my_msort/2).
body_pred(empty/1).
body_pred(not_empty/1).
body_pred(even/1).
body_pred(odd/1).

%% metarules
metarule([P,Q], [P,A], [[Q,A]]).
metarule([P,Q,R], [P,A], [[Q,A],[R,A]]).
metarule([P,Q], [P,A,B], [[Q,A,B]]).
metarule([P,Q,F], [P,A,B], [[Q,A,B,F]]).
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,B]]).

%% interpreted BK
ibk([map,[],[],_],[]).
ibk([map,[H1|T1],[H2|T2],F],[[F,H1,H2],[map,T1,T2,F]]).

ibk([filter,[],[],_],[]).
ibk([filter,[A|T1],[A|T2],F],[[F,A],[filter,T1,T2,F]]).
ibk([filter,[_|T1],T2,F],[[filter,T1,T2,F]]).

map([],[],_F).
map([A|As],[B|Bs],F):-
    call(F,A,B),
    map(As,Bs,F).

reduceback([Last],Last,_F).
reduceback([A|T],Out,F):-
  reduceback(T,C,F),
  call(F,C,A,Out).

gen_ex(A,B):-
    NumRows = 4,
    findall(SubList,(between(1,NumRows,_),random(2,8,NumColumns),randseq(NumColumns,NumColumns,SubList)),A),
    maplist(my_droplast,A,C),
    my_droplast(C,B).

a :-
  set_random(seed(111)),
  findall(f(A,B),(between(1,5,_),gen_ex(A,B)), Pos),
  learn(Pos,[]).
