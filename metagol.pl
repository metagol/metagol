:- module(metagol,[learn/4,pprint/1,member/2]).

:- use_module(library(lists)).

:- dynamic(functional/0).
:- dynamic(min_clauses/1).
:- dynamic(max_clauses/1).

default(min_clauses(1)).
default(max_clauses(6)).

get_option(X):-call(X),!.
get_option(X):-default(X).

learn(Name,Pos1,Neg1,G):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  proveall(Name,Pos2,PS,G),
  nproveall(Neg2,PS,G).
  %% (functional -> functional(Pos,PS,G); true).

proveall(Name,Atoms,PS,G):-
  iterator(N,M),
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  init_sig(Name,M,PS),
  prove(Atoms,PS,N,[],G).

prove([],_,_,G,G).

prove(['@'(OrderTest)|Atoms],PS,MaxN,G1,G2):-!,
  user:OrderTest,
  prove(Atoms,PS,MaxN,G1,G2).

%% prim
prove([Atom|Atoms],PS,MaxN,G1,G2):-
  prim_atom(Atom),
  Goal =..Atom,!,
  user:Goal,
  prove(Atoms,PS,MaxN,G1,G2).

%% use existing
prove([Atom|Atoms],PS1,MaxN,G1,G2):-
  member(sub(Name,MetaSub),G1),
  once(user:metarule(Name,MetaSub,(Atom :- Body),_)),
  prove(Body,PS1,MaxN,G1,G3),
  prove(Atoms,PS1,MaxN,G3,G2).

%% use new
prove([Atom|Atoms],PS1,MaxN,G1,G2):-
  length(G1,L),
  L < MaxN,
  slice_sig(Atom,PS1,PS2),!,
  user:metarule(Name,MetaSub,(Atom :- Body),PS2),
  not(memberchk(sub(Name,MetaSub),G1)),
  prove(Body,PS1,MaxN,[sub(Name,MetaSub)|G1],G3),
  prove(Atoms,PS1,MaxN,G3,G2).

prim_atom(Atom):-
  Atom=[P|Args],
  length(Args,A),
  user:prim(P/A).

inv_preds(0,_Name,[]) :- !.
inv_preds(M,Name,[Sk/_|PS]) :-
  atomic_list_concat([Name,'_',M],Sk),
  succ(Prev,M),
  inv_preds(Prev,Name,PS).

init_sig(Name,M,[Name/_|PS]):-
  inv_preds(M,Name,InvPreds),
  findall(Prim, user:prim(Prim), Prims),
  append(InvPreds,Prims,PS).

slice_sig([P|Args],PS1,PS2):-
  length(Args,A),
  append(_,[P/A|PS2],PS1),!.

nproveall([],_PS,_G).
nproveall([Atom|T],PS,G):-
  length(G,N),
  not(prove([Atom],PS,N,G,G)),
  nproveall(T,PS,G).

iterator(N,M):-
  get_option(min_clauses(MIN)),
  get_option(max_clauses(MAX)),!,
  between(MIN,MAX,N),
  succ(MaxM,N),
  between(0,MaxM,M).

func_test([],_,_).
func_test([Atom|Atoms],PS,G) :-
  do_func_test(Atom,PS,G),
  func_test(Atoms,PS,G).

pprint([]).
pprint([sub(Name,MetaSub)|T]):-
  user:metarule(Name,MetaSub,Clause,_),
  copy_term(Clause,X),
  numbervars(X,0,_),
  format('~q.~n', [X]),
  pprint(T).

atom_to_list([],[]).
atom_to_list([Atom|T],[AtomAsList|Out]):-
  Atom =..AtomAsList,
  atom_to_list(T,Out).

%% expand metarules?
%% user:term_expansion(metarule(Name,Subs,(Head:-Body)),
%%   (
%%     metarule(Name,Subs,(Head:-Body),PS) :-
%%       Head = [P|_],
%%       selectchk(P,Subs,ToBind),
%%       metagol:bind_metasubs(ToBind,PS)
%%   )).

%% bind_metasubs([],_).
%% bind_metasubs([P|T],PS):-
%%   member(P/_,PS),
%%   bind_metasubs(T,PS).



%% isfunctions([Atom|Atoms]) :-
%%   isfunction(Atom),
%%   isfunctions(Atoms).

%% isfunction(Atom):-
%%   Atom=..[Head|Args],
%%   append(FuncArgs,[OrigReturn],Args),
%%   append(FuncArgs,[TestReturn],TestArgs),
%%   TestAtom=..[Head|TestArgs],!,
%%   not((call(user:TestAtom),TestReturn \= OrigReturn)).
