:- module(metagol,[learn/4,learn_seq/2,pprint/1,member/2]).

:- user:call(op(950,fx,'@')).

:- use_module(library(lists)).

:- dynamic(functional/0).
:- dynamic(min_clauses/1).
:- dynamic(max_clauses/1).

default(min_clauses(1)).
default(max_clauses(6)).

get_option(X):-call(X),!.
get_option(X):-default(X).

learn(Name,Pos,Neg,G):-
  learn(Name,Pos,Neg,[],_PS,[],G).

learn(Name,Pos1,Neg1,PS1,PS2,G1,G2):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  proveall(Name,Pos2,PS1,PS2,G1,G2),
  nproveall(Name,Neg2,PS2,G2),
  (functional -> check_functions(Pos2,PS2,G2); true).

learn_seq(Seq,G):-
  learn_seq_aux(Seq,[],_PS2,[],G).

learn_seq_aux([],PS,PS,G,G).
learn_seq_aux([(Name,Pos,Neg)|T],PS1,PS2,G1,G2):-
  learn(Name,Pos,Neg,PS1,PS3,G1,G3),!, % purposely never backtrack
  learn_seq_aux(T,PS3,PS2,G3,G2).

proveall(Name,Atoms,PS1,PS2,G1,G2):-
  iterator(N,M),
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  init_sig(Name,M,PS1,PS2),
  prove(Atoms,PS2,N,G1,G2).

prove([],_,_,G,G).

prove(['@'(Atom)|Atoms],PS,MaxN,G1,G2):-
  !,
  user:call(Atom),
  prove(Atoms,PS,MaxN,G1,G2).

%% prim
prove([[P|Args]|Atoms],PS,MaxN,G1,G2):-
  user:primtest(P,Args),!,
  user:primcall(P,Args),
  prove(Atoms,PS,MaxN,G1,G2).

%% use existing
prove([Atom|Atoms],PS1,MaxN,G1,G2):-
  Atom=[P|_],
  member(sub(Name,P,MetaSub),G1),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,PS1,MaxN,G1,G3),
  prove(Atoms,PS1,MaxN,G3,G2).

%% use new
prove([Atom|Atoms],PS1,MaxN,G1,G2):-
  length(G1,L),
  L < MaxN,
  lower_sig(Atom,P,PS1,PS2),
  user:metarule(Name,MetaSub,(Atom :- Body),PS2),
  not(memberchk(sub(Name,P,MetaSub),G1)),
  prove(Body,PS1,MaxN,[sub(Name,P,MetaSub)|G1],G3),
  prove(Atoms,PS1,MaxN,G3,G2).

inv_preds(0,_Name,[]):-!.

inv_preds(M,Name,[Sk/_|PS]) :-
  atomic_list_concat([Name,'_',M],Sk),
  succ(Prev,M),
  inv_preds(Prev,Name,PS).

init_sig(Name,M,[],[Name/_|PS]):-!,
  inv_preds(M,Name,InvPreds),
  findall(Prim, user:prim(Prim), Prims),
  append(InvPreds,Prims,PS).

init_sig(Name,M,PS1,[Name/_|PS2]):-
  inv_preds(M,Name,InvPreds),
  append(InvPreds,PS1,PS2).

lower_sig([P|Args],P,PS1,PS2):-
  length(Args,A),
  append(_,[P/A|PS2],PS1),!.

nproveall(_Name,[],_PS,_G).

nproveall(Name,[Atom|T],PS,G):-
  length(G,N),
  not(prove([Atom],PS,N,G,G)),
  nproveall(Name,T,PS,G).

iterator(N,M):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),!,
  between(MinN,MaxN,N),
  succ(MaxM,N),
  between(0,MaxM,M).

pprint([]).

pprint([sub(Name,_P,MetaSub)|T]):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  listtocomma(AtomBodyList,Body),
  numbervars((Head:-Body),0,_),
  format('~q.~n', [(Head:-Body)]),
  pprint(T).

listtocomma([E],E):-!.

listtocomma([H|T],(H,R)):-
  listtocomma(T,R).

convert_preds([],[]).

convert_preds(['@'(Atom)|T],[Atom|R]):-
  !,
  convert_preds(T,R).

convert_preds([List|T],[Atom|R]):-
  !,
  Atom=..List,
  convert_preds(T,R).

atom_to_list([],[]).

atom_to_list([Atom|T],[AtomAsList|Out]):-
  Atom =..AtomAsList,
  atom_to_list(T,Out).

check_functions([],_PS,_G).

check_functions([Atom|Atoms],PS,G) :-
  check_function(Atom,PS,G),
  check_functions(Atoms,PS,G).

check_function([Head|Args],PS,G):-
  length(G,N),
  append(FuncArgs,[OrigReturn],Args),
  append(FuncArgs,[TestReturn],TestArgs),
  not((prove([[Head|TestArgs]],PS,N,G,G),TestReturn \= OrigReturn)).

:- user:discontiguous(prim/1).
:- user:discontiguous(primcall/2).
:- user:discontiguous(primtest/2).

user:term_expansion(prim(P/A),[prim(P/A),primtest(P,Args),(primcall(P,Args):-Call)]):-
    functor(Call,P,A),
    Call=..[P|Args].

:- user:discontiguous(metarule/4).
:- user:discontiguous(metarule_init/3).

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  Asserts = [
             (metarule(Name,MetaSub,Clause,PS):-Body),
             (metarule_init(Name,MetaSub,Clause))
            ].
