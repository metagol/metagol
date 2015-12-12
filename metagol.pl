:- module(metagol,[learn/4,learn/3,learn_seq/2,pprint/1,member/2]).

:- user:call(op(950,fx,'@')).

:- use_module(library(lists)).
:- use_module(library(charsio)).

:- dynamic(functional/0).
:- dynamic(min_clauses/1).
:- dynamic(max_clauses/1).

default(min_clauses(1)).
default(max_clauses(6)).

get_option(X):-call(X),!.
get_option(X):-default(X).

learn(Pos,Neg,G):-
  learn(Pos,Neg,[],_PS,[],G).

learn(_Name,Pos,Neg,G):- % depreciated
  learn(Pos,Neg,[],_PS,[],G).

learn(Pos1,Neg1,PS1,PS2,G1,G2):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  proveall(Pos2,PS1,PS2,G1,G2),
  nproveall(Neg2,PS2,G2),
  (functional -> is_functional(Pos2,PS2,G2); true).

target_name([[P|_]|_],P).

learn_seq(Seq,G):-
  learn_seq_aux(Seq,[],_PS2,[],G).

learn_seq_aux([],PS,PS,G,G).
learn_seq_aux([(Pos,Neg)|T],PS1,PS2,G1,G2):-
  learn(Pos,Neg,PS1,PS3,G1,G3),!, % purposely never backtrack
  learn_seq_aux(T,PS3,PS2,G3,G2).

proveall(Atoms,PS1,PS2,G1,G2):-
  target_name(Atoms,Name),
  iterator(N,M),
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  init_sig(Name,M,PS1,PS2),
  prove(Atoms,PS2,N,G1,G2).

prove([],_,_,G,G).

prove(['@'(Atom)|Atoms],PS,MaxN,G1,G2):- !,
  user:call(Atom),
  prove(Atoms,PS,MaxN,G1,G2).

%% prove primitive atom
prove([[P|Args]|Atoms],PS,MaxN,G1,G2):-
  user:primtest(P,Args),!,
  user:primcall(P,Args),
  prove(Atoms,PS,MaxN,G1,G2).

%% use existing abduction
prove([Atom|Atoms],PS1,MaxN,G1,G2):-
  Atom=[P|_],
  member(sub(Name,P,MetaSub),G1),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,PS1,MaxN,G1,G3),
  prove(Atoms,PS1,MaxN,G3,G2).

%% new abduction
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

prove_deduce(Atom,PS,G):-
  length(G,N),
  prove([Atom],PS,N,G,G).

nproveall([],_PS,_G).
nproveall([Atom|T],PS,G):-
  not(prove_deduce(Atom,PS,G)),
  nproveall(T,PS,G).

iterator(N,M):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),!,
  between(MinN,MaxN,N),
  succ(MaxM,N),
  between(0,MaxM,M).

pprint(G1):-
  reverse(G1,G2),
  pprint_aux(G2).

pprint_aux([]).
pprint_aux([sub(Name,_P,MetaSub)|T]):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  listtocomma(AtomBodyList,Body),
  numbervars((Head:-Body),0,_),
  format('~q.~n', [(Head:-Body)]),
  pprint_aux(T).

listtocomma([E],E):-!.
listtocomma([H|T],(H,R)):-
  listtocomma(T,R).

convert_preds([],[]).
convert_preds(['@'(Atom)|T],[Atom|R]):- !,
  convert_preds(T,R).
convert_preds([List|T],[Atom|R]):- !,
  Atom=..List,
  convert_preds(T,R).

atom_to_list([],[]).
atom_to_list([Atom|T],[AtomAsList|Out]):-
  Atom =..AtomAsList,
  atom_to_list(T,Out).

is_functional([],_,_).
is_functional([Atom|Atoms],PS,G) :-
  user:func_test(Atom,PS,G),
  is_functional(Atoms,PS,G).

:- user:discontiguous(prim/1).
:- user:discontiguous(primcall/2).
:- user:discontiguous(primtest/2).
:- user:discontiguous(metarule/4).
:- user:discontiguous(metarule_init/3).

gen_metarule_name(Clause,Name):-
  copy_term(Clause,Copy),
  numbervars(Copy,0,_),
  write_to_chars(Copy,Codes),
  atom_codes(Name,Codes).

user:term_expansion(prim(P/A),[prim(P/A),primtest(P,Args),(primcall(P,Args):-Call)]):-
  functor(Call,P,A),
  Call=..[P|Args].

user:term_expansion(metarule(MetaSub,Clause),Asserts):-
  gen_body(MetaSub,Clause,PS,Body),
  user:term_expansion((metarule(MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(MetaSub,Clause,PS):-Body),Asserts):-
  is_list(MetaSub),
  gen_metarule_name(id(MetaSub,Clause,Body),Name),
  user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause),Asserts):-
  atom(Name),
  gen_body(MetaSub,Clause,PS,Body),
  user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  Asserts = [
   (metarule(Name,MetaSub,Clause,PS):-Body),
   (metarule_init(Name,MetaSub,Clause))
  ].

%% automatically adds the bindings for the metasubs
gen_body(MetaSub,([P|_]:-Goals),PS,Body):-
  remove_var(P,MetaSub,MetaSubNew),
  gen_body_aux(MetaSubNew,Goals,PS,Body).

gen_body_aux([],_Goals,_PS,true):-!.

gen_body_aux(Vars,[[Var|Args]|Goals],PS,(member(Var/A,PS),Body)):-
  select_var(Var,Vars,NewVars),!,
  length(Args,A),
  gen_body_aux(NewVars,Goals,PS,Body).

gen_body_aux(Vars,[_|Goals],PS,Body):-
  gen_body_aux(Vars,Goals,PS,Body).

remove_var(_Var,[],[]).

remove_var(Var,[V|Vars],R):-
  Var==V,!,
  remove_var(Var,Vars,R).

remove_var(Var,[V|Vars],[V|R]):-
  remove_var(Var,Vars,R).

select_var(Var,[V|Vars],Vars):-
  Var==V,!.

select_var(Var,[V|Vars],[V|R]):-
  select_var(Var,Vars,R).
