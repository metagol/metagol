:- module(metagol,[learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).

:- user:use_module(library(lists)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- dynamic
    functional/0,
    min_clauses/1,
    max_clauses/1,
    metarule_next_id/1.

:- discontiguous
    user:metarule/4,
    user:metarule_init/3,
    user:primcall/2.

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(1)).

learn(Pos1,Neg1,G):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  proveall(Pos2,Sig,G),
  nproveall(Neg2,Sig,G),
  ( get_option(functional)
  -> is_functional(Pos2,Sig,G)
  ;  true ).

learn_seq(Seq,G2):-
  learn_seq_aux(Seq,G1),
  flatten(G1,G2).

learn_seq_aux([],[]).
learn_seq_aux([Pos/Neg|T],[G|Out]):-
  learn(Pos,Neg,G), !,
  maplist(assert_clause,G),
  assert_prims(G),
  learn_seq_aux(T,Out).

proveall(Atoms,Sig,G):-
  target_predicate(Atoms,Name/Arity),
  iterator(N),
  format('% clauses: ~d\n',[N]),
  invented_symbols(N,Name/Arity,Sig),
  prove(Atoms,Sig,Sig,N,cl(0,[]),cl(N,G)).

prove([],_Sig,_FullSig,_MaxN,G,G).
prove([Atom|Atoms],Sig,FullSig,MaxN,G1,G2):-
  prove_aux(Atom,Sig,FullSig,MaxN,G1,G3),
  prove(Atoms,Sig,FullSig,MaxN,G3,G2).

%% prove order constraint
prove_aux('@'(Atom),_Sig,_FullSig,_MaxN,G,G):- !,
  user:call(Atom).
%% prove primitive atom
prove_aux([P|Args],_Sig,_FullSig,_MaxN,G,G):-
  user:primcall(P,Args).
%% use existing abduction
prove_aux(Atom,Sig1,FullSig,MaxN,cl(N,Cl),G2):-
  Atom=[P|_Args],
  select_lower(Atom,P,FullSig,Sig1,Sig2),
  member(sub(Name,P,MetaSub),Cl),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,Sig2,FullSig,MaxN,cl(N,Cl),G2).
%% new abduction
prove_aux(Atom,Sig1,FullSig,MaxN,cl(N1,Cl1),G2):-
  N1 < MaxN,
  succ(N1,N3),
  bind_lower(Atom,P,FullSig,Sig1,Sig2),
  user:metarule(Name,MetaSub,(Atom:-Body),Sig2),
  prove(Body,Sig2,FullSig,MaxN,cl(N3,[sub(Name,P,MetaSub)|Cl1]),G2).

select_lower([P|_],P,FullSig,_Sig1,Sig2):-
  nonvar(P), !,
  append(_,[sym(P,_A,_U)|Sig2],FullSig), !.
select_lower([P|Args],P,_FullSig,Sig1,Sig2):-
  length(Args,A),
  append(_,[sym(P,A,U)|Sig2],Sig1),
  ( var(U)
  -> !, fail
  ;  true ).

bind_lower([P|_],P,FullSig,_Sig1,Sig2):-
  nonvar(P),!,
  append(_,[sym(P,_,_)|Sig2],FullSig), !.
bind_lower([P|Args],P,_FullSig,Sig1,Sig2):-
  length(Args,A),
  append(_,[sym(P,A,U)|Sig2],Sig1),
  ( var(U)
  -> U = 1, !
  ;  true ).

prove_deduce(Atoms,PS,G):-
  length(G,N),
  prove(Atoms,PS,PS,N,cl(N,G),cl(N,G)).

nproveall([],_PS,_G):- !.
nproveall([Atom|Atoms],PS,G):-
  \+ prove_deduce([Atom],PS,G),
  nproveall(Atoms,PS,G).

iterator(N):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),
  between(MinN,MaxN,N).

target_predicate([[P|Args]|_],P/A):-
  length(Args,A).

invented_symbols(N,Name/Arity,[sym(Name,Arity,_U)|Sig]):-
  succ(M,N),
  findall(sym(InvSym,_Artiy,_Used),
          (between(1,M,I),atomic_list_concat([Name,'_',I],InvSym)),
          Sig).

pprint(G1):-
  reverse(G1,G2),
  map_list_to_pairs(arg(2),G2,Pairs),
  keysort(Pairs,Sorted),
  pairs_values(Sorted,G3),
  maplist(pprint_clause,G3).

pprint_clause(Sub):-
  construct_clause(Sub,Clause),
  numbervars(Clause,0,_),
  format('~q.~n',[Clause]).

construct_clause(sub(Name,_P,MetaSub),AtomClause):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  ( AtomBodyList == []
  -> AtomClause = Head
  ;  listtocomma(AtomBodyList,Body),
     AtomClause = (Head:-Body) ).

listtocomma([],true):- !.
listtocomma([E],E):- !.
listtocomma([H|T],(H,R)):-
  listtocomma(T,R).

convert_preds([],[]).
convert_preds(['@'(Atom)|T],[Atom|R]):- !,
  convert_preds(T,R).
convert_preds([List|T],[Atom|R]):-
  Atom =.. List,
  convert_preds(T,R).

atom_to_list([],[]).
atom_to_list([Atom|T],[AtomAsList|Out]):-
  Atom =.. AtomAsList,
  atom_to_list(T,Out).

is_functional([],_Sig,_G).
is_functional([Atom|Atoms],Sig,G):-
  user:func_test(Atom,Sig,G),
  is_functional(Atoms,Sig,G).

get_option(Option):-call(Option), !.
get_option(Option):-default(Option).

set_option(Option):-
  functor(Option,Name,Arity),
  functor(Retract,Name,Arity),
  retractall(Retract),
  assert(Option).

gen_metarule_id(Id):-
  get_option(metarule_next_id(Id)),
  succ(Id,IdNext),
  set_option(metarule_next_id(IdNext)).

user:term_expansion(prim(P/A),user:(primcall(P,Args):-Call)):-
    functor(Call,P,A),
    Call =.. [P|Args].

user:term_expansion(metarule(MetaSub,Clause),Asserts):-
  clean_metasub(MetaSub,MetaSubClean),
  user:term_expansion(metarule(MetaSubClean,Clause,_PS),Asserts).

user:term_expansion(metarule(MetaSub,Clause,PS),Asserts):-
  is_list(MetaSub),
  gen_metarule_id(Name),
  user:term_expansion(metarule(Name,MetaSub,Clause,PS),Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause),Asserts):-
  atom(Name),
  clean_metasub(MetaSub,MetaSubClean),
  user:term_expansion(metarule(Name,MetaSubClean,Clause,_PS),Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause,PS),
                   [metarule(Name,MetaSub,Clause,PS),
                    metarule_init(Name,MetaSub,Clause)]).

clean_metasub([],[]).
clean_metasub([Var|Vars],[Var|R]):-
  var(Var), !,
  clean_metasub(Vars,R).
clean_metasub([Var/_Arity|Vars],[Var|R]):-
  clean_metasub(Vars,R).
