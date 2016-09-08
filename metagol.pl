%% This is a copyrighted file under the BSD 3-clause licence, details of which can be found in the root directory.

:- module(metagol,[learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).

:- user:use_module(library(lists)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- dynamic
    functional/0,
    print_ordering/0,
    min_clauses/1,
    max_clauses/1,
    metarule_next_id/1,
    interpreted_bk/2,
    user:prim/1,
    user:primcall/2.

:- discontiguous
    user:metarule/4,
    user:metarule_init/3,
    user:prim/1,
    user:primcall/2.

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(1)).

learn(Pos1,Neg1,G):-
  maplist(atom_to_list,Pos1,Pos2),
  maplist(atom_to_list,Neg1,Neg2),
  proveall(Pos2,Sig,G),
  nproveall(Neg2,Sig,G),
  is_functional(Pos2,Sig,G).

learn_seq(Seq,G2):-
  maplist(learn_task,Seq,G1),
  flatten(G1,G2).

learn_task(Pos/Neg,G):-
  learn(Pos,Neg,G),!,
  maplist(assert_clause,G),
  assert_prims(G).

proveall(Atoms,Sig,G):-
  target_predicate(Atoms,P/A),
  format('% learning ~w\n',[P/A]),
  iterator(MaxN),
  format('% clauses: ~d\n',[MaxN]),
  invented_symbols(MaxN,P/A,Sig),
  prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],G).

prove_examples([],_FullSig,_Sig,_MaxN,N,N,G,G).
prove_examples([E|Es],FullSig,Sig,MaxN,N1,N2,G1,G2):-
  prove_deduce([E],FullSig,G1),!,
  is_functional([E],Sig,G1),
  prove_examples(Es,FullSig,Sig,MaxN,N1,N2,G1,G2).
prove_examples([E|Es],FullSig,Sig,MaxN,N1,N2,G1,G2):-
  prove([E],FullSig,Sig,MaxN,N1,N3,G1,G3),
  prove_examples(Es,FullSig,Sig,MaxN,N3,N2,G3,G2).

prove_deduce(Atoms,Sig,G):-
  length(G,N),
  prove(Atoms,Sig,_,N,N,N,G,G).

prove([],_FullSig,_Sig,_MaxN,N,N,G,G).
prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,G1,G2):-
  prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,G1,G3),
  prove(Atoms,FullSig,Sig,MaxN,N3,N2,G3,G2).

%% prove order constraint
prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,G,G):-!,
  user:call(Atom).

%% prove primitive atom
prove_aux([P|Args],_FullSig,_Sig,_MaxN,N,N,G,G):-
  (nonvar(P)-> (user:prim(P/_),!,user:primcall(P,Args)); user:primcall(P,Args)).

%% use interpreted BK
prove_aux(Atom,FullSig,Sig,MaxN,N1,N2,G1,G2):-
  interpreted_bk(Atom,Body),
  %% writeln(Body),
  prove(Body,FullSig,Sig,MaxN,N1,N2,G1,G2).

%% use existing abduction
prove_aux(Atom,FullSig,Sig1,MaxN,N1,N2,G1,G2):-
  Atom=[P|Args],
  length(Args,A),
  select_lower(P,A,FullSig,Sig1,Sig2),
  member(sub(Name,P,A,MetaSub),G1),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,FullSig,Sig2,MaxN,N1,N2,G1,G2).

%% new abduction
prove_aux(Atom,FullSig,Sig1,MaxN,N1,N2,G1,G2):-
  N1 < MaxN,
  Atom=[P|Args],
  length(Args,A),
  bind_lower(P,A,FullSig,Sig1,Sig2),
  user:metarule(Name,MetaSub,(Atom:-Body),FullSig),
  check_new_metasub(Name,P,A,MetaSub,G1),
  succ(N1,N3),
  prove(Body,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,MetaSub)|G1],G2).

select_lower(P,A,FullSig,_Sig1,Sig2):-
  nonvar(P),!,
  ((append(_,[sym(P,A,_)|Sig2],FullSig),!);Sig2=[]).

select_lower(P,A,_FullSig,Sig1,Sig2):-
  append(_,[sym(P,A,U)|Sig2],Sig1),
  (var(U)-> !,fail;true ).

bind_lower(P,A,FullSig,_Sig1,Sig2):-
  nonvar(P),!,
  ((append(_,[sym(P,A,_)|Sig2],FullSig),!);Sig2=[]).

bind_lower(P,A,_FullSig,Sig1,Sig2):-
  append(_,[sym(P,A,U)|Sig2],Sig1),
  (var(U)-> U = 1,!;true).

check_new_metasub(Name,P,A,MetaSub,G):-
  memberchk(sub(Name,P,A,_),G),!,
  last(MetaSub,X),
  when(nonvar(X),\+memberchk(sub(Name,P,A,MetaSub),G)).
check_new_metasub(_Name,_P,_A,_MetaSub,_G).

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

invented_symbols(N,P/A,[sym(P,A,_U)|Sig]):-
  succ(M,N),
  findall(sym(InvSym,_Artiy,_Used),(between(1,M,I),atomic_list_concat([P,'_',I],InvSym)),Sig).

pprint(G1):-
  map_list_to_pairs(arg(2),G1,Pairs),
  keysort(Pairs,Sorted),
  pairs_values(Sorted,G2),
  maplist(pprint_clause,G2).

pprint_clause(Sub):-
  construct_clause(Sub,Clause),
  numbervars(Clause,0,_),
  format('~q.~n',[Clause]).

%% construct clause is horrible and needs refactoring
construct_clause(sub(Name,_,_,MetaSub),Clause):-
  user:metarule_init(Name,MetaSub,ClauseAsList),
  copy_term(ClauseAsList,(HeadList:-BodyAsList)),
  atom_to_list(Head,HeadList),
  (BodyAsList == [] ->Clause=Head;(pprint_list_to_clause(BodyAsList,Body),Clause = (Head:-Body))).

pprint_list_to_clause(List1,Clause):-
  atomsaslists_to_atoms(List1,List2),
  list_to_clause(List2,Clause).

atomsaslists_to_atoms([],[]).
atomsaslists_to_atoms(['@'(Atom)|T1],Out):- !,
  (get_option(print_ordering) -> Out=[Atom|T2]; Out=T2),
  atomsaslists_to_atoms(T1,T2).
atomsaslists_to_atoms([AtomAsList|T1],[Atom|T2]):-
  atom_to_list(Atom,AtomAsList),
  atomsaslists_to_atoms(T1,T2).

list_to_clause([Atom],Atom):-!.
list_to_clause([Atom|T1],(Atom,T2)):-!,
  list_to_clause(T1,T2).

list_to_atom(AtomList,Atom):-
  Atom =..AtomList.
atom_to_list(Atom,AtomList):-
  Atom =..AtomList.

is_functional(Atoms,Sig,G):-
  (get_option(functional) -> is_functional_aux(Atoms,Sig,G); true).
is_functional_aux([],_Sig,_G).
is_functional_aux([Atom|Atoms],Sig,G):-
  user:func_test(Atom,Sig,G),
  is_functional_aux(Atoms,Sig,G).

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

user:term_expansion(prim(P/A),[user:prim(P/A),user:(primcall(P,Args):-Call)]):-
  functor(Call,P,A),
  Call =.. [P|Args].

user:term_expansion(metarule(MetaSub,Clause),Asserts):-
  get_asserts(_Name,MetaSub,Clause,Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause),Asserts):-
  get_asserts(Name,MetaSub,Clause,Asserts).

user:term_expansion((metarule(MetaSub,Clause):-Body),Asserts):-
  gen_metarule_id(Name),
  user:term_expansion((metarule(Name,MetaSub,Clause):-Body),Asserts).

user:term_expansion((metarule(Name,MetaSub,Clause):-Body),Asserts):-
  user:term_expansion((metarule(Name,MetaSub,Clause,_PS):-Body),Asserts).

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  Asserts = [(metarule(Name,MetaSub,Clause,PS):-Body),metarule_init(Name,MetaSub,Clause)].

user:term_expansion(interpreted(P/A),L2):-
  functor(Head,P,A),
  findall((Head:-Body),user:clause(Head,Body),L1),
  maplist(convert_to_interpreted,L1,L2).
  %% maplist(writeln,L2).

get_asserts(Name,MetaSub,Clause,Asserts):-
  (var(Name)->gen_metarule_id(AssertName);AssertName=Name),
  Asserts = [
    metarule(AssertName,MetaSub,Clause,_PS),
    metarule_init(AssertName,MetaSub,Clause)
  ].

assert_program(G):-
  maplist(assert_clause,G).

assert_clause(Sub):-
  construct_clause(Sub,Clause),
  assert(user:Clause).

assert_prims(G):-
  findall(P/A,(member(sub(_Name,P,A,_MetaSub),G)),Prims),!,
  list_to_set(Prims,PrimSet),
  maplist(assert_prim,PrimSet).

assert_prim(Prim):-
  prim_asserts(Prim,Asserts),
  maplist(assertz,Asserts).

prim_asserts(P/A,[user:prim(P/A), user:(primcall(P,Args):-Call)]):-
  functor(Call,P,A),
  Call =.. [P|Args].

convert_to_interpreted((Head:-true),metagol:(interpreted_bk(HeadAsList,[]))):-!,
  ho_atom_to_list(Head,HeadAsList).

convert_to_interpreted((Head:-Body),metagol:(interpreted_bk(HeadAsList,BodyList2))):-
  ho_atom_to_list(Head,HeadAsList),
  clause_to_list(Body,BodyList1),
  maplist(ho_atom_to_list,BodyList1,BodyList2).

clause_to_list((Atom,T1),[Atom|T2]):-
  clause_to_list(T1,T2).
clause_to_list(Atom,[Atom]):- !.

ho_atom_to_list(Atom,T):-
  Atom=..AtomList,
  AtomList = [call|T],!.
ho_atom_to_list(Atom,AtomList):-
  Atom=..AtomList.