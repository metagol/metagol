:- module(metagol,[learn/4,learn/3,pprint/1]).

:- user:call(op(950,fx,'@')).
:- user:use_module(library(lists)).

:- use_module(library(lists)).
:- use_module(library(charsio)).

:- dynamic(functional/0).
:- dynamic(min_clauses/1).
:- dynamic(max_clauses/1).
:- dynamic(metarule_next_id/1).

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(1)).

get_option(Option):-call(Option),!.
get_option(Option):-default(Option).

set_option(Option):-
  functor(Option,Name,Arity),
  functor(Retract,Name,Arity),
  retractall(Retract),
  assert(Option).

learn(_Name,Pos,Neg,G):- % deprecated
  write('WARNING: metagol learn(Name,...) is deprecated. Use learn/3 instead.'),nl,
  learn(Pos,Neg,G).

learn(Pos1,Neg1,G):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  initial_sig(Pos2,Neg2,Sig1),!,
  proveall(Pos2,Sig1,Sig2,G),
  nproveall(Neg2,Sig2,G),
  (functional -> is_functional(Pos2,Sig2,G); true).

learn_seq_aux([],[]).
learn_seq_aux([(Pos,Neg)|T],[G|Out]):-
  learn(Pos,Neg,G),!,
  %% TODO: assert G to BK, and also assert prims
  learn_seq(T,Out).

proveall(Atoms,Sig1,Sig2,G):-
  target_predicate(Atoms,Name/Arity),!,
  iterator(N,M),
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  augmented_sig(Name/Arity,M,Sig1,Sig2),
  prove(Atoms,Sig2,N,[],G).

prove([],_Sig,_MaxN,G,G).

prove(['@'(Atom)|Atoms],Sig,MaxN,G1,G2):- !,
  user:call(Atom),
  prove(Atoms,Sig,MaxN,G1,G2).

%% prove primitive atom
prove([[P|Args]|Atoms],Sig,MaxN,G1,G2):-
  user:primtest(P,Args),!,
  user:primcall(P,Args),
  prove(Atoms,Sig,MaxN,G1,G2).

%% use existing abduction
prove([Atom|Atoms],Sig,MaxN,G1,G2):-
  Atom=[P|_Args],
  member(sub(Name,P,MetaSub),G1),
  user:metarule_init(Name,MetaSub,(Atom:-Body)),
  prove(Body,Sig,MaxN,G1,G3),
  prove(Atoms,Sig,MaxN,G3,G2).

%% new abduction
prove([Atom|Atoms],Sig1,MaxN,G1,G2):-
  length(G1,L),
  L < MaxN,
  lower_sig(Atom,P,Sig1,Sig2),
  user:metarule(Name,MetaSub,(Atom :- Body),Sig2),
  not(memberchk(sub(Name,P,MetaSub),G1)),
  prove(Body,Sig1,MaxN,[sub(Name,P,MetaSub)|G1],G3),
  prove(Atoms,Sig1,MaxN,G3,G2).

prove_deduce(Atom,Sig,G):-
  length(G,N),
  prove([Atom],Sig,N,G,G).

nproveall([],_Sig,_G).
nproveall([Atom|T],Sig,G):-
  not(prove_deduce(Atom,Sig,G)),
  nproveall(T,Sig,G).

iterator(N,M):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),!,
  between(MinN,MaxN,N),
  succ(MaxM,N),
  between(0,MaxM,M).

target_predicate([[P|Args]|_],P/A):-
  length(Args,A).

invented_symbols(0,_Name,[]):-!.
invented_symbols(M,Name,[InvSym/_Arity|Sig]) :-
  atomic_list_concat([Name,'_',M],InvSym),
  succ(Prev,M),
  invented_symbols(Prev,Name,Sig).

lower_sig([P|Args],P,Sig1,Sig2):-
  length(Args,A),
  append(_,[P/A|Sig2],Sig1),!.

augmented_sig(P/A,M,Sig1,[P/A|Sig2]):-
  invented_symbols(M,P,InventedSymbols),
  append(InventedSymbols,Sig1,Sig2).

initial_sig(_Pos,_Neg,Prims):- !,
  findall(P/A,user:prim(P/A),Prims).

pprint(G1):-
  reverse(G1,G2),
  pprint_aux(G2).

pprint_aux([]).
pprint_aux([sub(Name,_P,MetaSub)|T]):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  (AtomBodyList==[] -> AtomClause=Head
                       ;
                       listtocomma(AtomBodyList,Body),
                       AtomClause=(Head:-Body)
  ),
  numbervars(AtomClause,0,_),
  format('~q.~n', [AtomClause]),
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
is_functional([Atom|Atoms],Sig,G) :-
  user:func_test(Atom,Sig,G),
  is_functional(Atoms,Sig,G).

:- user:multifile(prim/1).
:- user:multifile(primcall/2).
:- user:multifile(primtest/2).

:- user:discontiguous(prim/1).
:- user:discontiguous(primcall/2).
:- user:discontiguous(primtest/2).

:- user:discontiguous(metarule/4).
:- user:discontiguous(metarule_init/3).

gen_metarule_id(Id):-
  get_option(metarule_next_id(Id)),
  succ(Id,IdNext),
  set_option(metarule_next_id(IdNext)),!.

user:term_expansion(prim(P/A),[prim(P/A),primtest(P,Args),(primcall(P,Args):-Call)]):-
  functor(Call,P,A),
  Call=..[P|Args].

user:term_expansion(metarule(MetaSub,Clause),Asserts):-
  gen_body(MetaSub,Clause,PS,Body),
  clean_metasub(MetaSub,MetaSubClean),
  user:term_expansion((metarule(MetaSubClean,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(MetaSub,Clause,PS):-Body),Asserts):-
  is_list(MetaSub),
  gen_metarule_id(Name),
  user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

user:term_expansion(metarule(Name,MetaSub,Clause),Asserts):-
  atom(Name),
  gen_body(MetaSub,Clause,PS,Body),
  clean_metasub(MetaSub,MetaSubClean),
  user:term_expansion((metarule(Name,MetaSubClean,Clause,PS):-Body),Asserts).

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts).

add_metaruleinit((metarule(Name,MetaSub,Clause,PS):-Body),Asserts):-
  Asserts = [
   (metarule(Name,MetaSub,Clause,PS):-Body),
   (metarule_init(Name,MetaSub,Clause))
  ].

%% automatically adds the bindings for the metasubs
gen_body(MetaSub,(Head:-Goals),PS,Body):-
  term_variables(Head,HeadVars),
  remove_vars(HeadVars,MetaSub,MetaSubNew),
  gen_body_aux(MetaSubNew,Goals,PS,Body).

gen_body_aux([],_Goals,_PS,true):-!.

gen_body_aux(Vars1,[[Var|Args]|Goals],PS,(member(Var/Arity,PS),Body)):-
  select_var(Var,Arity,Vars1,Vars2),!,
  (var(Arity)->length(Args,Arity);true),
  term_variables(Args,TermVars),
  remove_vars(TermVars,Vars2,Vars3),
  gen_body_aux(Vars3,Goals,PS,Body).

gen_body_aux(Vars1,[[_|Args]|Goals],PS,Body):-
  term_variables(Args,TermVars),
  remove_vars(TermVars,Vars1,Vars2),
  gen_body_aux(Vars2,Goals,PS,Body).

clean_metasub([],[]).

clean_metasub([Var|Vars],[Var|R]):-
    var(Var),!,
    clean_metasub(Vars,R).

clean_metasub([Var/_Arity|Vars],[Var|R]):-
    clean_metasub(Vars,R).

remove_vars([],Vars,Vars).

remove_vars([Var|VarList],Vars,R):-
    remove_var(Var,Vars,VarsNew),
    remove_vars(VarList,VarsNew,R).

remove_var(_Var,[],[]).

remove_var(Var,[V/_|Vars],R):-
  Var==V,!,
  remove_var(Var,Vars,R).

remove_var(Var,[V|Vars],R):-
  Var==V,!,
  remove_var(Var,Vars,R).

remove_var(Var,[V|Vars],[V|R]):-
  remove_var(Var,Vars,R).

select_var(Var,_Arity,[V|Vars],Vars):-
  Var==V,!.

select_var(Var,Arity,[V/Arity|Vars],Vars):-
  Var==V,!.

select_var(Var,Arity,[V|Vars],[V|R]):-
  select_var(Var,Arity,Vars,R).
