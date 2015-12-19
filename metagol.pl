:- module(metagol,[learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).

:- user:use_module(library(lists)).

:- use_module(library(lists)).
:- use_module(library(apply)).

:- dynamic
    functional/0,
    min_clauses/1,
    max_clauses/1,
    metarule_next_id/1,
    user:prim/1,
    user:primtest/2,
    user:primcall/2.

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(1)).

learn(Pos1,Neg1,G):-
  atom_to_list(Pos1,Pos2),
  atom_to_list(Neg1,Neg2),
  proveall(Pos2,Sig,G),
  nproveall(Neg2,Sig,G),
  (get_option(functional) -> is_functional(Pos2,Sig,G); true).

learn_seq(Seq,G2):-
  learn_seq_aux(Seq,G1),
  flatten(G1,G2).

learn_seq_aux([],[]).
learn_seq_aux([Pos/Neg|T],[G|Out]):-
  learn(Pos,Neg,G),!,
  maplist(assert_clause,G),
  assert_prims(G),!,
  learn_seq_aux(T,Out).

proveall(Atoms,Sig2,G):-
  initial_sig(Sig1),
  target_predicate(Atoms,Name/Arity),
  iterator(N,M),
  format('% clauses: ~d invented predicates: ~d\n',[N,M]),
  augmented_sig(Name/Arity,M,Sig1,Sig2),
  prove(Atoms,Sig2,N,[],G).

prove([],_,_,G,G).

%% prove order constraint
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
  \+ memberchk(sub(Name,P,MetaSub),G1),
  prove(Body,Sig1,MaxN,[sub(Name,P,MetaSub)|G1],G3),
  prove(Atoms,Sig1,MaxN,G3,G2).

prove_deduce(Atoms,PS,G):-
  length(G,N),
  prove(Atoms,PS,N,G,G).

nproveall([],_,_):-!.
nproveall(Atoms,PS,G) :-
  \+ prove_deduce(Atoms,PS,G).

iterator(N,M):-
  get_option(min_clauses(MinN)),
  get_option(max_clauses(MaxN)),
  between(MinN,MaxN,N),
  succ(MaxM,N),
  between(0,MaxM,M).

target_predicate([[P|Args]|_],P/A):-
  length(Args,A).

invented_symbols(M,Name,Sig) :-
  findall( InvSym/_Artiy,
           ( between(1,M,I),
             atomic_list_concat([Name,'_',I],InvSym)
           ),
           Sig).

lower_sig([P|Args],P,Sig1,Sig2):-
  length(Args,A),
  append(_,[P/A|Sig2],Sig1),!.

augmented_sig(P/A,M,Sig1,[P/A|Sig2]):-
  invented_symbols(M,P,InventedSymbols),
  append(InventedSymbols,Sig1,Sig2).

initial_sig(Prims):-
  findall(P/A,user:prim(P/A),Prims).

pprint(G1):-
  reverse(G1,G2),
  maplist(pprint_clause,G2).

pprint_clause(Sub):-
  construct_clause(Sub,Clause),
  numbervars(Clause,0,_),
  format('~q.~n', [Clause]).

construct_clause(sub(Name,_P,MetaSub),AtomClause):-
  user:metarule_init(Name,MetaSub,Clause),
  copy_term(Clause,(ListHead:-ListBodyWithAts)),
  Head=..ListHead,
  convert_preds(ListBodyWithAts,AtomBodyList),
  (AtomBodyList==[] -> AtomClause=Head
                       ;
                       listtocomma(AtomBodyList,Body),
                       AtomClause=(Head:-Body)
  ).

listtocomma([E],E):-!.
listtocomma([H|T],(H,R)):-
  listtocomma(T,R).

convert_preds([],[]).
convert_preds(['@'(Atom)|T],[Atom|R]):- !,
  convert_preds(T,R).
convert_preds([List|T],[Atom|R]):-
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

assert_prims(G):-
  setof(P/A,(
    member(sub(Name,_,MetaSub),G),
    user:metarule_init(Name,MetaSub,([P|Args]:-_)),
    length(Args,A)),
  Prims),
  assert_prims_aux(Prims).

assert_prims_aux([]).
assert_prims_aux([P/A|T]):-
  functor(Call,P,A),
  Call=..[P|Args],
  assert(user:prim(P/A)),
  assert(user:primtest(P,Args)),
  assert(user:(primcall(P,Args):- Call)),!,
  assert_prims_aux(T).

assert_clause(Sub):-
  construct_clause(Sub,Clause),!,
  assert(user:Clause),!.

get_option(Option):-call(Option),!.
get_option(Option):-default(Option).

set_option(Option):-
  functor(Option,Name,Arity),
  functor(Retract,Name,Arity),
  retractall(Retract),
  assert(Option).

:- multifile
    user:prim/1,
    user:primcall/2,
    user:primtest/2.

:- discontiguous
    user:prim/1,
    user:primcall/2,
    user:primtest/2.

:- discontiguous
    user:metarule/4,
    user:metarule_init/3.

gen_metarule_id(Id):-
  get_option(metarule_next_id(Id)),
  succ(Id,IdNext),
  set_option(metarule_next_id(IdNext)).

user:term_expansion(prim(P/A),Asserts):-
  functor(Call,P,A),
  Call=..[P|Args],
  Asserts=[prim(P/A),primtest(P,Args),(primcall(P,Args):-Call)].

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

user:term_expansion((metarule(Name,MetaSub,Clause,PS):-Body),
                    [ (metarule(Name,MetaSub,Clause,PS):-Body),
                      (metarule_init(Name,MetaSub,Clause))
                    ]).

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
