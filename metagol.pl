%% This is a copyrighted file under the BSD 3-clause licence, details of which can be found in the root directory.

:- module(metagol,[learn/2,learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).

:- user:use_module(library(lists)).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- dynamic
    functional/0,
    unfold_program/0,
    print_ordering/0,
    min_clauses/1,
    max_clauses/1,
    max_inv_preds/1,
    metarule_next_id/1,
    user:ibk/3,
    user:prim/1,
    user:primcall/2.

:- discontiguous
    user:metarule/8,
    user:metarule_init/7,
    user:prim/1,
    user:primcall/2.

default(min_clauses(1)).
default(max_clauses(6)).
default(metarule_next_id(0)).
default(max_inv_preds(10)).

%% learn a program from pos and neg examples
learn(Pos1,Neg1):-
    learn(Pos1,Neg1,Prog),
    pprint(Prog).

%% same as above but also returns the program as a list of metasubs
learn(Pos1,Neg1,Prog):-
    maplist(atom_to_list,Pos1,Pos2),
    maplist(atom_to_list,Neg1,Neg2),
    proveall(Pos2,Sig,Prog),
    %% ground(Prog),
    nproveall(Neg2,Sig,Prog),

    is_functional(Pos2,Sig,Prog).

proveall(Atoms,Sig,Prog):-
    target_predicate(Atoms,P/A),
    format('% learning ~w\n',[P/A]),
    iterator(MaxN),
    format('% clauses: ~d\n',[MaxN]),
    invented_symbols(MaxN,P/A,Sig),
    prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove_deduce([Atom],FullSig,Prog1),!,
    is_functional([Atom],Sig,Prog1),
    prove_examples(Atoms,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
prove_examples([Atom1|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    add_empty_path(Atom1,Atom2),
    prove([Atom2],FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    prove_examples(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

prove_deduce(Atoms1,Sig,Prog):-
    maplist(add_empty_path,Atoms1,Atoms2),
    length(Prog,N),
    prove(Atoms2,Sig,_,N,N,N,Prog,Prog).

prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    prove(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-!,
    user:call(Atom).

%% prove primitive atom
prove_aux(p(prim,P,_A,Args,_,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-
    user:primcall(P,Args).

%% can we skip this if no inter?
%% can add a check in setup to skip if not used - I am unsure how to retract this specific clause %% could retract if I add some name to the clause %% only works if inter/2 is below the corresponding definition
prove_aux(p(inv,_P,_A,_Args,Atom,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    user:ibk(Atom,Body,Path),
    prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

%% use existing abduction
prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    select_lower(P,A,FullSig,Sig1,Sig2),
    member(sub(Name,P,A,Subs,PredTypes),Prog1),
    user:metarule_init(Name,Subs,PredTypes,Atom,Body1,Recursive,[Atom|Path]),
    (Recursive==true -> \+memberchk(Atom,Path); true),
    prove(Body1,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

%% new abduction
prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    (N1 == MaxN -> fail; true),
    bind_lower(P,A,FullSig,Sig1,Sig2),
    user:metarule(Name,Subs,PredTypes,Atom,Body1,FullSig,Recursive,[Atom|Path]),
    (Recursive==true -> \+memberchk(Atom,Path); true),
    check_new_metasub(Name,P,A,Subs,Prog1),
    succ(N1,N3),
    prove(Body1,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs,PredTypes)|Prog1],Prog2).

add_empty_path([P|Args],p(inv,P,A,Args,[P|Args],[])):-
    size(Args,A).

select_lower(P,A,FullSig,_Sig1,Sig2):-
    nonvar(P),!,
    append(_,[sym(P,A,_)|Sig2],FullSig),!.

select_lower(P,A,_FullSig,Sig1,Sig2):-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U)-> !,fail;true ).

bind_lower(P,A,FullSig,_Sig1,Sig2):-
    nonvar(P),!,
    append(_,[sym(P,A,_)|Sig2],FullSig),!.

bind_lower(P,A,_FullSig,Sig1,Sig2):-
    append(_,[sym(P,A,U)|Sig2],Sig1),
    (var(U)-> U = 1,!;true).

check_new_metasub(Name,P,A,Subs,Prog):-
    memberchk(sub(Name,P,A,_,_),Prog),!,
    last(Subs,X),
    when(ground(X),\+memberchk(sub(Name,P,A,Subs,_),Prog)).
check_new_metasub(_Name,_P,_A,_Subs,_Prog).

size([],0) :-!.
size([_],1) :-!.
size([_,_],2) :-!.
size([_,_,_],3) :-!.
size(L,N):- !,
  length(L,N).

nproveall([],_PS,_Prog):- !.
nproveall([Atom|Atoms],PS,Prog):-
    \+ prove_deduce([Atom],PS,Prog),
    nproveall(Atoms,PS,Prog).

iterator(N):-
    get_option(min_clauses(MinN)),
    get_option(max_clauses(MaxN)),
    between(MinN,MaxN,N).

target_predicate([[P|Args]|_],P/A):-
    length(Args,A).

invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]):-
    NumSymbols is MaxClauses-1,
    get_option(max_inv_preds(MaxInvPreds)),
    M is min(NumSymbols,MaxInvPreds),
    findall(sym(InvSym,_Artiy,_Used),(between(1,M,I),atomic_list_concat([P,'_',I],InvSym)),Sig).

%% learns a sequence of programs and asserts each program that it learns
learn_seq(Seq,Prog):-
    maplist(learn_task,Seq,Progs),
    flatten(Progs,Prog).

learn_task(Pos/Neg,Prog1):-
    learn(Pos,Neg,Prog1),!,
    maplist(metasub_to_clause_list,Prog1,Prog3),
    maplist(remove_orderings,Prog3,Prog4),
    maplist(clause_list_to_clause,Prog4,Prog2),
    foreach(memberchk(Clause,Prog2),assert(user:Clause)),
    findall(P/A,(member(sub(_Name,P,A,_Subs,_PredTypes),Prog2)),Prims),!,
    list_to_set(Prims,PrimSet),
    maplist(assert_prim,PrimSet).
learn_task(_,[]).

pprint(Prog1):-
    map_list_to_pairs(arg(2),Prog1,Pairs),
    keysort(Pairs,Sorted),
    pairs_values(Sorted,Prog2),
    maplist(metasub_to_clause_list,Prog2,Prog3),
    (get_option(unfold_program) -> unfold_program(Prog3,Prog4); Prog3=Prog4),
    maplist(remove_orderings,Prog4,Prog5),
    maplist(clause_list_to_clause,Prog5,Prog6),
    maplist(pprint_clause,Prog6).

remove_orderings([],[]).
remove_orderings(['@'(_H)|T],Out):-!,
    remove_orderings(T,Out).
remove_orderings([H|T],[H|Out]):-
    remove_orderings(T,Out).

pprint_clause(Clause):-
    numbervars(Clause,0,_),
    format('~q.~n',[Clause]).

clause_list_to_clause([H|B1],Clause):-
    list_to_atom(H,Head),
    (B1 = [] ->Clause=Head;(
        maplist(list_to_atom,B1,B2),
        list_to_clause(B2,B3),
        Clause = (Head:-B3))).

%% construct clause is horrible and needs refactoring
metasub_to_clause_list(sub(Name,_,_,Subs,_),[HeadList|BodyAsList2]):-
    user:metarule_init(Name,Subs,_,HeadList,BodyAsList1,_,_),
    add_path_to_body(BodyAsList2,_,BodyAsList1,_).

list_to_clause([Atom],Atom):-!.
list_to_clause([Atom|T1],(Atom,T2)):-!,
    list_to_clause(T1,T2).

list_to_atom(AtomList,Atom):-
    Atom =..AtomList.
atom_to_list(Atom,AtomList):-
    Atom =..AtomList.

is_functional(Atoms,Sig,Prog):-
    (get_option(functional) -> is_functional_aux(Atoms,Sig,Prog); true).
is_functional_aux([],_Sig,_Prog).
is_functional_aux([Atom|Atoms],Sig,Prog):-
    user:func_test(Atom,Sig,Prog),
    is_functional_aux(Atoms,Sig,Prog).

get_option(Option):-call(Option), !.
get_option(Option):-default(Option).

set_option(Option):-
    functor(Option,Name,Arity),
    functor(Retract,Name,Arity),
    retractall(Retract),
    assert(Option).

%% expand user prims
user:term_expansion(prim(P/A),[user:prim(P/A),user:(primcall(P,Args):-user:Call)]):-
    functor(Call,P,A),
    Call =.. [P|Args].

%% expand user IBK
user:term_expansion(ibk(Head,Body1),user:ibk(Head,Body2,Path)):-
    add_path_to_body(Body1,Path,Body2,_).

%% legacy clauses
user:term_expansion(metarule(Subs,(Head:-Body)),Asserts):-
    get_asserts(_Name,Subs,Head,Body,_,_PS,Asserts).
user:term_expansion(metarule(Name,Subs,(Head:-Body)),Asserts):-
    get_asserts(Name,Subs,Head,Body,_,_PS,Asserts).

%% new ones
user:term_expansion(metarule(Subs,Head,Body),Asserts):-
    get_asserts(_Name,Subs,Head,Body,_,_PS,Asserts).
user:term_expansion(metarule(Name,Subs,Head,Body),Asserts):-
    get_asserts(Name,Subs,Head,Body,_,_PS,Asserts).
user:term_expansion((metarule(Subs,Head,Body):-MetaBody),Asserts):-
    get_asserts(_Name,Subs,Head,Body,MetaBody,_PS,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body):-MetaBody),Asserts):-
    get_asserts(Name,Subs,Head,Body,MetaBody,_PS,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body,PS):-MetaBody),Asserts):-
    get_asserts(Name,Subs,Head,Body,MetaBody,PS,Asserts).

%% build the internal metarule clauses
get_asserts(Name,Subs,Head,Body1,MetaBody,PS,[MRule,metarule_init(AssertName,Subs,PredTypes,Head,Body2,Recursive,Path)]):-
    Head = [P|_],
    is_recursive(Body1,P,Recursive),
    add_path_to_body(Body1,Path,Body2,PredTypes),
    gen_metarule_id(Name,AssertName),
    (var(MetaBody) ->
        MRule = metarule(AssertName,Subs,PredTypes,Head,Body2,PS,Recursive,Path);
        MRule = (metarule(AssertName,Subs,PredTypes,Head,Body2,PS,Recursive,Path):-MetaBody)).

is_recursive([],_,false).
is_recursive([[Q|_]|_],P,true):-
    Q==P,!.
is_recursive([_|T],P,Res):-
    is_recursive(T,P,Res).

add_path_to_body([],_Path,[],[]).
add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest],Out):-
    add_path_to_body(Atoms,Path,Rest,Out).
add_path_to_body([[P|Args]|Atoms],Path,[p(PType,P,A,Args,[P|Args],Path)|Rest],[PType|Out]):-
    size(Args,A),
    add_path_to_body(Atoms,Path,Rest,Out).

gen_metarule_id(Name,Name):-
    ground(Name),!.
gen_metarule_id(_Name,IdNext):-
    get_option(metarule_next_id(Id)),
    succ(Id,IdNext),
    set_option(metarule_next_id(IdNext)).

assert_prim(Prim):-
    prim_asserts(Prim,Asserts),
    maplist(assertz,Asserts).

retract_prim(Prim):-
    Prim = P/_,
    retractall(user:prim(Prim)),
    retractall(user:primcall(P,_)).

prim_asserts(P/A,[user:prim(P/A), user:(primcall(P,Args):-user:Call)]):-
    functor(Call,P,A),
    Call =.. [P|Args].

clause_to_list((Atom,T1),[Atom|T2]):-
    clause_to_list(T1,T2).
clause_to_list(Atom,[Atom]):- !.

ho_atom_to_list(Atom,T):-
    Atom=..AtomList,
    AtomList = [call|T],!.
ho_atom_to_list(Atom,AtomList):-
    Atom=..AtomList.

unfold_clause(C1,[[P|Args]|C2],P,D):-
    append(Pre,[[P|Args]|Post],C1),
    append(Pre,C2,C_),
    append(C_,Post,D).

head([H|_],H).
head_pred([[P|_]|_],P).
body_preds([_|T],Ps):-
    findall(P,(member(X,T),head(X,P)),Ps).

does_not_appear_twice(P,Prog):-
    findall(Q,(
        member(C,Prog),
        body_preds(C,Cs),
        member(Q,Cs)),Qs1),
    select(P,Qs1,Qs2),
    \+ member(P,Qs2).


unfold_program(Prog1,Prog2):-
    select(C1,Prog1,Prog3),
    head_pred(C1,P),
    % check that the head pred does not appear in a head elsewhere in the program
    \+ (member(X,Prog3),head_pred(X,P)),
    % check that the head pred does not appear in the body
    \+ (body_preds(C1,Ps),member(P,Ps)),
    % check that that head pred does not appear twice in the program
    does_not_appear_twice(P,Prog3),
    select(C2,Prog3,Prog4),
    body_preds(C2,C2Body),
    member(P,C2Body),
    unfold_clause(C2,C1,P,D),
    unfold_program([D|Prog4],Prog2).
unfold_program(Prog,Prog):-!.