%% This file is a copyrighted under the BSD 3-clause licence, details of which can be found in the root directory.

:- module(metagol,[learn/2,learn/3,learn_seq/2,pprint/1,op(950,fx,'@')]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- dynamic
    user:head_pred/1,
    user:body_pred/1,
    functional/0,
    body_pred_call/2,
    ibk/3,
    ibk_body_pred_call/2.

:- discontiguous
    metarule/7,
    metarule_init/6.

default(max_clauses(10)).
default(timeout(600)). % 10 minutes

learn(Pos,Neg):-
    learn(Pos,Neg,Prog),
    pprint(Prog).
learn(Pos,Neg,Prog):-
    setup,
    timeout(Time),
    call_with_time_limit(Time,learn_(Pos,Neg,Prog)).

learn_(Pos1,Neg1,Prog):-
    make_atoms(Pos1,Pos2),
    make_atoms(Neg1,Neg2),
    proveall(Pos2,Sig,Prog),
    nproveall(Neg2,Sig,Prog),
    ground(Prog),
    check_functional(Pos2,Sig,Prog).
learn_(_,_,_):-!,
    writeln('% unable to learn a solution'),
    false.

proveall(Atoms,Sig,Prog):-
    target_predicate(Atoms,P/A),
    format('% learning ~w\n',[P/A]),
    iterator(MaxN),
    format('% clauses: ~d\n',[MaxN]),
    invented_symbols(MaxN,P/A,Sig),
    assert_sig_types(Sig),
    prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    deduce_atom(Atom,FullSig,Prog1),!,
    check_functional([Atom],Sig,Prog1),
    prove_examples(Atoms,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove([Atom],FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    check_functional([Atom],Sig,Prog3),
    prove_examples(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

deduce_atom(Atom,Sig,Prog):-
    length(Prog,N),
    prove([Atom],Sig,_,N,N,N,Prog,Prog).

prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
    prove(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):- !,
    user:call(Atom).

prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-
    nonvar(P),
    type(P,A,ibk_body_pred),!,
    ibk_body_pred_call(P,Args).

prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-
    (nonvar(P) -> type(P,A,body_pred); true),
    body_pred_call(P,Args).

prove_aux(p(P,A,Args,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    (nonvar(P) -> (\+ type(P,A,head_pred), !, type(P,A,ibk_head_pred)); true),
    ibk([P|Args],Body,Path),
    prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    Atom=[P|Args],
    select_lower(P,A,FullSig,Sig1,Sig2),
    member(sub(Name,P,A,Subs),Prog1),
    metarule_init(Name,Subs,Atom,Body1,Recursive,[Atom|Path]),
    check_recursion(Recursive,MaxN,Atom,Path),
    prove(Body1,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    N1 < MaxN,
    Atom = [P|Args],
    metarule(Name,Subs,Atom,Body1,FullSig,Recursive,[Atom|Path]), % ??
    bind_lower(P,A,FullSig,Sig1,Sig2),
    check_recursion(Recursive,MaxN,Atom,Path),
    check_new_metasub(Name,P,A,Subs,Prog1),
    succ(N1,N3),
    prove(Body1,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs)|Prog1],Prog2).

nproveall(Atoms,Sig,Prog):-
    forall(member(Atom,Atoms), \+ deduce_atom(Atom,Sig,Prog)).

make_atoms(Atoms1,Atoms2):-
    maplist(atom_to_list,Atoms1,Atoms3),
    maplist(make_atom,Atoms3,Atoms2).

make_atom([P|Args],p(P,A,Args,[])):-
    length(Args,A).

check_functional(Atoms,Sig,Prog):-
    (functional ->
        forall(member(Atom1,Atoms),
        \+ (
            make_atom(Atom2,Atom1),
            user:func_test(Atom2,TestAtom2,Condition),
            make_atom(TestAtom2,TestAtom1),
            deduce_atom(TestAtom1,Sig,Prog),
            \+ call(Condition)));
        true).

check_recursion(false,_,_,_).  %% if not recursive continue
check_recursion(true,1,_,_):- !,false. %% if recursive then check that maxn != 1
check_recursion(true,_,Atom,Path):- %% otherwise check path - this step could be very expensive
    \+memberchk(Atom,Path).

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
    memberchk(sub(Name,P,A,_),Prog),!,
    last(Subs,X),
    when(ground(X), \+memberchk(sub(Name,_P,A,Subs),Prog)).
check_new_metasub(_Name,_P,_A,_Subs,_Prog).

assert_sig_types(Sig):-
    forall((member(sym(P,A,_),Sig),\+type(P,A,head_pred)),assert(type(P,A,head_pred))).

body_preds:-
    findall(P/A,user:body_pred(P/A),S0),
    assert_body_preds(S0).

assert_body_preds(S0):-
    list_to_set(S0,S1),
    forall(member(P/A,S1),(
        retractall(user:body_pred(P/A)),
        retractall(type(P,A,body_pred)),
        retractall(body_pred_call(P,_)),
        functor(Atom,P,A),
        Atom =.. [P|Args],
        (current_predicate(P/A) -> (
            assert(user:body_pred(P/A)),
            assert(type(P,A,body_pred)),
            assert((body_pred_call(P,Args):-user:Atom))
        );
            format('% WARNING: body pred ~w does not exist\n',[P/A])
        )
    )).

head_preds:-
    retractall(type(_,_,head_pred)),
    findall(P/A,user:head_pred(P/A),S0),
    list_to_set(S0,S1),
    forall(member(P/A,S1),(
        assert(type(P,A,head_pred))
    )).

ibk:-
    ibk_head_preds,
    ibk_body_preds.

ibk_head_preds:-
    findall(P/A,type(P,A,ibk_head_pred),S0),
    list_to_set(S0,S1),
    retractall(type(_,_,ibk_head_pred)),
    forall(member(P/A,S1),
        assert(type(P,A,ibk_head_pred))
    ).

ibk_body_preds:-
    findall(P/A,(type(P,A,ibk_body_pred),  not(type(P,A,head_pred)), not(type(P,A,ibk_head_pred))),S0),
    list_to_set(S0,S1),
    retractall(type(_,_,ibk_body_pred)),
    forall(member(P/A,S1),(
        assert(type(P,A,ibk_body_pred)),
        functor(Atom,P,A),
        Atom =..[P|Args],
        assert((ibk_body_pred_call(P,Args):-user:Atom))
    )).

options:-
    (current_predicate(timeout/1) -> true; (default(timeout(Time)),set_option(timeout(Time)))),
    (current_predicate(min_clauses/1) -> true; (set_option(min_clauses(1)))),
    (current_predicate(max_clauses/1) -> true; (default(max_clauses(MaxN)),set_option(max_clauses(MaxN)))),
    (current_predicate(max_inv_preds/1) -> true; (max_clauses(MaxN),succ(MaxInv,MaxN),set_option(max_inv_preds(MaxInv)))).

set_option(Option):-
    functor(Option,Name,Arity),
    functor(Retract,Name,Arity),
    retractall(Retract),
    assert(Option).

setup:-
    options,
    body_preds,
    head_preds,
    ibk.

iterator(N):-
    min_clauses(MinN),
    max_clauses(MaxN),
    between(MinN,MaxN,N).

target_predicate([p(P,A,_Args,[])|_],P/A).

%% target_predicates(Atoms, Preds2):-
%%     findall(P/A, member([p(inv,P,A,_Args,_Atom,[])],Atoms), Preds1),
%%     list_to_set(Preds1,Preds2).

invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]):-
    NumSymbols is MaxClauses-1,
    max_inv_preds(MaxInvPreds),
    M is min(NumSymbols,MaxInvPreds),
    findall(sym(Sym1,_Artiy,_Used1),(between(1,M,I),atomic_list_concat([P,'_',I],Sym1)),Sig1),
    findall(sym(Sym2,Arity2,_Used2),head_pred(Sym2/Arity2),Sig2),
    append(Sig1,Sig2,Sig).

pprint(Prog1):-
    reverse(Prog1,Prog3),
    maplist(metasub_to_clause,Prog3,Prog2),
    maplist(pprint_clause,Prog2).

pprint_clause(C):-
    numbervars(C,0,_),
    format('~q.~n',[C]).

metasub_to_clause(sub(Name,_,_,Subs),Clause2):-
    metarule_init(Name,Subs,HeadList,BodyAsList1,_,_),
    add_path_to_body(BodyAsList3,_,BodyAsList1),
    include(no_ordering,BodyAsList3,BodyAsList2),
    maplist(atom_to_list,ClauseAsList,[HeadList|BodyAsList2]),
    list_to_clause(ClauseAsList,Clause1),
    (Clause1 = (H,T) -> Clause2=(H:-T); Clause2=Clause1).

no_ordering(H):-
    H\='@'(_).

list_to_clause([Atom],Atom):-!.
list_to_clause([Atom|T1],(Atom,T2)):-!,
    list_to_clause(T1,T2).

atom_to_list(Atom,AtomList):-
    Atom =..AtomList.

%% build the internal metarule clauses
user:term_expansion(metarule(Subs,Head,Body),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,_,_PS,Asserts).
user:term_expansion(metarule(Name,Subs,Head,Body),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,_,_PS,Asserts).
user:term_expansion((metarule(Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(_Name,Subs,Head,Body,MetaBody,_PS,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body):-MetaBody),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,MetaBody,_PS,Asserts).
user:term_expansion((metarule(Name,Subs,Head,Body,PS):-MetaBody),Asserts):-
    metarule_asserts(Name,Subs,Head,Body,MetaBody,PS,Asserts).

metarule_asserts(Name,Subs,Head,Body1,MetaBody,PS,Asserts):-
    Head = [P|_],
    is_recursive(Body1,P,Recursive),
    add_path_to_body(Body1,Path,Body2),
    gen_metarule_id(Name,AssertName),
    (var(MetaBody) ->
        MRule = metarule(AssertName,Subs,Head,Body2,PS,Recursive,Path);
        MRule = (metarule(AssertName,Subs,Head,Body2,PS,Recursive,Path):-MetaBody)),
    Asserts = [metagol:MRule,metagol:metarule_init(AssertName,Subs,Head,Body2,Recursive,Path)].

user:term_expansion((ibk(Head,Body):-IbkBody),(ibk(Head,Body):-IbkBody)):-
    ibk_asserts(Head,Body,IbkBody,[]).

user:term_expansion(ibk(Head,Body),ibk(Head,Body)):-
    ibk_asserts(Head,Body,false,[]).

ibk_asserts(Head,Body1,IbkBody,[]):-
    Head = [P0|Args1],
    length(Args1,A0),
    assert(type(P0,A0,ibk_head_pred)),
    add_path_to_body(Body1,Path,Body2),
    (IbkBody == false -> assert(ibk(Head,Body2,Path)); assert((ibk(Head,Body2,Path):-IbkBody))),
    forall(member(p(P1,A1,_,_),Body2),((ground(P1) -> assert(type(P1,A1,ibk_body_pred)); true))).

is_recursive([],_,false).
is_recursive([[Q|_]|_],P,true):-
    Q==P,!.
is_recursive([_|T],P,Res):-
    is_recursive(T,P,Res).

add_path_to_body([],_Path,[]).
add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest]):-
    add_path_to_body(Atoms,Path,Rest).
add_path_to_body([[P|Args]|Atoms],Path,[p(P,A,Args,Path)|Rest]):-
    length(Args,A),
    add_path_to_body(Atoms,Path,Rest).

gen_metarule_id(Name,Name):-
    ground(Name),!.
gen_metarule_id(_Name,IdNext):-
    current_predicate(metarule_next_id/1),!,
    metarule_next_id(Id),
    succ(Id,IdNext),
    set_option(metarule_next_id(IdNext)).
gen_metarule_id(_Name,1):-
    set_option(metarule_next_id(2)).

learn_seq(Seq,Prog):-
    maplist(learn_task,Seq,Progs),
    flatten(Progs,Prog).

learn_task(Pos/Neg,Prog1):-
    learn(Pos,Neg,Prog1),!,
    maplist(metasub_to_clause,Prog1,Prog2),
    forall(member(Clause,Prog2),assert(user:Clause)),
    findall(P/A,(member(sub(_Name,P,A,_Subs),Prog1)),Preds),!,
    assert_body_preds(Preds).
learn_task(_,[]).