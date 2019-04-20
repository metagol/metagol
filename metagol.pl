%% more refactoring
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

learn(Pos1,Neg1):-
    learn(Pos1,Neg1,Prog),
    pprint(Prog).

learn(Pos1,Neg1,Prog):-
    setup,
    %% convert atoms to internal atoms of the form p(Type,P,A,Args,Atom,Path)
    make_atoms(Pos1,Pos2),
    make_atoms(Neg1,Neg2),
    proveall(Pos2,Sig,Prog),
    nproveall(Neg2,Sig,Prog),
    ground(Prog),
    check_functional(Pos2,Sig,Prog).

proveall(Atoms,Sig,Prog):-
    %% TODO - SHOULD GET ALL OF THE TARGET PREDICATES
    target_predicate(Atoms,P/A),
    format('% learning ~w\n',[P/A]),
    iterator(MaxN),
    format('% clauses: ~d\n',[MaxN]),
    invented_symbols(MaxN,P/A,Sig),
    prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).

prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    %% if we can deduce the atom from the program then do not try to 'learn' a solution to it (hence !)
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

%% used when the user gives an ordering over the herbrand base prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-!,
prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-!,
    user:call(Atom).

%% prove primitive atom
prove_aux(p(prim,P,_A,Args,_,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog):-
    user:primcall(P,Args).

%% TODO: retract this clause if no ibk predicate is given
prove_aux(p(inv,_P,_A,_Args,Atom,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2):-
    user:ibk(Atom,Body,Path),
    prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

%% use existing abduction
prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    select_lower(P,A,FullSig,Sig1,Sig2),
    member(sub(Name,P,A,Subs,PredTypes),Prog1),
    user:metarule_init(Name,Subs,PredTypes,Atom,Body1,Recursive,[Atom|Path]),
    check_recursion(Recursive, MaxN, Atom, Path),
    prove(Body1,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

%% new abduction
prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2):-
    N1 < MaxN,
    bind_lower(P,A,FullSig,Sig1,Sig2),
    user:metarule(Name,Subs,PredTypes,Atom,Body1,FullSig,Recursive,[Atom|Path]),
    check_recursion(Recursive, MaxN, Atom, Path),
    check_new_metasub(Name,P,A,Subs,Prog1),
    succ(N1,N3),
    prove(Body1,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs,PredTypes)|Prog1],Prog2).

nproveall(Atoms,Sig,Prog):-
    forall(member(Atom,Atoms), \+ deduce_atom(Atom,Sig,Prog)).

make_atoms(Atoms1,Atoms2):-
    maplist(atom_to_list,Atoms1,Atoms3),
    maplist(make_atom,Atoms3,Atoms2).

make_atom([P|Args],p(inv,P,A,Args,[P|Args],[])):-
    length(Args,A).

check_functional(Atoms,Sig,Prog):-
    (get_option(functional) ->
        forall(member(Atom1,Atoms),
        \+ (
            make_atom(Atom2,Atom1),
            user:func_test(Atom2,TestAtom2,Condition),
            make_atom(TestAtom2,TestAtom1),
            deduce_atom(TestAtom1,Sig,Prog),
            call(Condition)));
        true).

%% if not recursive continue as normal
check_recursion(false, _MaxN, _Atom, _Path).
%% if recursive then check that we are allowed at least one two clauses (we need a base and inductive step) check_recursion(false, _MaxN, _Atom, _Path).
check_recursion(true, MaxN, Atom, Path):-
    MaxN \== 1,
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
    memberchk(sub(Name,P,A,_,_),Prog),!,
    last(Subs,X),
    when(ground(X),\+memberchk(sub(Name,P,A,Subs,_),Prog)).
check_new_metasub(_Name,_P,_A,_Subs,_Prog).

check_prim_exists(P/A):-
    functor(Atom,P,A),
    current_predicate(_,user:Atom),!.
check_prim_exists(P/A):-
    retractall(user:prim(P/A)),
    length(Args,A),!,
    retractall(user:primcall(P,Args)).

setup:-
    forall(user:prim(P/A),check_prim_exists(P/A)).

iterator(N):-
    get_option(min_clauses(MinN)),
    get_option(max_clauses(MaxN)),
    between(MinN,MaxN,N).

target_predicate([p(inv,P,A,_Args,_Atom,[])|_],P/A).

%% target_predicates(Atoms, Preds2):-
%%     findall(P/A, member([p(inv,P,A,_Args,_Atom,[])],Atoms), Preds1),
%%     list_to_set(Preds1,Preds2).

invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]):-
    NumSymbols is MaxClauses-1,
    get_option(max_inv_preds(MaxInvPreds)),
    M is min(NumSymbols,MaxInvPreds),
    findall(sym(InvSym,_Artiy,_Used),(between(1,M,I),atomic_list_concat([P,'_',I],InvSym)),Sig).

pprint(Prog1):-
    reverse(Prog1,Prog3),
    maplist(metasub_to_clause,Prog3,Prog2),
    maplist(pprint_clause,Prog2).

pprint_clause(C):-
    numbervars(C,0,_),
    format('~q.~n',[C]).

metasub_to_clause(sub(Name,_,_,Subs,_),Clause2):-
    user:metarule_init(Name,Subs,_,HeadList,BodyAsList1,_,_),
    add_path_to_body(BodyAsList3,_,BodyAsList1,_),
    filter(no_ordering,BodyAsList3,BodyAsList2),
    maplist(atom_to_list,ClauseAsList,[HeadList|BodyAsList2]),
    list_to_clause(ClauseAsList,Clause1),
    (Clause1 = (H,T) -> Clause2=(H:-T); Clause2=Clause1).


%% metasub_to_clause_list(sub(Name,_,_,Subs,_),[HeadList|BodyAsList2]):-
%%     user:metarule_init(Name,Subs,_,HeadList,BodyAsList1,_,_),
%%     add_path_to_body(BodyAsList3,_,BodyAsList1,_),
%%     filter(no_ordering,BodyAsList3,BodyAsList2).

no_ordering(H):-
    H\='@'(_).

list_to_clause([Atom],Atom):-!.
list_to_clause([Atom|T1],(Atom,T2)):-!,
    list_to_clause(T1,T2).

atom_to_list(Atom,AtomList):-
    Atom =..AtomList.

get_option(Option):-
    call(Option), !.

get_option(Option):-
    default(Option).

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
    length(Args,A),
    add_path_to_body(Atoms,Path,Rest,Out).

gen_metarule_id(Name,Name):-
    ground(Name),!.
gen_metarule_id(_Name,IdNext):-
    get_option(metarule_next_id(Id)),
    succ(Id,IdNext),
    set_option(metarule_next_id(IdNext)).

%% =========== NEED TO REFACTOR


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

%% learns a sequence of programs and asserts each program that it learns
learn_seq(Seq,Prog):-
    maplist(learn_task,Seq,Progs),
    flatten(Progs,Prog).

learn_task(Pos/Neg,Prog1):-
    learn(Pos,Neg,Prog1),!,
    maplist(metasub_to_clause,Prog1,Prog2),
    forall(member(Clause,Prog2),assert(user:Clause)),
    findall(P/A,(member(sub(_Name,P,A,_Subs,_PredTypes),Prog1)),Prims),!,
    list_to_set(Prims,PrimSet),
    maplist(writeln,PrimSet),
    maplist(assert_prim,PrimSet).
learn_task(_,[]).

filter(_F,[],[]).
filter(F,[H|T],[H|Out]):-
    call(F,H),!,
    filter(F,T,Out).
filter(F,[_H|T],Out):-
    filter(F,T,Out).

%% UGLY UNFOLDING CODE

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