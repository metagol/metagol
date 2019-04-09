check_same(Prog1,Prog2):-
    Prog1 == Prog2,!,
    pprint(Prog2).

check_same(Prog1,Prog2):-
    format('expected: ~w\n',[Prog1]),
    format('actual: ~w\n',[Prog2]),
    false.

test(Name,Pos,Neg,Prog1):-
    writeln('testing:'-Name),
    consult(Name),
    metagol:set_option(metarule_next_id(0)),
    learn(Pos,Neg,Prog2),!,
    (check_same(Prog1,Prog2) -> unload_file(Name); (format('FAILED: ~w\n', [Name]),false)).

test_adjred:-
    Name='adjacent-to-red',
    Pos = [target(b),target(c)],
    Neg = [target(a),target(d),target(e)],
    Prog = [sub(1,target_1,1,[target_1,colour,red],[prim,prim]),sub(1,target,1,[target,edge,target_1],[prim,inv])],
    test(Name,Pos,Neg,Prog).

test_finddup:-
    Name='find-duplicate',
     Pos = [
        f([1,3,3,4,2,5],3),
        f([6,4,2,5,3,5,1],5),
        f([7,3,4,2,1,5,6,7,8],7),
        f([6,5,7,8,4,2,1,3,7],7),
        f([14,4,13,6,12,1,9,2,10,8,15,5,7,14,3,11],14)
    ],
    Neg = [],
    Prog = [sub(chain,f_1,2,[f_1,tail,element],[prim,prim]),sub(dident,f,2,[f,head,f_1],[prim,inv]),sub(tailrec,f,2,[f,tail],[prim,inv])],
    test(Name,Pos,Neg,Prog).

test_ho1:-
    Name='higher-order1',
    Pos = [f([1,2,3],[2,3,4])],
    Neg = [],
    Prog = [sub(1,f,2,[f,map,my_succ],[inv])],
    test(Name,Pos,Neg,Prog).

test_ho2:-
    Name='higher-order2',
    Pos = [f([[a],[a,a],[a,a,a],[a,a,a,a]],[2,4,6,8])],
    Neg = [],
    Prog = [sub(2,f_1,2,[f_1,my_length,my_double],[prim,prim]),sub(1,f,2,[f,map,f_1],[inv])],
    test(Name,Pos,Neg,Prog).

test_ho3:-
    Name='higher-order3',
    Pos = [f([1,2,3,4,5,6,7,8,9,10],[2,4,5,6,8,10])],
    Neg = [],
    Prog = [sub(1,f_1,1,[f_1,divisible5],[prim]),sub(1,f_1,1,[f_1,divisible2],[prim]),sub(2,f,2,[f,filter,f_1],[inv])],
    test(Name,Pos,Neg,Prog).

test_kinship1a:-
    Name='kinship1',
    Pos = [grandparent(ann,amelia),grandparent(steve,amelia),grandparent(ann,spongebob),grandparent(steve,spongebob),grandparent(linda,amelia)],
    Neg = [grandparent(amy,amelia)],
    Prog = [sub(1,grandparent_1,2,[grandparent_1,father],[prim]),sub(1,grandparent_1,2,[grandparent_1,mother],[prim]),sub(2,grandparent,2,[grandparent,grandparent_1,grandparent_1],[inv,inv])],
    test(Name,Pos,Neg,Prog).

test_kinship1b:-
    Name='kinship1',
    consult(Name),
    Pos = [grandparent(ann,amelia)],
    Neg = [grandparent(ann,amelia)],
    not(learn(Pos,Neg)),
    unload_file(Name).

test_robotsa:-
    Name='robots',
    Pos = [f(world((1/1),(1/1),false),world((3/3),(3/3),false))],
    Prog = [sub(chain,f_3,2,[f_3,move_right,move_forwards],[prim,prim]),sub(chain,f_2,2,[f_2,f_3,f_3],[inv,inv]),sub(chain,f_1,2,[f_1,f_2,drop_ball],[inv,prim]),sub(chain,f,2,[f,grab_ball,f_1],[prim,inv])],
    test(Name,Pos,[],Prog).

test_robotsb:-
    Name='robots',
    Pos = [f(world((1/1),(1/1),false),world((5/5),(5/5),false))],
    Prog = [sub(chain,f_4,2,[f_4,move_right,move_forwards],[prim,prim]),sub(chain,f_3,2,[f_3,f_4,f_4],[inv,inv]),sub(chain,f_2,2,[f_2,f_3,f_3],[inv,inv]),sub(chain,f_1,2,[f_1,f_2,drop_ball],[inv,prim]),sub(chain,f,2,[f,grab_ball,f_1],[prim,inv])],
    test(Name,Pos,[],Prog).


test_trains:-
    Name='trains',
    Pos = [e(east1),e(east2),e(east3),e(east4),e(east5)],
    Neg = [e(west6),e(west7),e(west8),e(west9),e(west10)],
    Prog = [sub(2,e_1,1,[e_1,short,closed],[prim,prim]),sub(3,e,1,[e,has_car,e_1],[prim,inv])],
    test(Name,Pos,Neg,Prog).


%%
t:-
    test_adjred,
    test_finddup,
    test_ho1,
    test_ho2,
    test_ho3,
    test_kinship1a,
    test_kinship1b,
    test_robotsa,
    test_robotsb,
    test_trains,
    writeln('TESTS PASSED'),
    halt.