check_same(Prog1,Prog2):-
    Prog1 == Prog2,!,
    pprint(Prog2).

check_same(Prog1,Prog2):-
    format('expected: ~w\n',[Prog1]),
    format('actual: ~w\n',[Prog2]),
    false.

load_example(Name):-
    writeln(Name),

    %% (current_predicate(metagol:max_clauses/1) -> retractall(metagol:max_clauses(_)); true),
    retractall(user:body_pred(_)),
    retractall(user:head_pred(_)),
    retractall(metagol:ibk(_,_,_)),
    retractall(metagol:type(_,_)),
    retractall(metagol:body_pred_call(_,_)),
    %% retractall(metagol:ibk_body_pred_call(_,_)),
    consult(Name),
    metagol:set_option(metarule_next_id(0)).

test(Name,Pos,Neg,Prog1):-
    load_example(Name),
    learn(Pos,Neg,Prog2),!,
    (check_same(Prog1,Prog2) -> unload_file(Name); (format('FAILED: ~w\n', [Name]),false)).

test(Name,_Pos,_Neg,_Prog1):-
    format('FAILED: ~w\n', [Name]),
    false.

test_adjred:-
    Name='adjacent-to-red',
    Pos = [target(b),target(c)],
    Neg = [target(a),target(d),target(e)],
    Prog = [
    sub(1,target_1,1,[target_1,colour,red]),
    sub(1,target,1,[target,edge,target_1])
    ],
    test(Name,Pos,Neg,Prog).

test_constants1:-
    Name='constants1',
    Pos = [p(1,2),p(1,3),p(1,4),p(1,1),p(2,2),p(4,4)],
    Neg = [p(2,4),p(3,4),p(3,1)],
    Prog = [
    sub(1,p,2,[p,4]),
    sub(2,p,2,[p,2]),
    sub(1,p,2,[p,1])
    ],
    test(Name,Pos,Neg,Prog).

test_constants2:-
    Name='constants2',
    Pos = [q(1,2),q(1,3),q(1,4),q(1,1),q(2,2),q(4,4)],
    Neg = [q(2,4),q(3,4),q(3,1)],
    Prog = [
    sub(1,q,2,[q,num,4]),
    sub(2,q,2,[q,num,2]),
    sub(1,q,2,[q,num,1])
    ],
    test(Name,Pos,Neg,Prog).

test_constants3:-
    Name='constants3',
    Pos = [f(andy,laura),f(andy,amelia)],
    Neg = [],
    Prog = [
    sub(1,f,2,[f,p,andy,patrick]),
    sub(1,f,2,[f,p,andy,spongebob])
    ],
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
    Prog = [
    sub(chain,f_1,2,[f_1,tail,element]),
    sub(dident,f,2,[f,head,f_1]),
    sub(tailrec,f,2,[f,tail])
    ],
    test(Name,Pos,Neg,Prog).

test_grandparent:-
    Name='grandparent',
    Pos = [
        target(i,b),
        target(i,c),
        target(a,d),
        target(a,e),
        target(a,f),
        target(a,g),
        target(c,h)
    ],
    Neg = [
        target(a,b),
        target(b,c),
        target(c,d),
        target(d,e),
        target(e,f),
        target(f,g),
        target(g,h),
        target(h,i)
    ],
    Prog=[
    sub(1,target_1,2,[target_1,father]),
    sub(1,target_1,2,[target_1,mother]),
    sub(2,target,2,[target,target_1,target_1])],
    test(Name,Pos,Neg,Prog).

test_graph_colouring:-
    Name='graph-colouring',
    Pos = [
    target(e)
    ],
    Neg = [
    target(a),
    target(b),
    target(c),
    target(d),
    target(f)
    ],
    Prog=[
    sub(2,target_1,2,[target_1,colour,colour]),
    sub(1,target,1,[target,edge,target_1])
    ],
    test(Name,Pos,Neg,Prog).

test_graph_connectedness:-
    Name='graph-connectedness',
    Pos = [
    target(a,b),
    target(b,c),
    target(c,d),
    target(b,a),
    target(a,c),
    target(a,d),
    target(a,a),
    target(b,d),
    target(b,a),
    target(b,b)
      ],
      Neg = [
      ],
    Prog=[sub(tailrec,target,2,[target,edge]),sub(ident,target,2,[target,edge])],
    test(Name,Pos,Neg,Prog).

test_graph_reachability:-
    Name='graph-reachability',
    Pos = [p(a, b), p(a, c), p(a, a)],
    Prog=[sub(2,p,2,[p,edge]),sub(1,p,2,[p,edge])],
    test(Name,Pos,[],Prog).

test_ho1:-
    Name='higher-order1',
    Pos = [
        f([1,2,3],[3,4,5]),
        f([10,12,33,3,2,1],[12,14,35,5,4,3])
    ],
    Neg = [],
    Prog = [sub(3,f_1,2,[f_1,my_succ,my_succ]),sub(2,f,2,[f,map,f_1])],
    test(Name,Pos,Neg,Prog).

test_ho2:-
    Name='higher-order2',
    Pos = [f([[a],[a,a],[a,a,a],[a,a,a,a]],[3,5,7,9])],
    Neg = [],
    Prog = [sub(2,f_2,2,[f_2,my_double,my_succ]),sub(2,f_1,2,[f_1,my_length,f_2]),sub(1,f,2,[f,map,f_1])],
    test(Name,Pos,Neg,Prog).

test_ho3:-
    Name='higher-order3',
    Pos = [f([1,2,3,4,5,6,7,8,9,10],[2,4,5,6,8,10])],
    Neg = [],
    Prog = [sub(2,f_1,1,[f_1,mydiv,5]),sub(2,f_1,1,[f_1,mydiv,2]),sub(3,f,2,[f,filter,f_1])],
    test(Name,Pos,Neg,Prog).

test_ibk1:-
    Name='ibk1',
    Pos = [f([1,2,3],[5,6,7])],
    Neg = [],
    Prog = [
    sub(chain,f_2,2,[f_2,my_succ,my_succ]),
    sub(chain,f_1,2,[f_1,f_2,f_2]),
    sub(curry,f,2,[f,map,f_1])],
    test(Name,Pos,Neg,Prog).

test_kinship1a:-
    Name='kinship1',
    Pos = [grandparent(ann,amelia),grandparent(steve,amelia),grandparent(ann,spongebob),grandparent(steve,spongebob),grandparent(linda,amelia)],
    Neg = [grandparent(amy,amelia)],
    Prog = [
    sub(1,grandparent_1,2,[grandparent_1,father]),
    sub(1,grandparent_1,2,[grandparent_1,mother]),
    sub(2,grandparent,2,[grandparent,grandparent_1,grandparent_1])],
    test(Name,Pos,Neg,Prog).

test_kinship1b:-
    Name='kinship1',
    load_example(Name),
    Pos = [grandparent(ann,amelia)],
    Neg = [grandparent(ann,amelia)],
    not(learn(Pos,Neg)),
    unload_file(Name).

test_lessthan:-
    Name='less-than',
    Pos = [
    target(1,3),
    target(2,5),
    target(3,7),
    target(4,10),
    target(5,9),
    target(6,8),
    target(7,9),
    target(8,10),
    target(9,10)
    ],
    Neg = [
    target(3,1),
    target(7,1),
    target(2,2),
    target(8,2),
    target(4,3),
    target(9,3),
    target(4,0),
    target(10,4),
    target(5,5),
    target(6,5)
    ],
    Prog=[
    sub(chain,target_2,2,[target_2,succ,succ]),
    sub(chain,target_1,2,[target_1,succ,target_2]),
    sub(ident,target,2,[target,target_1]),
    sub(ident,target_1,2,[target_1,succ]),
    sub(chain,target,2,[target,target_1,target_1])],
    test(Name,Pos,Neg,Prog).

test_member:-
    Name = 'member',
    Pos = [
    target(4,[4,3,2,1]),
    target(3,[4,3,2,1]),
    target(2,[4,3,2,1]),
    target(1,[4,3,2,1]),
    target(3,[3,2,1]),
    target(2,[3,2,1]),
    target(1,[3,2,1]),
    target(2,[2,1]),
    target(1,[2,1]),
    target(1,[1])
  ],
  Neg = [
    target(5,[4,3,2,1]),
    target(6,[4,3,2,1]),
    target(7,[4,3,2,1]),
    target(8,[4,3,2,1]),
    target(4,[3,2,1]),
    target(5,[3,2,1]),
    target(6,[3,2,1]),
    target(3,[2,1]),
    target(4,[2,1]),
    target(2,[1])
  ],
  Prog = [sub(2,target,2,[target,cons]),sub(1,target,2,[target,value])],
  test(Name,Pos,Neg,Prog).

test_mutual_recursion:-
    Name = 'mutual-recursion',
    Pos = [even(10),even(8),even(6),even(4),even(2)],
    Neg = [even(3)],
    Prog=[
    sub(base,even,1,[even,0]),
    sub(mutual,even_1,1,[even_1,s,even]),
    sub(mutual,even,1,[even,s,even_1])],
    test(Name,Pos,Neg,Prog).

test_prec:-
    Name='predecessor',
Pos = [
    target(1,0),
    target(2,1),
    target(3,2),
    target(4,3),
    target(5,4),
    target(6,5),
    target(7,6),
    target(8,7),
    target(9,8),
    target(10,9)
  ],
  Neg = [
    target(1,3),
    target(1,7),
    target(2,2),
    target(2,8),
    target(3,1),
    target(3,9),
    target(4,0),
    target(4,10),
    target(5,5),
    target(5,6)
  ],
    Prog = [sub(inverse,target,2,[target,succ])],
    test(Name,Pos,Neg,Prog).



test_robotsa:-
    Name='robots',
    Pos = [f(world((1/1),(1/1),false),world((3/3),(3/3),false))],
    Prog = [
    sub(chain,f_3,2,[f_3,move_right,move_forwards]),
    sub(chain,f_2,2,[f_2,f_3,f_3]),
    sub(chain,f_1,2,[f_1,f_2,drop_ball]),
    sub(chain,f,2,[f,grab_ball,f_1])],
    test(Name,Pos,[],Prog).

test_robotsb:-
    Name='robots',
    Pos = [f(world((1/1),(1/1),false),world((5/5),(5/5),false))],
    Prog = [
    sub(chain,f_4,2,[f_4,move_right,move_forwards]),
    sub(chain,f_3,2,[f_3,f_4,f_4]),
    sub(chain,f_2,2,[f_2,f_3,f_3]),
    sub(chain,f_1,2,[f_1,f_2,drop_ball]),
    sub(chain,f,2,[f,grab_ball,f_1])],
    test(Name,Pos,[],Prog).

test_sequential1:-
    Name = 'sequential',
    load_example(Name),

    T1 = [
    parent(ann,andy),
    parent(steve,andy),
    parent(ann,amy),
    parent(ann,andy)
  ]/[],

  T2 = [
    grandparent(steve,amelia),
    grandparent(ann,amelia),
    grandparent(linda,amelia),
    grandparent(ann,spongebob)
  ]/[],

  T3 = [
    great_grandparent(ann,sally),
    great_grandparent(steve,sally)
  ]/[],

      learn_seq([T1,T2,T3],Prog),
      Prog=[
      sub(ident,parent,2,[parent,father]),
      sub(ident,parent,2,[parent,mother]),
      sub(chain,grandparent,2,[grandparent,parent,parent]),
      sub(chain,great_grandparent,2,[great_grandparent,parent,grandparent])],
      unload_file(Name).



test_strings1:-
    Name='strings1',
    Pos = [
    f(['a','b','c']/['a','a','b','b','c','c'],_/[]),
    f(['a','a','c']/['a','a','a','a','c','c'],_/[]),
    f(['a','c']/['a','a','c','c'],_/[])
    ],
    Prog = [sub(1,f,2,[f,skip1]),sub(6,f,2,[f,skip1]),sub(6,f,2,[f,copy1])],
    test(Name,Pos,[],Prog).

test_strings2:-
    Name='strings2',
    Pos = [
    f(['a','b','c']/['a','b','c','d'],_/[]),
    f(['a','a','c']/['a','a','c','d'],_/[]),
    f(['a','c']/['a','c','d'],_/[])
    ],
    Prog = [sub(6,f_2,2,[f_2,write1,d]),sub(2,f,2,[f,empty,f_2]),sub(5,f_1,2,[f_1,copy1,skip1]),sub(7,f,2,[f,f_1])],
    test(Name,Pos,[],Prog).

test_strings3:-
    Name='strings3',
    Pos = [
    f(['a','b','c']/['a','b','c','d'],_/[]),
    f(['a','a','c']/['a','a','c','d'],_/[]),
    f(['a','c']/['a','c','d'],_/[])
    ],
    Prog = [sub(6,f_1,2,[f_1,write1,d]),sub(2,f,2,[f,empty,f_1]),sub(5,f_1,2,[f_1,copy1,skip1]),sub(7,f,2,[f,f_1])],
    test(Name,Pos,[],Prog).

test_trains :-
    Name='trains',
    Pos = [e(east1),e(east2),e(east3),e(east4),e(east5)],
    Neg = [e(west6),e(west7),e(west8),e(west9),e(west10)],
    Prog = [
    sub(2,e_1,1,[e_1,short,closed]),
    sub(3,e,1,[e,has_car,e_1])],
    test(Name,Pos,Neg,Prog).

%%
t :-
    test_adjred,
    test_constants1,
    test_constants2,
    test_constants3,
    test_finddup,
    test_grandparent,
    test_graph_colouring,
    test_graph_connectedness,
    test_graph_reachability,
    test_ho1,
    test_ho2,
    test_ho3,
    test_ibk1,
    test_kinship1a,
    test_kinship1b,
    test_lessthan,
    test_member,
    test_mutual_recursion,
    test_prec,
    test_robotsa,
    test_robotsb,
    test_strings1,
    test_strings2,
    test_strings3,
    test_trains,
    %% THIS TEST MUST GO AT THE END
    test_sequential1,
    writeln('TESTS PASSED').

:-
    time(t),halt.