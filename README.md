Metagol is an inductive logic programming (ILP) system based on meta-interpretive learning. Please contact Andrew Cropper (andrew.cropper@gmail.com) with any questions / bugs. If you use Metagol for research, please use [this citation](https://raw.githubusercontent.com/metagol/metagol/master/metagol.bib) or cite the relevant paper.

#### Using Metagol

Metagol is written in Prolog and runs with both Yap and SWI. The following code demonstrates learning the grandparent relation given the mother and father relations as background knowledge:

```prolog
:- use_module('metagol').

%% first-order background knowledge
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).

%% predicates that can be used in the learning
prim(mother/2).
prim(father/2).

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% learning task
a :-
  %% positive examples
  Pos = [
    grandparent(ann,amelia),
    grandparent(steve,amelia),
    grandparent(ann,spongebob),
    grandparent(steve,spongebob),
    grandparent(linda,amelia)
  ],
  %% negative examples
  Neg = [
    grandparent(amy,amelia)
  ],
  learn(Pos,Neg).

```
Running the above program will print the output:

```prolog
% clauses: 1
% clauses: 2
% clauses: 3
grandparent(A,B):-grandparent_1(A,C),grandparent_1(C,B).
grandparent_1(A,B):-mother(A,B).
grandparent_1(A,B):-father(A,B).
```

where the predicate `grandparent_1/2` is invented and corresponds to the parent relation.

#### Metarules

Metagol requires higher-order metarules to define the form of clauses permitted in a hypothesis. An example metarule is:

```prolog
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
```

In this metarule, known as the chain metarule, the symbols `P`, `Q`, and `R` denote existentially quantified higher-order variables, and the symbols `A`, `B`, and `C` denote universally quantified first-order variables. The list of symbols in the first argument denote the existentially quantified variables which Metagol will attempt to find substitutions for during the learning.

Users need to supply Metarules. We are working on automatically identifying the necessary metarules, with preliminary work detailed in the paper:

* A. Cropper and S.H. Muggleton. [Logical minimisation of meta-rules within meta-interpretive learning](http://andrewcropper.com/pubs/ilp14-minmeta.pdf). In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.

Here are more metarules:

```prolog
metarule([P,Q],([P,A,B]:-[[Q,A,B]])). % identity
metarule([P,Q],([P,A,B]:-[[Q,B,A]])). % inverse
metarule([P,Q,X],([P,A,B]:-[[Q,A,B,X]])). % curry
metarule([P,Q,R],([P,A,B]:-[[Q,A],[R,A,B]])). % precon
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,B]])). % postcon
```

#### Recursion

The above metarules are all non-recursive. By contrast, this metarule is recursive:

```prolog
metarule([P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
```
Recursive metarules can lead to infinite search spaces. Metagol comes with an inbuilt loop detection. However, should you wish to guarantee termination, you can define a total ordering over the terms:

```prolog
metarule([P,Q],([P,A,B]:-[[Q,A,C],@term_gt(A,C),[P,C,B],@term_gt(C,B)])).
```

The atoms `@term_gt(A,C)` and `@term_gt(C,B)` must be supplied by the user. For instance, suppose you are learning robot strategies for a robot in a one-dimensional space where each term is a world state (a list of Prolog facts). Then the following ordering ensures that the robot always moves at least one place to the right. Because the space is finite, termination is guaranteed:

```prolog
term_gt(A,B):-
  member(robot_position(APos),A),
  member(robot_position(BPos),B),
  APos < BPos.
```
For more examples of learning with recursion see the find-duplicate, graph-reachability, kinship2, strings2, and strings3 examples.

<!-- TODO Interpreted BK -->

<!-- #### Sequential learning

To learn a sequence of tasks use the following command.

```prolog
T1 = [
  parent(ann,andy),
  parent(steve,andy),
  parent(ann,amy)]/[],
T2 = [
  grandparent(ann,amelia),
  grandparent(steve,amelia)
  ]/[],
learn_seq([T1,T2],H),
pprint(H).
```

In this approach, the solution to parent task (including its constituent predicates) is added to the background knowledge so that it can be used to solve the grandparent task. -->

#### Metagol settings

The following settings are all optional.

Metagol searches for a hypothesis using iterative deepening on the number of clauses in the solution. The starting depth can be adjusted as follows:

```prolog
metagol:min_clauses(Integer). % default 1
```
You can specify a maximum number of clauses:

```prolog
metagol:max_clauses(Integer). % default 10
```

You can specify a maximum number of invented predicates:

```prolog
metagol:max_inv_preds(Integer). % default 10
```

The following flag denotes whether the learned theory should be functional:

```prolog
metagol:functional. % default false
```
If the functional flag is enabled, then the must define a func_test predicate. An example func test is:

```prolog
func_test(Atom,PS,G):-
  Atom = [P,A,B],
  Actual = [P,A,Z],
  \+ (metagol:prove_deduce([Actual],PS,G),Z \= B).
```

This func test is used in the robot examples. Here, the `Atom` variable is formed of a predicate symbol `P` and two states `A` and `B`, which represent initial and final state pairs respectively.  The func_test checks whether the learned hypothesis can be applied to the initial state to reach any state `Z` other that the expected final state `B`. For more examples of functional tests, see the robots.pl, sorter.pl, and strings2.pl files.

<!-- By default, Metagol hides orderings when printing solutions. You can override this using the following flag.

```prolog
metagol:show_orderings. % default false
``` -->


<!-- ```prolog
metagol:limit_recursion. % default false
```

(TODO) THIS IS NOT YET WORKING IN THE BETA VERSION

```prolog
metagol:fold_theory. % default false
```

(TODO) THIS IS NOT YET WORKING IN THE BETA VERSION -->
