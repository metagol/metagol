Metagol is an inductive logic programming (ILP) system based on meta-interpretive learning. Please contact Andrew Cropper (andrew.cropper@cs.ox.ac.uk) with any questions / bugs. If you use Metagol for research, please use [this citation](https://raw.githubusercontent.com/metagol/metagol/master/metagol.bib) or cite the relevant paper.

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

#### Metagol settings


Metagol searches for a hypothesis using iterative deepening on the number of clauses in the solution. You can specify a maximum number of clauses:

```prolog
metagol:max_clauses(Integer). % default 10
```

The following flag denotes whether the learned program should be unfolded to remove unnecessary invented predicates:

```prolog
metagol:unfold_program. % default false
```

For instance, with the flag set to false, Metagol would learn this great-grandmother theory:

```prolog
ggmother(A,B):-mother(A,C),ggmother_1(C,B).
ggmother_1(A,B):-mother(A,C),mother(C,B).
```

With the flag set to true, Metagol would learn this great-grandmother theory:

```prolog
ggparent(A,B):-mother(A,C),mother(C,D),mother(D,B).
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
