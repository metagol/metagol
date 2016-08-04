# Metagol

Metagol is an inductive logic programming (ILP) system based on the meta-interpretive learning framework (MIL).  For more information about MIL see the papers listed at the end of this readme file.

The latest release of Metagol can be found here https://github.com/metagol/metagol/releases

Please contact Andrew Cropper (a.cropper13@imperial.ac.uk) with any questions / bugs.

## Using Metagol

Metagol is written in Prolog and runs with both Yap and SWI. Metagol is contained in a single file called 'metagol.pl'. To use Metagol, load the metagol module in your Prolog compiler. The following code demonstrates using Metagol to learn the grandparent relation given mother and father relations.

```prolog
:- use_module('metagol').

%% FIRST-ORDER BACKGROUND KNOWLEDGE
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).

%% PREDICATES TO BE USED IN THE LEARNING.
prim(mother/2).
prim(father/2).

%% METARULES
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% LEARNING TASK
a :-
  Pos = [
    grandparent(ann,amelia),
    grandparent(steve,amelia),
    grandparent(ann,spongebob),
    grandparent(steve,spongebob),
    grandparent(linda,amelia)
  ],
  Neg = [
    grandparent(amy,amelia)
  ],
  learn(Pos,Neg,H),
  pprint(H).

```
Running the above program will print the following output.

```prolog
% clauses: 1
% clauses: 2
% clauses: 3
grandparent(A,B):-grandparent_1(A,C),grandparent_1(C,B).
grandparent_1(A,B):-mother(A,B).
grandparent_1(A,B):-father(A,B).
```

In this solution, the predicate `grandparent_1/2` is invented and corresponds to the parent relation.

## Metarules

Metagol requires that the user provides a set of higher-order metarules, a form of language bias which defines the form of clauses permitted in a hypothesis. An example metarule is as follows:

```prolog
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
```

In this metarule, known as the chain metarule, the symbols `P`, `Q`, and `R` denote existentially quantified higher-order variables, and the symbols `A`, `B`, and `C` denote universally quantified first-order variables. The list of symbols in the first argument denote variables which Metagol will attempt to find substitutions for during the proof of a goal.

Currently, the metarules are supplied by the user. We are working on automatically identifying the necessary metarules, and preliminary work is detailed in the following paper:

* A. Cropper and S.H. Muggleton. [Logical minimisation of meta-rules within meta-interpretive learning](http://www.andrewcropper.com/pubs/ilp2014-minmeta.pdf). In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.

Here are more metarules:

```prolog
metarule([P,Q],([P,A,B]:-[[Q,A,B]])). % identity
metarule([P,Q],([P,A,B]:-[[Q,B,A]])). % inverse
metarule([P,Q,X],([P,A,B]:-[[Q,A,B,X]])). % curry
metarule([P,Q,R],([P,A,B]:-[[Q,A],[R,A,B]])). % precon
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,B]])). % postcon
```

The above metarules are all non-recursive.  By contrast, the following metarule is recursive.

```prolog
% P(A,B) <- Q(A,C), A>C, P(C,B),C>B.
metarule([P,Q],([P,A,B]:-[[Q,A,C],@term_gt(A,C),[P,C,B],@term_gt(C,B)])).
```

The atoms `@term_gt(A,C)` and `@term_gt(C,B)` define a total ordering over the terms, which must be supplied by the user. A total order is necessary to guarantee termination of the meta-interpreter. For example, suppose you are learning robot strategies for a robot in a one-dimensional space and each term is a state (a list of Prolog facts). The following ordering ensures that the robot always moves at least one place to the right. Because the space is finite, termination is guaranteed.

```prolog
term_gt(A,B):-
  member(robot_position(APos),A),
  member(robot_position(BPos),B),
  APos < BPos.
```

For more examples of learning with recursion see kinship2, sorter.pl, and strings2.pl examples..

## Sequential learning

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

In this approach, the solution to parent task (including its constituent predicates) is added to the background knowledge so that it can be used to solve the grandparent task.

## Metagol settings

The following settings are all optional.

Metagol searches for a hypothesis using iterative deepening on the number of clauses in the solution. The starting depth can be adjusted using the following setting.

```prolog
metagol:min_clauses(Integer). % default 1
```
The user can also specify a maximum solution length as follows.

```prolog
metagol:max_clauses(Integer). % default 6
```

The following flag denotes whether the learned theory should be functional.

```prolog
metagol:functional. % default false
```
If the functional flag is enabled, then the must define a func_test predicate. An example func test is as follows.

```prolog
func_test(Atom,PS,G):-
  Atom = [P,A,B],
  Actual = [P,A,Z],
  \+ (metagol:prove_deduce([Actual],PS,G),Z \= B).
```

This func test is used in the robot examples. Here, the `Atom` variable is formed of a predicate symbol `P` and two states `A` and `B`, which represent initial and final state pairs respectively.  The func_test checks whether the learned hypothesis can be applied to the initial state to reach any state `Z` other that the expected final state `B`. For more examples of functional tests, see the robots.pl, sorter.pl, and strings2.pl files.

By default, Metagol hides orderings when printing solutions. You can override this using the following flag.

```prolog
metagol:show_orderings. % default false
```


<!-- ```prolog
metagol:limit_recursion. % default false
```

(TODO) THIS IS NOT YET WORKING IN THE BETA VERSION

```prolog
metagol:fold_theory. % default false
```

(TODO) THIS IS NOT YET WORKING IN THE BETA VERSION -->

## Further details

For more information on Metagol and the MIL framework, see the following papers:

* A. Cropper and S.H. Muggleton. [Learning higher-order logic programs through abstraction and invention](http://www.andrewcropper.com/pubs/ijcai16-metafunc.pdf). In Proceedings of the 25th International Joint Conference Artificial Intelligence (IJCAI 2016), pages 1418-1424. IJCAI, 2016.

* A. Cropper and S.H. Muggleton. [Learning efficient logical robot strategies involving composable objects](http://www.andrewcropper.com/pubs/ijcai15-metagolo.pdf). In Proceedings of the 24th International Joint Conference Artificial Intelligence (IJCAI 2015), pages 3423-3429. IJCAI, 2015.

* A. Cropper and S.H. Muggleton. [Logical minimisation of meta-rules within meta-interpretive learning](http://www.andrewcropper.com/pubs/ilp14-minmeta.pdf). In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.

* A. Cropper and S.H. Muggleton. [Can predicate invention compensate for incomplete background knowledge?](http://www.andrewcropper.com/pubs/scai15-incomplete.pdf). In Proceedings of the 13th Scandinavian Conference on Artificial Intelligence, pages 27-36. IOS Press, 2015.

* S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. [Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited](http://www.doc.ic.ac.uk/~shm/Papers/metagolD_MLJ.pdf). Machine Learning, 100(1):49-73, 2015.

* S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. [Meta-interpretive learning: application to grammatical inference](http://www.doc.ic.ac.uk/~shm/Papers/metagol_gram.pdf). Machine Learning, 94:25-49, 2014.

* D. Lin, E. Dechter, K. Ellis, J.B. Tenenbaum, and S.H. Muggleton. [Bias reformulation for one-shot function induction](http://www.doc.ic.ac.uk/~shm/Papers/metabias.pdf). In Proceedings of the 23rd European Conference on Artificial Intelligence (ECAI 2014), pages 525-530, Amsterdam, 2014. IOS Press.

* S.H. Muggleton, D. Lin, J. Chen, and A. Tamaddoni-Nezhad. [Metabayes: Bayesian meta-interpretative learning using higher-order stochastic refinement](http://www.doc.ic.ac.uk/~shm/Papers/metabayeslong07.pdf). In Gerson Zaverucha, Vitor Santos Costa, and Aline Marins Paes, editors, Proceedings of the 23rd International Conference on Inductive Logic Programming (ILP 2013), pages 1-17, Berlin, 2014. Springer-Verlag. LNAI 8812.

*  S.H. Muggleton and D. Lin. [Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited](http://www.doc.ic.ac.uk/~shm/Papers/metagol_d.pdf). In Proceedings of the 23rd International Joint Conference Artificial Intelligence (IJCAI 2013), pages 1551-1557, 2013.
