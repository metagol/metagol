# Metagol (ALPHA)

THIS IS STILL AN ALPHA VERSION AND IT NOT YET READY FOR USE

Metagol is an inductive logic programming (ILP) system based on the meta-interpretive learning framework (MIL).  For more details about MIL, see the following papers.

* S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. Meta-interpretive learning: application to grammatical inference. Machine Learning, 94:25-49, 2014.

* S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited. Machine Learning, 100(1):49-73, 2015.

Metagol is primarily maintained by Andrew Cropper (a.cropper13@imperial.ac.uk), Martin Möhrmann (martisch@uni-osnabrueck.de), and Stephen Muggleton (s.muggleton@imperial.ac.uk).

Metagol is free for academic use. If you intend to use it for commercial purposes then contact one of the maintainers above.

## Using Metagol

Metagol is written in Prolog and runs with both Yap and, albeit slower, SWI Prolog. Metagol is contained in a single file called 'metagol.pl'. To use Metagol, load the metagol.pl module into your Prolog compiler. The following code demonstrates using Metagol to learn the great grandparent relation.

```prolog

%% LOAD METAGOL
:- use_module('metagol').

%% FIRST-ORDER BACKGROUND KNOWLEDGE
parent(elizabeth_second,prince_charles).
parent(prince_philip,prince_charles).
parent(prince_charles,prince_william).
parent(prince_charles,prince_harry).
parent(princess_diana,prince_william).
parent(princess_diana,prince_harry).
parent(prince_william,prince_george).

%% PREDICATES TO BE USED IN THE LEARNING
prim(parent/2).

%% METARULES
metarule([P,Q,R],([P,A,B]:-[Q,A,C],[R,C,B])).

%% LEARNING
x :-
  Pos = [
    great_grandparent(elizabeth_second,prince_george)
    great_grandparent(prince_philip,prince_george)],
  Neg = [great_grandparent(prince_charles,prince_william)],
  learn(great_grandparent,Pos,Neg,H),
  pprint(H).
```
Running the above program will print the following output.

```prolog
% clauses: 1 invented predicates: 0
% clauses: 2 invented predicates: 0
% clauses: 2 invented predicates: 1
great_grandparent(A,B) :- great_grandparent_1(A,C), parent(C,B).
great_grandparent_1(A,B) :- parent(A,C), parent(C,B).
```

In this solution, the predicate `great_grandparent_1/2` is invented. See the aforementioned papers for details on Metagol's predicate invention.

## Metarules

Metagol requires that the user provides a set of second-order metarules, a form of language bias which defines the form of clauses permitted in a hypothesis, similar to mode declarations used in Progol, Aleph, etc. An example metarule is as follows:

```prolog
metarule([P,Q,R],([P,A,B]:-[Q,A,C],[R,C,B])). % P(A,B)<- Q(A,C), R(C,B).
```

In this metarule, called the chain metarule, the symbols P, Q, and R denote existentially quantified second-order variables, and the symbols A, B, and C denote universally quantified first-order variables. All metarules follow the following pattern:

```prolog
metarule(Metasubs,(Body :- Head)).
```

The `Metasubs` list denotes the existential variables in a metarule. All other variables are assumed to be universally quantified.

Currently, the metarules are supplied by the user. We are working on automatically identifying the necessary metarules, and preliminary work is detailed in the following paper:

* A. Cropper and S.H. Muggleton. Logical minimisation of meta-rules within meta-interpretive learning. In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.

Here are more metarules:

```prolog
metarule([P,Q],([P,A,B]:-[Q,A,B])). % identity
metarule([P,Q],([P,A,B]:-[Q,B,A])). % inverse
metarule([P,Q,X],([P,A,B]:-[Q,A,B,X])). % curry
metarule([P,Q,R],([P,A,B]:-[Q,A],[R,A,B])). % precon
metarule([P,Q,R],([P,A,B]:-[Q,A,B],[R,B])). % postcon
```

The above metarules are all non-recursive.  By contrast, the following metarule is recursive.

```prolog
% P(A,B) <- Q(A,C), A>C, P(C,B),C>B.
metarule([P,Q],([P,A,B]:-[Q,A,C],obj_gt(A,C),[P,C,B],obj_gt(C,B))).
```

Here, the atoms `obj_gt(A,C)` and `obj_gt(C,B)` are used to define a total order over the terms. This is necessary to guarantee termination of the meta-interpreter. For example, suppose you are learning robot strategies for a robot in a one-dimensional space and each term is a state (a list of Prolog facts). The following ordering ensures that the robot always moves at least one place to the right. Because the space is finite, termination is guaranteed.

```prolog
obj_gt(A,B):-
  member(robot_position(APos),A),
  member(robot_position(BPos),B),
  APos < BPos.
```

## Dependent learning

(todo)

## Metagol settings

The following settings are all optional.

Metagol searches for a hypothesis using iterative deepening on the number of clauses in the solution. The starting depth can be adjusted using the following setting.

```prolog
metagol:min_clauses(Integer). % default 1
```

The user can also specify a max solution length, as follows.

```prolog
metagol:max_clauses(Integer). % default 6
```

The following flag denotes whether the learned theory should be functional.

```prolog
metagol:functional. % default false
```

```prolog
metagol:limit_recursion. % default false
```

```prolog
metagol:fold_theory. % default false
```



## Further details

For more information on Metagol and the MIL framework, see the following papers:

* A. Cropper and S.H. Muggleton. Learning efficient logical robot strategies involving composable objects. In Proceedings of the 24th International Joint Conference Artificial Intelligence (IJCAI 2015), pages 3423-3429. IJCAI, 2015.

* A. Cropper and S.H. Muggleton. Logical minimisation of meta-rules within meta-interpretive learning. In Proceedings of the 24th International Conference on Inductive Logic Programming, pages 65-78. Springer-Verlag, 2015. LNAI 9046.

* A. Cropper and S.H. Muggleton. Can predicate invention compensate for incomplete background knowledge?. In Proceedings of the 13th Scandinavian Conference on Artificial Intelligence, pages 27-36. IOS Press, 2015.

* S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad. Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited. Machine Learning, 100(1):49-73, 2015.

* D. Lin, E. Dechter, K. Ellis, J.B. Tenenbaum, and S.H. Muggleton. Bias reformulation for one-shot function induction. In Proceedings of the 23rd European Conference on Artificial Intelligence (ECAI 2014), pages 525-530, Amsterdam, 2014. IOS Press.

* S.H. Muggleton, D. Lin, J. Chen, and A. Tamaddoni-Nezhad. Metabayes: Bayesian meta-interpretative learning using higher-order stochastic refinement. In Gerson Zaverucha, Vitor Santos Costa, and Aline Marins Paes, editors, Proceedings of the 23rd International Conference on Inductive Logic Programming (ILP 2013), pages 1-17, Berlin, 2014. Springer-Verlag. LNAI 8812.

* S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad. Meta-interpretive learning: application to grammatical inference. Machine Learning, 94:25-49, 2014.

*  S.H. Muggleton and D. Lin. Meta-interpretive learning of higher-order dyadic datalog: Predicate invention revisited. In Proceedings of the 23rd International Joint Conference Artificial Intelligence (IJCAI 2013), pages 1551-1557, 2013.