Metagol is an inductive logic programming (ILP) system based on meta-interpretive learning.
Please contact Andrew Cropper (andrew.cropper@cs.ox.ac.uk) with any questions / bugs.
If you use Metagol for research, please use [this citation](https://raw.githubusercontent.com/metagol/metagol/master/metagol.bib) and cite the relevant paper.

#### Using Metagol

Metagol is written in Prolog and runs with SWI-Prolog.

The following code demonstrates learning the grandparent relation given the mother and father relations as background knowledge:

```prolog
:- use_module('metagol').

%% metagol settings
body_pred(mother/2).
body_pred(father/2).

%% background knowledge
mother(ann,amy).
mother(ann,andy).
mother(amy,amelia).
mother(linda,gavin).
father(steve,amy).
father(steve,andy).
father(gavin,amelia).
father(andy,spongebob).

%% metarules
metarule([P,Q],[P,A,B],[[Q,A,B]]).
metarule([P,Q,R],[P,A,B],[[Q,A,B],[R,A,B]]).
metarule([P,Q,R],[P,A,B],[[Q,A,C],[R,C,B]]).

%% learning task
:-
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
In this program the predicate symbol `grandparent_1` is invented (i.e. does not appear in the background knowledge nor in the examples).


#### Metarules

Metagol requires metarules as input.
Metarules define the form of clauses permitted in a hypothesis and thus the search space.

Metarules are of the form `metarule(Name, Subs, Head, Body)`. An example metarule is:

```prolog
metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
```

Here the symbols `P`, `Q`, and `R` denote second-order variables (i.e. can unify with predicate symbols).
The symbols `A`, `B`, and `C` denote first-order variables (i.e. can unify with constant symbols).
Metagol will search for appropriate substitutions for the variables in the second argument of a metarule (*Subs*).

Users must supply metarules.
Deciding which metarules to use is still an open (and hard!) problem.
Preliminary work in this area is detailed in papers:

* A. Cropper and S. Tourret: [Derivation reduction of metarules in meta-interpretive learning](http://andrewcropper.com/pubs/ilp18-dreduce.pdf). ILP 2018.

* A. Cropper and S.H. Muggleton: [Logical minimisation of meta-rules within meta-interpretive learning](http://andrewcropper.com/pubs/ilp14-minmeta.pdf). ILP 2014.

For learning dyadic programs without constant symbols, we recommend using these metarules:

```prolog
metarule([P,Q], [P,A,B], [[Q,A,B]]). % identity
metarule([P,Q], [P,A,B], [[Q,B,A]]). % inverse
metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]). % precon
metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,B]]). % postcon
metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]). % chain
```

Please look at the examples to see how metarules are used.

#### Recursion

The above metarules are all non-recursive.
By contrast, this metarule is recursive:

```prolog
metarule([P,Q], [P,A,B], [[Q,A,C],[P,C,B]]).
```

See the find-duplicate, sorter, member, and string examples.


#### Interpreted background knowledge

Metagol supports interpreted background knowledge (IBK), which was initially introduced in this paper:

* A. Cropper and S.H. Muggleton: [Learning Higher-Order Logic Programs through Abstraction and Invention](http://andrewcropper.com/pubs/ijcai16-metafunc.pdf). IJCAI 2016.

IBK is usually used to learn higher-order programs.
For instance, one can define the `map/3` construct as follows:

```prolog
ibk([map,[],[],_],[]).
ibk([map,[A|As],[B|Bs],F],[[F,A,B],[map,As,Bs,F]]).
```

Given this IBK, Metagol will try to prove it through meta-interpretation, and will also try to learn a sub-program for the atom `[F,A,B]`.

See the droplast, higher-order, and ibk examples.

#### Metagol settings

You can specify a maximum timeout (in seconds) for Metagol as follows:

```prolog
metagol:timeout(600). % default 10 minutes
```

Metagol searches for a hypothesis using iterative deepening on the number of clauses in the solution.
You can specify a maximum number of clauses:

```prolog
metagol:max_clauses(Integer). % default 10
```

The following flag denotes whether the learned theory should be functional:

```prolog
metagol:functional. % default false
```
If the functional flag is enabled, then you must define a func_test predicate. An example func test is:

```prolog
func_test(Atom1,Atom2,Condition):-
  Atom1 = [P,A,X],
  Atom2 = [P,A,Y],
  Condition = (X=Y).
```

This func test is used in the robot examples.
This test checks that if Metagol can prove any two Atoms `Atom1` and `Atom2` from an induced program, then the condition `X=Y` always holds.
For more examples of functional tests, see the robots.pl, sorter.pl, and strings2.pl files.


### Publications

Here are some publications on MIL and Metagol.

#### Key papers

* A. Cropper and S.H. Muggleton: [Learning efficient logic programs](http://andrewcropper.com/pubs/mlj18-metaopt.pdf). Machine learning 2018.

* A. Cropper and S.H. Muggleton: [Learning Higher-Order Logic Programs through Abstraction and Invention](http://andrewcropper.com/pubs/ijcai16-metafunc.pdf). IJCAI 2016.

* S.H. Muggleton, D. Lin, and A. Tamaddoni-Nezhad: [Meta-interpretive learning of higher-order dyadic datalog: predicate invention revisited](https://link.springer.com/article/10.1007/s10994-014-5471-y). Machine Learning 2015.

* S.H. Muggleton, D. Lin, N. Pahlavi, and A. Tamaddoni-Nezhad: [Meta-interpretive learning: application to grammatical inference](https://link.springer.com/article/10.1007/s10994-013-5358-3). Machine Learning 2014.

#### Theory and Implementation

* R. Morel, A. Cropper, and L. Ong. [Typed meta-interpretive learning of logic programs](http://andrewcropper.com/pubs/jelia19-typed.pdf). JELIA 2019.

* A. Cropper and S. Tourret: [Derivation Reduction of Metarules in Meta-interpretive Learning](http://andrewcropper.com/pubs/ilp18-dreduce.pdf). ILP 2018.

* A. Cropper and S.H. Muggleton: [Learning Efficient Logical Robot Strategies Involving Composable Objects](http://andrewcropper.com/pubs/ijcai15-metagolo.pdf). IJCAI 2015.

* A. Cropper and S.H. Muggleton: [Logical Minimisation of Meta-Rules Within Meta-Interpretive Learning](http://andrewcropper.com/pubs/ilp14-minmeta.pdf). ILP 2014.

* S.H. Muggleton, D. Lin, Jianzhong Chen, and A. Tamaddoni-Nezhad: MetaBayes: Bayesian Meta-Interpretative Learning Using Higher-Order Stochastic Refinement. ILP 2013.

#### Applications / other

* A.Cropper: [Playgol: learning programs through play](http://andrewcropper.com/pubs/ijcai19-playgol.pdf). IJCAI 2019.

* S.H. Muggleton, U. Schmid, C. Zeller, A. Tamaddoni-Nezhad, and T.R. Besold: Ultra-Strong Machine Learning: comprehensibility of programs learned with ILP. Machine Learning 2018.

* S. Muggleton, W-Z. Dai, C. Sammut, A. Tamaddoni-Nezhad, J. Wen, and Z-H. Zhou:
Meta-Interpretive Learning from noisy images. Machine Learning 2018.

* M. Siebers and U. Schmid: [Was the Year 2000 a Leap Year? Step-Wise Narrowing Theories with Metagol](https://link.springer.com/chapter/10.1007/978-3-319-99960-9_9). ILP 2018.

* A. Cropper, A. Tamaddoni-Nezhad, and S.H. Muggleton: [Meta-Interpretive Learning of Data Transformation Programs](http://andrewcropper.com/pubs/ilp15-datacurate.pdf). ILP 2015.

* A. Cropper and S.H. Muggleton: [Can predicate invention compensate for incomplete background knowledge?](http://andrewcropper.com/pubs/scai15-incomplete.pdf) SCAI 2015.

* D. Lin, E. Dechter, K. Ellis, J.B. Tenenbaum, and S.H. Muggleton: Bias reformulation for one-shot function induction. ECAI 2014.

#### Theses

* A. Cropper: [Efficiently learning efficient programs](http://andrewcropper.com/pubs/phd-thesis.pdf). Imperial College London, UK 2017

* D. Lin: Logic programs as declarative and procedural bias in inductive logic programming. Imperial College London, UK 2013
