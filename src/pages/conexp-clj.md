This is (a [clone](http://www.math.tu-dresden.de/~borch/conexp-clj) of) the homepage of conexp-clj,
a [Clojure][]-rewrite of [Conexp][].

[Clojure]: http://www.clojure.org (Clojure)
[Conexp]: http://conexp.sf.net (Concept Explorer)

**WARNING**: This software is in alpha stage. Expect the unexpected while using it.

## Sources

can be found [here][conexp-clj-src]. Note that you need [Leiningen][lein] to use them properly.

[conexp-clj-src]: http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=summary
  (conexp-clj source code)
[lein]: http://github.com/technomancy/leiningen
  (leiningen, a bulding tool for clojure)

## Binary Development Snapshot

 * [Last tagged release](http://www.math.tu-dresden.de/~borch/conexp-clj/conexp-clj.zip) (0.0.6-alpha)
 * [Current snapshot](http://www.math.tu-dresden.de/~borch/conexp-clj/conexp-clj-SNAPSHOT.zip)

Note that the snapshot release might have some incompatible changes to the last tagged one.

To install conexp-clj, download the zip-file you want, unzip and follow the instructions in the
INSTALL file. To start just call the appropriate script under conexp-clj/bin, i.e. for Linux this is

    conexp-clj/bin/conexp-clj.sh

and if you want to start the gui (which is experimental and not yet finished by now (when will a gui
be finished at all?)), call the script with `--gui`:

    conexp-clj/bin/conexp-clj.sh --gui

## Features

 * Basic Operations on Formal Contexts
 * Relational Algebra with Formal Contexts
 * Transparent IO for Formal Contexts (in development)
 * Scaling for Many-Valued Contexts
 * Implicational Theory and Basic Attribute Exploration
 * NextClosure (of course)
 * Computing Luxenburger-Bases and Iceberg Concept Sets
 * IO for Many-Valued Contexts
 * Lattice Layouts and Lattice IO (some...)
 * A bit of Fuzzy-FCA
 * Interface for sage

## Documentation

By now, documentation is done via example files. Besides the special examples files given below,
there exist some example files which give a general overview of how to use conexp-clj. Those files
cover

 * [basics](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/01-basics.clj)
 * [formal contexts](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/02-contexts.clj)
 * [lattices](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/03-lattices.clj)
 * [IO](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/04-io.clj)
 * [implications](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/05-implications.clj)

They have been written by Sebastian Böhm.

Additionally, for general help on a function f, you can use the clojure function `doc` with

    (doc f)

For finding functions you may find useful, you can use `find-doc`

    (find-doc "Whatever you may find useful")

## Further Examples

 * [Attribute Exploration](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/exploration.clj),
   a demonstration how attribute exploration can be done in conexp-clj.
 * [Fuzzy FCA](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/fuzzy.clj), a sample file to
   show how to use fuzzy FCA with conexp-clj
 * [Factor Analysis](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/factor-analysis.clj),
   a small program that demonstrates how to use conexp-clj for factorizing contexts
 * [Formal Contexts for Implications](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/implication-closure.clj),
   computing a context for a set of implications
 * A
   [Formal Context of Functions](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/function-context.clj),
   see the paper by Artem Revenko and Sergej Kuznetzov for the CLA2010
 * [Permutations as Formal Context](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/permutation-context.clj),
   computes a context whose concept lattice is isomorphic to the lattice of permutations on the set
   \{0,...,n\}.
 * [Tamari Lattice](http://www.math.tu-dresden.de/~borch/conexp-clj/examples/tamari-lattice.clj),
   the lattice of all bracketings of n+1 symbols (a.k.a. the Tamari Lattice of parameter n)

## To come

 * More Tests and Documentation
 * More Context IO Formats (all supported by FCAStone)
 * GUI (maybe)

