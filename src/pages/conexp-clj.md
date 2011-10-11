## conexp-clj

`conexp-clj` is an attempt to provide a general purpose tool for
[Formal Concept Analysis](/math/fca).  As an original attempt to extend [ConExp][] with new
functionalities it is written completely from scratch in [Clojure][].

[Clojure]: http://www.clojure.org (Clojure)
[ConExp]: http://conexp.sf.net (Concept Explorer)

**WARNING**: This software is in alpha stage. Expect the unexpected while using it.

* * *

### News

* 2011-10-02
  Moved the website of `conexp-clj` from the math account to kxpq.de and restructured it.

* * *

### Development

[Sources][conexp-clj-src] are available online, licensed under the Eclipse Public License -v 1.0,
same as Clojure.  Note that you need [Leiningen][lein] for development.

[conexp-clj-src]: http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=summary
  (conexp-clj source code)
[lein]: http://github.com/technomancy/leiningen
  (leiningen, a bulding tool for clojure)

Besides that, binary snapshots are available, namely

 * [Last tagged release](http://www.math.tu-dresden.de/~borch/conexp-clj/conexp-clj.zip) (0.0.6-alpha)
 * [Current snapshot](http://www.math.tu-dresden.de/~borch/conexp-clj/conexp-clj-SNAPSHOT.zip)

Note that the snapshot release might have some incompatible changes to the last tagged one.

### Installation

To install conexp-clj, download the zip-file you want, unzip and follow the instructions in the
INSTALL file. To start just call the appropriate script under conexp-clj/bin, i.e. for Linux this is

    conexp-clj/bin/conexp-clj.sh

and if you want to start the gui (which is experimental and not yet finished by now (when will a gui
be finished at all?)), call the script with `--gui`:

    conexp-clj/bin/conexp-clj.sh --gui

### Features

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

### Documentation

By now, documentation is done via example files. Besides the special examples files given below,
there exist some example files which give a general overview of how to use conexp-clj. Those files
cover

 * [basics](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;f=doc/examples/01-basics.clj;hb=master)
 * [formal contexts](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;f=doc/examples/02-contexts.clj;hb=master)
 * [lattices](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;f=doc/examples/03-lattices.clj;hb=master)
 * [IO](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;f=doc/examples/04-io.clj;hb=master)
 * [implications](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;f=doc/examples/05-implications.clj;hb=master)

They have been written by Sebastian Böhm.

Additionally, for general help on a function f, you can use the clojure function `doc` with

    (doc f)

For finding functions you may find useful, you can use `find-doc`

    (find-doc "Whatever you may find useful")

Some more examples:

 * [Attribute Exploration](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/exploration.clj)
   a demonstration how attribute exploration can be done in conexp-clj.
 * [Fuzzy FCA](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/fuzzy.clj),
   a sample file to show how to use fuzzy FCA with conexp-clj
 * [Factor Analysis](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/factor-analysis.clj),
   a small program that demonstrates how to use conexp-clj for factorizing contexts
 * [Formal Contexts for Implications](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/implication-closure.clj),
   computing a context for a set of implications
 * A
   [Formal Context of Functions](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/function-context.clj),
   see the paper by Artem Revenko and Sergej Kuznetzov for the CLA2010
 * [Permutations as Formal Context](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/permutation-context.clj),
   computes a context whose concept lattice is isomorphic to the lattice of permutations on the set
   \{0,...,n\}.
 * [Tamari Lattice](http://www.math.tu-dresden.de/extern/cgi-bin/algebra/borch/gitweb.cgi?p=clojure-conexp.git;a=blob;hb=master;f=doc/examples/tamari-lattice.clj),
   the lattice of all bracketings of n+1 symbols (a.k.a. the Tamari Lattice of parameter n)
