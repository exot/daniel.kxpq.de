The Lisp Language Family has begotten some of the most interesting programming languages that are
currently around.  Three of them are

  1. [Common Lisp][CL], though quite old, still offers a lot innovative concepts most other
  languages don't have.

  2. [Guile Scheme][guile], the GNU Scripting Language, offers a very modern dialect of
  [Scheme][scheme] with a lot of additional libraries, macros, delimited continuations and an
  interface to C.

  3. [Clojure][clojure], a dialect of Lisp running on the JVM with built in support for lazy
  sequences.

[CL]: http://en.wikipedia.org/wiki/Common_Lisp "Common Lisp"
[guile]: http://www.gnu.org/s/guile/ "Guile Scheme"
[scheme]: http://www.schemers.org "Scheme in general"
[clojure]: http://www.clojure.org "Clojure"


## Thoughts on Weblocks

This webpage is made with weblocks (yay!) and here are some random thoughts about it.

* If you make a children of a widget dirty, then the widget itself is not marked dirty.  This is of
  course the right thing to do, because otherwise you could just mark the root widget as dirty.
  However, it might also may be problematic if the widget must be repainted, see poems.  How to
  avoid this?
