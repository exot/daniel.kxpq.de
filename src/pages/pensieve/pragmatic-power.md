# On the Pragmatic Power of Programming Languages

Sometimes we hear statements like "the programming language X is more powerful than the programming
language Y" and most often we (at least seem to) understand this statement in the particular
circumstances.  But what does this mean at all?  Aren't all (serious) programming languages equally
powerful, at least in the mathematical sense, since they are Turing complete?  And aren't there even
programming languages that are not Turing complete but regarded as powerful, as for example SQL?  So
what do we mean when we talk about the "power of a programming language"?  And if such a thing
exists, how can it be that people use programming languages less powerful than others?

## Problems, Programmers and Pragmatic Power

For this to understand we look at the very first steps of programming on a very abstract level.
Suppose that we have given a problem P we want to solve with a computer, and we have a programming
language X to do this.  Then the programmer has to translate the problem specification (at its
solution) into the world of programs expressible with X.  That might be easy or not, depending on
both the problem and the programming language.  However, it might be easier with another programming
language Y, and in this case we may say that X is more powerful than Y (for this problem, and for
this programmer).

So let us loosely define the *pragmatic power*.  Suppose a programming language X (without defining
what actually a programming language is), a certain problem P and a certain programmer R.  Then the
pragmatic power of X corresponds to the amount of work needed by R to formulate P (and its solution)
in the language X.

This "definition" is by far formal, and it may be that it cannot be formalized, because it describes
an inherently subjective matter -- the way programmers think.

What might be even more important is the observation that the pragmatic power of X depends on both
the *problem* and the *programmer*.  We may eliminate this dependency by saying that X is more
powerful than Y if it is for more problems and more programmers, but this is at least vague.  And it
practically ensures that the pragmatic power of programming languages may not be comparable, since
we cannot judge all possible programmers and imagine all possible problems they want to solve.  What
we can see from this is a first imagination why there exist different languages at all.

So let us imagine a certain programmer on a certain *problem domain*.  An example might be an
operating system developer, a mathematician or a typist.  They all have different problems to solve,
and they might find different programming languages suitable for their tasks at hand.  The operating
system developer might want to use C and Assembler because he has to modify very low level parts of
the computer.  The mathematician might or might not, and may prefer programming languages with more
maths and abstraction, like Common Lisp.  And the typist may even use other languages for daily
computations, like Excel.  Yet all of them may be excellent in what they are doing, their demands on
the programming languages differe widely.

## The Success of Programming Languages

This shows that different programming languages may be better suited for different problem domains,
and legitimates their existence.  In the same manner we can use the concept of pragmatic power to
examine another effect in the history of programming language -- *success*.

There are some programming languages which have been designed thoroughly, with great effort and a
lot of money.  Ada is a good example for this.  Java may be.  And still they might be practically
irrelevant today.  Whether Ada is a good example for *this* may be true or not, there are pepole who
use it, willingly or not.  On the other hand there exist programming languages which have been
developed by single persons, or at least very small groups of persons.  Examples for this are C and
Perl (and many others).  They seem to be mainstream today.  How this?  Are big commitees more stupid
than single persons?

Well, experience may tell us this is right, but in our case a more detailed description is possible.
While programming commitees aim at designing a language the commitee members (or their employers)
are satisfyable with, single person programming language designers have only one aim for their
programming language: maximum pragmatic power for their daily use.  This was the case for Dennis
Ritchie when he developed C, Larry Wall while writing Perl, and even John McCarthy while developing
the first formalism for functions on computers, which later turned into the first Lisp.  And while
programming languages that may be average for all commitee members may also be average for most of
other programmers, a perfect language for a single person may be perfect for others, if only some.
And perfection for some is far better than average for all.

And average programmer may use other average programming languages from day to day, and some may be
more mainstream today and may be irrelevant tomorrow.  If I am to judge this, COBOL is a good
example for this.  On the other hand a programming language that suite perfectly for some
programmers may be used by them a very long time, and they may produce very good software with them,
bringing their whole community further, attracting other programmers, making programming fun.
That's how success starts with programming languages, and how it has started for successfull
programming languages today like C, C++, Python, Perl, Ruby and many others.  And this is very fine
this way.

And even another thing can be oberserved for successful programming languages -- they are much more
robust against the trends in mainstream programming.  The main example here is Lisp, which has
survived not because of much money and a small, very special purpose community (like Fortran).  It
has survived mostly because of its large pragmatic power for a lot of problem domains and for a lot
of programmers.  With that it was able to survive the AI winter in the eighties and the decline in
the ninties, and gains more and more attraction today, with a vast number of dialects around (Common
Lisp, Scheme, Clojure, Arc, ...)

Note that programming languages designed by single persons may become programming languages designed
by (quite large) groups of persons, and even commitees.  This happened to Java, Perl, Python and
others.  But this does not necessarily mean that they become average programming language (at least
as long not company is behind that blindly wants to make money with this language).  This is because
the groups that design the language originated from the communities formed around thoses languages,
and these people often share a great deal of common conceptions on how languages should look like.
Moreover, so called the original developers remain as *benevolant dictators* in the communities and
ensure that the language stays a language they like.  However, they may do this not from the
standpoint of personal practical needs anymore but from the standpoint of personal taste.  That may
be good, that may be bad, the future shall show.  But what can be seen today is that the evolution
of progamming languages has gotten very close to the evolution of natural languages in this
particular case: it evolves with the needs of their speakers.

## The End

So, pragmatic power of programming languages is highly subjective and context dependent.  We all
knew that before reading this essay.  But maybe we still tend to say that "X is dump, because it is
not powerful enough" and may judge others by their use thereof.  Programming languages involes the
personal taste of programmers, i.e. of humans, and judging taste is out of scope for everyone.  The
only thing we may see is whether someone uses a language designed to please money or a language
designed to reach personal perfection.
