# Maintaining Understandability

It has been argued before that succinct code is easier to maintain than longer code, both of which
accomplish the same thing.  This has quite rational, since less code means less work, especially
reading and writing.  And it is quite obvious that the human mind is not equipped with an infinite
memory of characters.

However, this all ends when some extreme cases come into play.  If the code is too succinct, or too
abstract, than it is still true that the programmer does not need to read that much, but instead she
has to do a lot to *understand* the code.  It may very well be that code that is too succinct fails
on the *pragmatic* side of representing the solution to a problem.  So the aim is not to write
succinct code but to write understandable code, and that's where pragmatism comes in.

We have addressed pragmatism
[before](/essays#on-the-pragmatic-power-of-programming-languages), and in particular found
out that pragmatism is not a property of a programming language but merely can only be
considered in the presence of a certain programmer and a particular problem.
Nevertheless, pragmatism can be related to understandability of code, as we shall see.

## Understandability implies Maintainability, and Vice Versa

Today maintaining code seems as important as programming itself.  The days when ad-hoc programs for
small problems where written, run and then forgotten are over.  Instead, large programs are
developed by companies getting a lot of money for that.  Consequently, the resulting programs are
quite valuable and one does not want to throw them away after using them once for something
whatsoever.  Instead, they are maintained by a variety of programmers, however competent, who try to
fix errors and extend the capabilities wherever needed.

However, since programs are living longer today than they did in the past, the set of programmers
maintaining the code may vary a lot and hence many different programmers change, modify and extend
large programs.  This is a big source of various problems, since programmers are humans, humans make
mistakes and are different in taste.  And they are irrational by nature.  The resulting code may
therefore then look like a big mixup of everything unwanted.

So, principles of proper maintaining have been developed, given programmers guidelines how they
should do their work.  But how can one assume that there even *exist* principles for fixing bugs?
Or principles for writing new features?  Wouldn't that imply that we already know the bug, or at
least the very nature of essentially every bug?  If we would do so, we wouldn't have bugs, which
[might be possible][landoflisp], but not with mainstream programming.  And with new features, things
are even worse, since creativity, in its very nature, cannot be principlized.

[landoflisp]: http://landoflisp.com

So, let's restart from the very beginning.  We have seen that two essential points of maintaining
programs are

* fixing bugs and be certain afterwards that everything else stays all right
* adding new features such that everything else stays all right.

The important part here is to make sure that everything "stays all right" (to the extend needed).
To ensure this, however, it not only seems *helpful* that one understands the code, but it merely is
*absolutely necessary* that the code is understandable to the working programmer.  Hence the first
(and maybe most important) principle of writing maintainable code is

>  Write understandable code.

This is obvious, far too obvious to be helpful at all.  However, we can still work with that.

One observation is the one from the very start of this text.  If the code is too long to fit in the
memory of the programmer (i.e. her head), this very programmer will have a hard time to understand
even something.  Does that mean that big programs cannot be maintained?  No, it doesn't.  It only
means that if the *conceptual parts* of the programs are too large, then the program is very
unlikely that it can be maintained properly, not to say efficiently, or even at all.  It's the set
of conceptual elements of the program that fills up the head of the progammer, not the set of
characters.  Hence we have to keep the conceptual parts of the program small, and we need to use
abstraction the make some concepts of the program that are not needed in other parts unnecessary
(after all, that's what abstraction has been invented for). So does that mean we have to write code
that is as succinct and abstract as possible, to keep the overall number of concepts as low as
possible?

Not quite.  If you overdo it, possibly throwing in some more abstractions than the original problem
had (like "design patterns"), you may kill understandability right before you write your program.
If the conceptual parts of your program, however small, are too complicated by themselve, you may
lose as well.  And we don't want to lose, at least not in this game.

So, what to do?  We have to find a good balance between conceptual granularity and conceptual
simplicity of our program.  This is hard.  However, if we have *understood the problem* the program
tries to solve, then we already have quite a good granulation and simplicity, namely that of our
non-program solution.  And an obvious idea would now be just to transform this non-program solution
into a program solution, keeping the conceptual structure of it.  And that's where pragmatism comes
into play.

## Pragmatism implies Understandability

Roughly speaking, pragmatism is the ability of a programmer to implement her solution of a
particular problem in a particular programming language.  Hence pragmatism cannot be addressed to a
progamming language itself.  However, we can address it with some slight modifications to
programming languages and certain *problem domains*.  This is because in a certain problem domain,
say numerical maths, weather forecast, expert systems and so on, programmers learned what they know
from various but often similar sources like textbooks or other experts.  It is hence quite likely
that they will use similar *language constructions* in their problem formulations and solutions.
Likewise, the problems they want to solve are similar, or at least have the similar targets, and
hence one could say that pragmatism of a certain programming language, for a certain programmer and
a certain problem *most likely* carries over to programmers working in the same problem domain on a
similar problem of this domain.  Hence we can talk about the fact, that a certain programming
language is *pragmatic for a certain problem domain*, just when its way of expressing things fits
quite well in the regular language that problem domain uses.

Examples for pragmatism of programming languages in certain problem domains are

* Fortran for numerical programming, at least some time ago,
* Lisp for Artifical Intelligence, at least some time ago,
* Haskell for Theoretical Computer Science,
* C for system programming,
* Perl for hacky scripting,
* Python for clean hacking,
* APL for greek-like numeric-golfing

and so on.  Of course, you can apply all of the above languages, and any other, to different
problems.  But then you may find that its quite hard to solve a certain problem nicely, unless you
have discovered a new and elegant way of solving problems in that domain.  Those things are great,
but not common.  Hence trying new things that are normally considered a bad idea turn sometimes out
to be good, so one should not say never.  But don't be upset if solving systems of differential
equations turns out to look ugly in PHP.

So let us assume that we have chosen a programming language that is pragmatic for our problem domain
at hand.  What does that buy us for maintainability?  Well, if the implementation of the problem
maps the theoretical solution of the problem we want to solve onto the implementation, then what
does that mean for another programmer to understand the code?  It means, to a large extend, that
understanding the theoretical solution implies understanding its implementation.  And this is good,
very good.  And transitively this means that understanding the theoretical solution of your problem
implies the possiblity to maintain your code.  Fantastic!  And even more: A programmer coming from
the same problem domain as the original programmer will *most likely* understand the solution to the
problem, just because she comes from the very same problem domain!  Great!  So all problems are
solved if we can transfer our theoretical solution onto an implementation in a one-to-one fashion!?

Not quite.  That's the point when math stops and when reality starts.  Maintainability is not the
only point one likes to consider when writing code.  Others are that programs should be fast,
portabale, small, whatever.  And that might come into your way when transforming your solution from
theory into practise.  And another point comes into play: what if you do not *have* a solution at
all, but merely develop it right in the same process as implementing it?  This is the very
characterisation of exploratory programming, and it doesn't fit in here.  Or does it?

When you explore your solution while implementing it, you do something very strange: You use the
implementation language as the *specification language* of your solution, i.e. you think and specify
your solution in your implementation language.  Only very few programming languages are able to do
that (not Java), and even then only in specific problem domains.  However, in that case, if the
programmer still uses the pragmatic concepts of her problem domain, the pragmatism is as large as it
can be, since the translation from solution to implementation is the identity mapping.  Nothing
happened!  And if you then use some kind of literal programming to make points clear that are not
quite clear from the code itself, you have won.  Congratulations!

This does not happend very often, and it expects the programmer to be a geek.  This is not normal,
more exceptional.  Most often, you will find some bug-ridden specification of a solution and an even
worse implementation of nothing specific.  This is not helpful, and the programs turning up are
likely to be the worse human kind has ever seen.  Beware of them, they are not the product of
aspiration for clarity but of aspiration for coffee breaks.

And don't believe that extensive commenting eases the understandability of code.  This does only
work in small and mostly academic examples.  Maintaining the synchronicity between code and comments
is as difficult as maintaining the synchronicity between intention and implementation.  And that you
have to do anyway, so don't burden yourself with another source of work, especially if it is
unnecesary.  And if you ever find yourself thinking again

> What a poor code one has that has no comments.

then think of Brecht's Galilei and his answer to a similar question:

> What a poor code one has that at all needs comments.
