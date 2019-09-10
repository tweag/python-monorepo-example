---
title: Why Haskell <br/>is important
author: Mark Karpov
tags: haskell
---

People keep asking me, “What's the point of Haskell, why do you folks even
use it?”. How can you answer this? Most of us Haskell practitioners would
start talking about strong static types, the elegance of abstractions, and
ease of refactoring—but we would be missing the bigger picture. That's
because we are programmers, and programmers typically think about the
technical details.

Let's take a step back.

We can view lots of things as black boxes that do stuff. Deep learning, for
example, allows us to solve certain problems we couldn't tackle before.
Another set of technologies helped us put a man on the moon. No one is going
to ask “What's the point of deep learning?” or “What's the point of a
spaceship?”. That's obvious.

At this point, one could balk: “Mark, but you're comparing apples to
oranges.”. Really? Let's see. Deep learning is an approach to solve a
problem. What about programming languages? They're an approach to solve a
problem too: they allow us to write programs. But programs can be different.
Some programs are complicated and very difficult to write in some languages,
say assembly. It can also be challenging in some languages to convince
ourselves that they are correct programs, say when effects are unrestricted
or invariants are not protected. By viewing programming languages as black
boxes, we could say that some of those unlock creation of more and more
complex high-quality software. And in that respect, programming languages
are not so different from deep learning or space travel—they allow us to do
something we could not do before.

Let's forget about what makes Haskell what it is. Let's view it as a black
box and see what we can make of it. It's a thing that comes [with some
risks][the-types-got-you] and seeing just its output, in most cases we
really can't say if it does something different than other languages do. It
doesn't allow us to make a quantum leap. You could write your software in
Haskell or in some other popular language X—other things being equal, if X
is anything pertinent to your problem domain, you'll do fine if you have
access to engineers with substantial experience in X.

Still I must say that it's essential for us to continue writing Haskell.
Why?

Richard Eisenberg predicted, at ZuriHac 2019, that we might not
be using Haskell itself in the distant future. Perhaps a different language—but it will be built
on the valuable discoveries we made with Haskell. I'm confident that his
prediction will come true because:

* Haskell is a unique vehicle for bringing modern programming language
  theory (PLT) to a production-quality programming language (†) where
  results of such research can be tried out very quickly by programmers
  tasked to solve real problems every day. For almost every other language,
  one of the following is true:

    * it is an experimental language with a small developer base that sees
      virtually no practical usage
    * it is a practical language that's widely used but not advancing very
      quickly, burdened by its huge user base and legacy code.

  I think there's lots of value in the linear and dependent types research
  happening now. Not because those features allow us to solve harder
  problems the first day they land in GHC (the main Haskell compiler). They
  probably won't. But, they can allow us to start trying to use the features
  in practice, which is the necessary condition if we want to build
  tomorrow's super language.

* Haskell is not a proprietary language built, controlled, or subject to the
  whims of some commercial entity. It is instead open source software, a
  product of a collective effort by academics and enthusiastic users.
  Haskell's tight, active community and its spirit of experimentation are
  perfect for moving forward with more exploration.

* Haskell evolves fast. I'm a relatively new user who started using it in
  2014, but even over just 5 years, I can say that the language has changed
  quite a bit and a lot of new features appeared in GHC. Veteran Haskellers
  who have been using it for a decade or more can attest that many things
  have changed for the better.

* Haskell thrives with ideas. Not only the compiler itself, but also its
  ecosystem of libraries—Haskellers are in a constant search for better
  solutions. Some of these solutions prove to be elegant and powerful, so
  they stick. Others are soon forgotten. Of course, the same thing happens
  in other languages, but what is so unique about Haskell is that the
  language itself acts as a foundation with unique qualities which make its
  users lean to more principled, although not-so-obvious solutions.

  So what, why explore those solutions? Is it worth it?

  The numerous techniques and concepts of functional programming that
  mainstream languages adopt and, without doubt, will keep adopting validate
  that Haskell continues to move in the right direction. It has heavily
  influenced design of newer languages such as Rust, and its influence is
  destined to continue. There are plenty of blog posts and articles praising
  strong static typing and its value for making programmers more productive
  nowadays. Haskell’s core principles, like purity and isolation of
  side-effects, are understood and appreciated by more today than ever before.

Skeptical? That's fine. Just remember that the idea for neural networks is
from the [1950s][perceptron]. It took quite some time for us to get the
computational power and data to make that idea useful. Haskell started
without a way to perform side effects but evolved to be adopted by
cutting-edge players like Facebook and Google to do real work today. Time
will only bring more great things—I don’t see an end to Haskell’s evolution.

----

(†) If you do not believe that a lot of companies use Haskell, you may find
that you are a victim of the same type of outdated world view as the one
described by Hans Rosling in his book [_Factfulness_][factfulness]. The
world is changing and it's changing quickly. What was true for Haskell ten
years ago is not true anymore. It's fine to still poke jokes about
unemployed Haskellers, almost by inertia, but here are some facts to
consider: we're hiring Haskell developers all the time, and I know quite a
few other companies that do the same. I, for one, have been a Haskeller for
my entire professional career and write in other languages only
occasionally. Am I so unique or the world is changing?

[the-types-got-you]: https://www.tweag.io/posts/2019-02-13-types-got-you.html
[perceptron]: https://en.wikipedia.org/wiki/Perceptron
[factfulness]: https://www.goodreads.com/book/show/34890015-factfulness
