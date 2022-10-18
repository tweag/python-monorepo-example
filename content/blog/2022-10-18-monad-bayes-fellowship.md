---
title: "Improving the Probabilistic Programming Language monad-bayes"
shortTitle: "Improving monad-bayes"
author: Reuben Cohn-Gordon
description: "My experience improving monad-bayes, the probabilistic programming language package, as a Tweag fellow."
tags: [data-science, fellowship, haskell, statistics, MCMC]
---

I've spent some time this summer extending [Monad-Bayes][monad-bayes], a
probabilistic programming library in Haskell. This was done in the context of a
Tweag open source fellowship, under the supervision of Dominic Steinitz. These
improvements are part of the upcoming 1.0 release of the library.

[monad-bayes]: https://github.com/tweag/monad-bayes

Briefly, what's Monad-Bayes about? Like [Church][church], [Gen][gen] and
Pyro[pyro] (among other probabilistic programming languages), it allows a user
to specify a Bayesian model as a program, and automates the process of
inferring the specified distribution. **Unlike** other languages, it is a
first-class library of its host language (Haskell), rather than an embedded
language. This means that any language feature in the host language is
available to you. It also means that you can have distributions over absolutely
anything. For instance, you can visualize a distribution by transforming it
into a distribution over histograms or even over plots, and then just sampling
one!

[church]: https://en.wikipedia.org/wiki/Church_(programming_language)
[gen]: https://www.gen.dev/
[pyro]: https://pyro.ai/

## What's new?

What's new in version 1.0 of Monad-Bayes? This post outlines the changes, and
then some of the things I learned from the fellowship.

### Website and documentation

I used Hakyll to make a [website][website] for Monad-Bayes, including
[tutorials][tutorials], [examples][examples] and
[documentation][documentation].

[website]: https://monad-bayes-site.netlify.app/_site/about.html
[tutorials]: https://monad-bayes-site.netlify.app/_site/tutorials.html
[examples]: https://monad-bayes-site.netlify.app/_site/examples.html
[documentation]: https://monad-bayes-site.netlify.app/_site/examples.html

Fairly thorough documentation of both the API and the architecture is now
available. The documentation is stored in the same repository as the library,
so contributions are welcome.

### Choose your own RNG

Monad-Bayes was previously bound to the [mwc][mwc] random number generator, but
now the sampler is parametric in the choice of RNG (although a default from the
[random][random] package is provided). Dominic did this.

[mwc]: https://hackage.haskell.org/package/mwc-random
[random]: https://hackage.haskell.org/package/random

### Terminal Based API

You can now run MCMC with an improved API that ensures a non-zero initial
state, and shows a progress bar. This is accessible in the
`Control.Monad.Bayes.Inference.TUI` module.

### Integrator

Inference by numerical integration is now possible, which you can express for
arbitrary probabilistic programs using the continuation monad, as described
nicely [here][numerical-integration]. The code borrows from this
implementation, with the author's permission.

[numerical-integration]: https://jtobin.io/giry-monad-implementation

For now, its purpose is mostly didactic, since it's impractical to numerically
calculate the normalizing constant of large probabilistic programs. But used as
a part of a more complex inference method, it may well be quite useful in
future. For example, one might want to integrate out discrete variables from a
model.

### Lazy sampling

Standard samplers are not lazy, and hang if you try to sample e.g. an infinite
list. The [LazyPPL][lazyppl] language has a clever implementation of a lazy
sampler, and Monad-Bayes now supports this interpretation of probabilistic
programs. This means you can sample an entire infinite list. Some accompanying
inference methods built on top of this sampler are also provided, also taken
more or less wholesale from LazyPPL.

[lazyppl]: https://lazyppl.bitbucket.io/

### Example gallery

The repo now hosts a variety of [Jupyter notebooks][notebooks], which can be
run using Nix. This allows them to rely on a much wider range of packages than
the library requires to install, so it's where I showcase uses of Monad-Bayes
that rely on more heavy-weight libraries.

[notebooks]: https://github.com/tweag/monad-bayes/tree/master/notebooks

These include physics simulations, probabilistic uses of lenses (randomly
updating JSON), probabilistic parser combinators (currently very simple, but an
exciting area to explore), comonads with probability for Ising models,
probabilistically generating `diagrams`, and more.

### Tutorials

A second set of nix-runnable notebooks are tutorials, inspired by the similar
ones for Gen, another probabilistic programming language. These show how to use
all of the inference methods available in Monad-Bayes, and visualize the
results. They are available in the [website][website].

### Streaming

Monadic streaming libraries interact very naturally with probability: a
probabilistic stream is a random walk, which is useful for defining Markov
Chain Monte Carlo, as well as a variety of models. To avoid heavy dependencies,
`streamly` is not present in the library, but `pipes` is. This is accessible
through the `mcmcP` function.

Future work here could use streamly for parallel chains (this worked when
testing on an experimental branch).

### Histograms and plotting

I use histogram-fill to easily form plots from weighted samples. I use hvega to
easily plot those histograms in notebooks. These are available in notebooks.

### Clean up

- consistent naming conventions
- core inference methods `smc` and `mcmc` now take config objects, so that its easier to work out what their parameters are
- removed or fixed broken models
- removed unused helper functions
- added many tests
- removed broken badges on the site

## Future work

Three future directions are particularly interesting to me:

- Implement something similar to [Reactive Probabilistic
  Programming](https://arxiv.org/abs/1908.07563), using a Haskell functional
  reactive programming library like
  [dunai](https://github.com/turion/dunai-bayes). This is now underway and is
  extremely cool.

- Implement Hamiltonian Monte Carlo, using Ed Kmett's `ad` package. This is
  somewhat underway (https://github.com/tweag/monad-bayes/issues/144)

- Use the continuation monad transformer to implement numerical integration on
  top of other `MonadSample` instances, to allow marginalization of only
  certain variables in a probabilistic program. Particularly for marginalizing
  out discrete variables when doing HMC.

## What I learned

First and foremost, how Monad-Bayes works under the hood. It's a very beautiful
library. At its core is the idea that there are many representations of the
probability monad, which can be built compositionally. Inference consists of
transformations within and between these representations. The developer guide
part of the documentation explains this in depth.

It's also a library that uses some advanced concepts. In fact, Monad-Bayes is a
pretty excellent tour of a range of more advanced level Haskell constructions,
from continuations and coroutines to the (Church transformed!) free monad
transformer.

I also learned a little about the limitations of Haskell. Some of the tracking
that Gen does with execution traces is difficult in Haskell because of the
static types, although one can always use `Dynamic` or similar to circumvent
this (or fancy dependent types). Performance-wise, I had the familiar
experience that most of the times, things were fast enough, but that
engineering for performance would have required me to learn more about
Haskell's compiler and runtime, and how to use a profiler. So not impossible,
but too much of a commitment for a summer.

Finally, thanks to everyone at Tweag for their help, especially Dominic
Steinitz and Matthias Meschede.
