---
title: Diversity: the key to success
author: Manuel M T Chakravarty
featured: yes
---

Haskell is an awesome language, but we need to remember that it is not very useful in isolation. In almost any realistic application, Haskell has to coexist with other languages, even if only to call existing C libraries or to make use of operating system services. In actual practice, the more easily we can fit Haskell into existing ecosystems, the more application domains we can unlock.

## Beyond bridging
The core of Haskell’s ability to interoperate, the [ForeignFunctionInterface](https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1490008) language extension, has been available and stable for a long time. However, for all but the simplest interoperability requirements, it tends to be tedious to use. Hence, we have got tools, such as [hsc2hs](https://hackage.haskell.org/package/hsc2hs) and [c2hs](https://hackage.haskell.org/package/c2hs), to automate some of the work of declaring foreign entities and writing marshalling code — this does not just save work, it also prevents many common mistakes.

Over time, we realised that this is not sufficient either. Tools like `hsc2hs` and `c2hs` are typically used to implement, what I like to call, *bridging libraries*. These libraries wrap the API of a foreign library in Haskell, usually by exposing a Haskell API that is close to the original and, occasionally, by providing a more functional, more high-level API. This works fine up to a certain API size. After that —just think about the API surface needed to write Android, iOS, macOS, or Windows apps— the overhead of bridging libraries tends to weigh down and break that bridge:

* The initial implementation is a huge undertaking, which few people, or institutions, are willing to embark on.
* API evolution of the foreign library creates a significant ongoing maintenance burden. Even worse, typically, multiple versions need to be supported simultaneously.
* Documentation becomes a major headache. It is infeasible to transliterate all of the original documentation, but referring Haskell users to the original requires to exactly mirror that original API and demands an understanding of the bridging conventions by the library user.
* Even just the overhead of linking all the bridging code starts to be an issue for large APIs.

Inline foreign code in Haskell sidesteps these issues. The effort to implement an inline library for a foreign language is fixed and supports an arbitrary number of foreign language libraries of arbitrary size without any further overhead. Documentation is naturally just the original and marshalling overhead is in proportion to its use in any single application. Admittedly, a user now needs to know both Haskell and the foreign language, but, given the documentation issue, that was always the case for large APIs.

I have illustrated this in a talk at the 2014 Haskell Symposium, where I introduced [language-c-inline](https://hackage.haskell.org/package/language-c-inline) to use Objective-C code inline in Haskell to code against macOS APIs. You can watch [the talk on YouTube](https://www.youtube.com/embed/pm_WFnWqn20).

## I was not alone
What I didn’t know at the time is that Mathieu Boespflug, Alexander Vershilov, and Facundo Domínguez drew the same inspiration as I did from Geoff Mainland’s work on [Quasiquoting Support for GHC](https://www.cs.drexel.edu/~mainland/projects/quasiquoting/) and independently developed [`inline-r`](https://tweag.github.io/HaskellR/), a Haskell library for inline [R code](https://www.r-project.org/). Subsequently, Mathieu worked with Francesco Mazzoli on [`inline-c`](https://github.com/fpco/inline-c/tree/master/inline-c) and developed [`inline-java`](https://github.com/tweag/inline-java#readme) with Facundo Domínguez.

The latter is where foreign inline code, once again, provides an unorthodox solution to an old problem. In an attempt to fit into the ubiquitous Java ecosystem, there has been a string of failed attempts to compile Haskell to JVM code — although, maybe, [one](http://eta-lang.org) will eventually be successfully, even if at a [steep price](http://eta-lang.org/docs/html/faq.html#how-different-is-eta-from-ghc). Integration with Java is highly attractive as it opens the door to many applications and commercial opportunities. In addition, successful entrants into the JVM ecosystem, such as Scala and Clojure, suggest that generating JVM byte code is the opportune approach.

Nevertheless, inline Java in Haskell provides a much simpler alternative with many benefits. 

* The implementation of a new code generator for the JVM is a large and complicated endeavour; hence, the many failed attempts. Even if completed, it creates a high maintenance load. In contrast, `inline-java` is a very manageable project.
* By generating JVM byte code, you lose access to all existing packages that depend on foreign code, such as C libraries. In contrast, `inline-java` happily enables projects involving Haskell, Java, and C without any need to change existing packages.
* The runtime characteristics of Haskell code are not particularly well matched with those that the JVM is optimised for. Haskell has a much higher allocation rate than Java, it has entirely different update patterns due to purity and laziness, and it relies on different control flow, including heavily reliance on tail calls and their optimised implementation. In contrast, `inline-java` just uses the tried and tested GHC native code as is.

Moreover, we can still maintain the convenience of bundled distribution, as the Java archive (JAR) format is sufficiently flexible to allow arbitrary native code alongside JVM byte code in a single self-contained bundle — we detailed this in a previous post on the [Haskell compute PaaS with Sparkle](http://www.tweag.io/posts/2016-06-20-haskell-compute-paas-with-sparkle.html).

## Happy coexistence
All in all, inline foreign code enables diversity in the form of scalable mixed language projects while requiring no more than a limited toolchain maintenance burden. Interestingly, all Haskell foreign inline code libraries have enabled and been driven by concrete applications. The [Haskell for Mac](http://haskellformac.com) IDE is built on `language-c-inline`. The package `inline-r` was originally developed for Amgen as part of a commercial project. The package `inline-c` was developed by FP Complete for client work and is used by LumiGuide for their [OpenCV](https://github.com/LumiGuide/haskell-opencv) work. Finally, `inline-java` was developed by Tweag I/O for a data science project targeting Apache Spark.
