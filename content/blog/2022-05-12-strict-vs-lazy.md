---
title: Comparing strict and lazy
shortTitle: Comparing strict and lazy
author: Arnaud Spiwack
tags: [haskell, ocaml, programming-languages]
description: Arnaud takes a step back from the slogans and examines the trade-offs between lazy and strict languages.
---

_This blog post covers essentially the same material as the talk I
gave at Haskell Exchange 2020 (time truly flies). If you prefer
watching it in a talk format, you can [watch the
recording][arnaud-talk]. Or you can [browse the
slides][arnaud-slides]._

I first conceived of writing (well, talking, initially) on this
subject after one too many person told me “lazy is better than strict
because it composes”. You see, this is a sentence that simply doesn't
make much sense to me, but it is oft repeated.

Before we get started, let me make quite specific what we are going
to discuss: we are comparing programming languages, and whether by
default their function calls are lazy or strict. Strict languages can
(and do) have lazy data structures and lazy languages can have strict
data structures (though it's a little bit harder, in GHC, for
instance, [full support][unlifted-datatypes-proposal] has only
recently been released).

In the 15 years that I've been programming professionally, the
languages in which I've written the most have been Haskell and
Ocaml. These two languages are similar enough, but Haskell is lazy and
Ocaml is strict. I'll preface my comparison by saying that, in my
experience, when switching between Ocaml and Haskell, I almost never
think about laziness or strictness. It comes up sometimes. But it's
far from being a central consideration. I'm pointing this out to
highlight that lazy versus strict is really not that important; it's
not something that's worth the very strong opinions that you can see
sometimes.

## Locks

With these caveats established, I'd like to put to rest the statement
that laziness composes better. Consider the following piece of Haskell

```haskell
atomicPut :: Handle -> String -> IO ()
atomicPut h line =
  withMVar lock $ \_ -> do
    hPutStrLn h line
```

This looks innocuous enough: it uses a lock to ensure that the `line`
argument is printed without being interleaved with another call to
`atomicPut`. It also has a severe bug. Don't beat yourself up if you don't
see why: it's pretty subtle; and this bit of code existed in a
well-used logging library for years (until it broke production on a
project I was working on and I pushed a fix). The problem, here, is
that `line` is lazy, hence can contain an arbitrary amount of
computation, which is subsequently run by `hPutStrLn`. Running
arbitrary amounts of computation within a locked section is very bad.

The fix, by the way, is to fully evaluate the line before entering the
lock

```haskell
atomicPut :: Handle -> String -> IO ()
atomicPut h line = do
  evaluate $ force line
  withMVar lock $ \_ -> do
    hPutStrLn h line
```

It goes to show, though, that laziness doesn't compose with locks. You
have to be quite careful too: for each variable used within the locked
section, you need to evaluate it at least as much as the locked
code will before entering the lock.

## Shortcutting fold

When people claim that lazy languages compose better, what they think
about is something like this definition

```haskell
or :: [Bool] -> Bool
or =  foldr (||) False
```

This is truly very clever: because this implementation will stop
traversing the list as soon as it finds a `True` element. To see why,
let's look at the definition of `foldr`

```haskell
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)
```

When we call `foldr` recursively, we do that as an argument to `f`,
but since `f` is lazy, the recursive call is not evaluated until `f`
itself asks for the evaluation. In `or`, `f` is `(||)`, which doesn't
evaluate its second argument when the first is `True`, so the
recursive call never happens in this case.

It's absolutely possible to do the same thing in a strict
language. But it requires quite a bit more setup:

```ocaml
(* val fold_right_lazily : ('a -> 'b Lazy.t -> 'b Lazy.t) -> 'a List.t -> 'b Lazy.t -> 'b Lazy.t *)
let rec fold_right_lazily f l accu =
  match l with
  | [] -> accu
  | a::l' -> f a (lazy (Lazy.force (fold_right_lazily f l' accu)))

(* val or_ : bool List.t -> bool *)
let or_ l = Lazy.force (fold_right_lazily (fun x y -> x || (Lazy.force y)) l (Lazy.from_val false))
```

But, honestly, it's not really worth it. GHC takes a lot of care to
optimise lazy evaluation, since it's so central in its evaluation
model. But Ocaml doesn't give so much attention to lazy values. So
`fold_right_lazily` wouldn't be too efficient. In practice, Ocaml
programmers will rather define `or` manually

```ocaml
(* val or_ : bool List.t -> bool *)
let rec or_ = function
  | [] -> false
  | b::l -> b || (or_ l) (* || is special syntax which is lazy in the
                           second argument*)
```

## Applicative functors

Another, probably lesser known, way in which laziness shines is
applicative functors. At its core, an applicative functor, is a data
structure which supports `zipWith<N>` (or `map<N>` in Ocaml) for all
`N`. For instance, for lists:

```haskell
zipWith0 :: a -> [a]
zipWith1 :: (a -> b) -> [a] -> [b]
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
…
```

Of course, that's infinitely many functions, and we can't define them
all. Though probably, it's enough to define 32 of them, but even that
would be incredibly tedious. The applicative functor abstraction very
cleverly finds a way to summarise all these functions as just three
functions:

```haskell
pure :: a -> [a]
(<$>) :: (a -> b) -> [a] -> [b]
(<*>) :: [a -> b] -> [a] -> [b]
```

(in Haskell, this would be the `Applicative` instance of the `ZipList` type,
but let's not be distracted by that)

Then, `zipWith5` is derived simply as:

```haskell
zipWith5 :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 f as bs cs ds es = f <$> as <*> bs <*> cs <*> ds <*> es
```

The definition is so simple that you never need to define `zipWith5`
at all: you just use the definition inline. But there's a catch: were
you to use this definition on a strict data structure, the performance
would be abysmal. Indeed, this `zipWith5` would allocate 5 lists: each
call to `(<*>)` allocates an intermediate list. But a manual
implementation of `zipWith5` requires a single list to be
allocated. This is very wasteful.

For lazy list it's alright: you do allocate all 5 lists as well, but
in a very different pattern. You first allocate the first cons cell of
each of the 5 lists, and discard all 4 intermediate results. Then you
allocate the second cons cell of each list, etc… This means that at
any point in time, the memory overhead is constant. This is the sort
of load that a garbage collector handles very very well.

Now, this is really about lazy data structures versus strict data
structures, which I said at the beginning that I wouldn't discuss. But
I think that there is something here: in a strict language, you will
usually be handling strict data structures. If you want them to
support an efficient applicative interface, you will need a lazy copy
of the same data structure. This is a non-trivial amount of extra
code. I imagine it could be mitigated by the language letting you
derive this lazy variant of your data structure. But I don't think any
language does this yet.

## Matching lazy data is weird

That being said, lazy languages come with their lot of mind-boggling
behaviour. Pattern-matching can be surprisingly counter-intuitive.

Consider the following

```haskell
f :: Bool -> Bool -> Int
f _    False = 1
f True False = 2
f _    _     = 3
```

This second clause may seem completely redundant: after all anything
matched by the second clause is already matched by the first. However
it isn't: it forces the evaluation of the first argument. So the mere
presence of this clause changes the behaviour of `f` (it makes it that
`f undefined False = undefined`). But `f` can _never_ return `2`.

This and more examples can be found in Simon Peyton Jones's
keynote talk at Haskell Exchange 2019 [_Revisiting Pattern Match
Overlap Checks_][spj-hx2019] as well as in an older paper [_GADTs meet
their match_][gadts-match], by Karachalias, Schrijvers, Vytiniotis,
and Peyton Jones.

## Memory

Consider the following implementation of list length:

```haskell
length :: [a] -> Integer
length [] = 0
length (_:as) = 1 + length as
```

It's not a good implementation: it will take $O(n)$ stack space, which
for big lists, will cause a stack overflow, even though `length`
really only require $O(1)$ space. It's worth noting that we are
consuming stack space, here, because `(+)` is strict. Otherwise, we
could have been in a case like the `or` function from earlier, where
the recursive call was guarded by the lazy argument and didn't use
space.

For such a strict recursion, the solution is well-known: change the
`length` function to be tail recursive with the help of an
accumulator:

```haskell
length :: [a] -> Integer
length = go 0
  where
    go :: Integer -> [a] -> Integer
    go acc [] = acc
    go acc (_:as) = go (acc+1) as
```

This transformation is well-understood, straightforward, and also
incorrect. Because while it is true that we are no longer using stack
space, we have traded it for just as much heap space. Why? Well, while
`(+)` is strict, `Integer` is still a lazy type: a value of type
`Integer` is a thunk. During our recursion we never evaluate the
accumulator; so what we are really doing is creating a sort of copy of the
list in the form of thunks which want to evaluate `acc + 1`.

This is a common trap of laziness (see also [this blog
post][neil-mitchell-space-leaks] by Neil Mitchell). It's much the same
as why you typically want to use `foldl'` instead of `foldl` in
Haskell. The solution is to evaluate intermediate results before
making recursive calls, for instance with bang patterns[^faulty-length]:

```haskell
length :: [a] -> Integer
length = go 0
  where
    go :: Integer -> [a] -> Integer
    go !acc [] = acc
    go !acc (_:as) = go (acc+1) as
```

[^faulty-length]:
  As it happens, the faulty implementation of length
  can be found in base. It's not exposed to the user but it's used
  internally. It's saved by the fact that it uses `Int` rather than
  `Integer`, and the strictness analysis automatically makes the
  `go` function strict. I honestly have no idea whether the author
  was conscious of the fact that they were leveraging the strictness
  analysis this way, or whether it's another piece of evidence that
  it's very easy to get wrong.

This is an instance of a bigger problem: it's often very difficult to
reason about memory in lazy languages. The question “do these few
lines of code leak memory” sometimes provokes very heated discussions
among seasoned Haskell programmers.

On a personal note I once fixed a pretty bad memory leak. The fix was
pushed in production. When I came back to work the next day, the
memory leak was still there. What had happened is that there were
actually _two_ memory leaks, one was caused by laziness: my reproducer
forced the guilty thunks, so masked that second leak. I only fixed
one. I got burnt by the fact that the memory usage of applications
with a lot of laziness [changes when you observe them][heisenbug].

## Lazy deserialisation

The flip side, though, is that if you don't need a whole
data structure, you won't allocate the unnecessary parts without
having to think about it. Where this shines the brightest, in my
opinion, is in lazy deserialisation.

The scenario is you get (from disk, from the network,…) a
datastructure in the form of a serialised byte-string. And you convert
it to a linked data structure for manipulation within your
application. If the data structure is lazy, you can arrange it so that
forcing part of the data structure performs the relevant
deserialisation (for instance [Json can be deserialised
lazily][hw-json]).

This is very nice because linked data structures weigh strongly on the
garbage collector which needs to traverse them again and again, while
byte-strings are essentially free.

In this scenario you can, guilt-free, convert your byte-string to a
value of your data structure. And deserialisation will happen
implicitly on demand.

This can be done in a strict language with a lazy data structure, but
this requires more care and more boilerplate, so it can get in the way
sometimes.

## Conclusion

Comparing laziness and strictness, it's difficult to find a clear
winner. I could have given more points of comparisons (in fact, there
are a few more in [my talk][arnaud-talk]), but the pattern
continues. Of course, among the different advantages and disadvantages
of laziness and strictness, there may be some which count more for
you. This could make you take a side. But others will want to make a
different trade-off.

From where I stand, laziness is not a defining feature of Haskell. In
fact, Purescript, which is undeniably a Haskell dialect, is a strict
language. Things like type classes, higher-order type
quantification, and purity are much more relevant in distinguishing
Haskell from the rest of the ML family of languages.

In my opinion, the main role that laziness has played for Haskell
is making sure that it stays a pure language. At the time, we
didn't really know how to make pure languages, and a strict language
would have likely eventually just added effects. You simply can't do
that with a lazy language, so Haskell was forced to be creative. But
this is a thing of the past: we now know how to make pure languages
(thanks to Haskell!), we don't really need laziness anymore.

I honestly think that, nowadays, laziness is a hindrance. Whether you
prefer laziness or strictness, I find it difficult to argue that the
benefits of laziness are large enough to justify Haskell being the only lazy
language out there. So much of the standard tooling assumes strictness
(it's a legitimate question to ask what it would mean to step through
a Haskell program with gdb, but there is no doubt about what it means
for Ocaml). So if you're making a new language, I think that you
should make it strict.

[arnaud-talk]: https://skillsmatter.com/skillscasts/14899-comparing-strict-and-lazy
[arnaud-slides]: https://slides.com/aspiwack/hx2020
[unlifted-datatypes-proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0265-unlifted-datatypes.rst
[spj-hx2019]: https://skillsmatter.com/skillscasts/13014-keynote-simon-peyton-jones
[gadts-match]: https://dl.acm.org/doi/10.1145/2784731.2784748
[neil-mitchell-space-leaks]: http://neilmitchell.blogspot.com/2015/09/three-space-leaks.html
[heisenbug]: https://en.wikipedia.org/wiki/Heisenbug
[hw-json]: https://hackage.haskell.org/package/hw-json
