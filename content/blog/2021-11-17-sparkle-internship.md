---
title: "Safe Sparkle: a resource-safe interface with linear types"
author: Noah Goodman
tags: [internship, linear-types, inline-java, data-science]
description: "On design choices to build a resource-safe interface for Sparkle using linear types"
---

During my internship at Tweag, I worked on creating a
safe interface for the [sparkle](https://github.com/tweag/sparkle) library under the excellent mentorship of Facundo
Domínguez. `sparkle` is a Haskell library that provides bindings for
[Apache Spark](https://spark.apache.org/) using `inline-java`'s Java interop capabilities.
As such, [this new safe interface](https://github.com/tweag/sparkle/pull/160) accomplishes the same
thing that [`inline-java`'s safe interface](https://www.tweag.io/blog/2020-02-06-safe-inline-java/) does; it helps to ensure safe
management of Java references at compile-time using linear types.

As discussed in [this earlier post](https://www.tweag.io/blog/2017-11-29-linear-jvm/),
we need to be careful to free Java references provided by `inline-java` when they're done
being used, and we also shouldn't use them after they've been freed. Since `sparkle`
manipulates references to Java objects defined in the Spark Java library, any potential users
must take care to safely manage references when using `sparkle` as well. Hence,
the goal of creating a safe interface for `sparkle` was clear: ensure that users
manage references to Spark objects safely, using linear types. However, actually
designing a safe interface that achieved this goal in the best way possible involved a
couple of non-obvious design decisions along the way.

In this post, I will discuss some of the more important design choices I made,
both as a way to introduce people to the new safe interface for `sparkle`, and
possibly, as a more general guideline for things to consider when designing a
library that achieves safe resource management in a linear monad.

## Porting Strategy

The first design decision I had to make, although not a user-facing one, was how
I wanted to port the unsafe `sparkle` library over to a safe version. For the
most part, the [`jvm`](https://github.com/tweag/inline-java/tree/master/jvm) and
[`jni`](https://github.com/tweag/inline-java/tree/master/jni) libraries (on top of which both
`inline-java` and `sparkle` are built) structure their safe interfaces as wrappers
around the corresponding unsafe ones. That is, a typical function defined in one
of these libraries will involve some unwrapping of data types, followed by an
unsafe call to the underlying unsafe function of interest, plus maybe some extra
reference management. For example, the `getArrayLength` function in the safe version of
`jni` is essentially a wrapper around the original implementation in the unsafe
version:

```Haskell
getArrayLength :: MonadIO m => JArray a %1-> m (JArray a, Ur Int32)
getArrayLength = Unsafe.toLinear $ \o ->
    liftPreludeIO ((,) o . Ur <$> JNI.getArrayLength (unJ o))
```

In this case, the library writer was careful to check that the "unsafe" version
of `getArrayLength` was actually safe and didn't delete or modify the original
array `o` that was passed in, justifying a call to `Unsafe.toLinear`.
And indeed, there's not much of a choice here but to use `Unsafe.toLinear`.
The actual implementation of `getArrayLength` involves a call to
[`inline-c`](https://hackage.haskell.org/package/inline-c-0.9.1.5), for which
there is no linearly-typed safe interface.

In the case of `sparkle`, however, there was another option: reimplement all the
functionality in the safe interface using the safe interfaces from
`inline-java`, `jvm`, and `jni`. Since `sparkle` is primarily built on top of
these libraries, we no longer run into the problem of primitives whose
implementation is inherently unsafe/nonlinear. The main benefits of this approach
are that the new implementations are more likely to be safe, as they're
built from safe building blocks (whereas using `Unsafe.toLinear` requires us to
be very careful each time we use it) and that many of `sparkle`'s bindings work
out of the box when we switch the underlying libraries. The main downsides are that there is more
code repetition between the safe and unsafe interfaces and that some functions
may be more complicated to implement if we limit ourselves to only using linear
types. For example, we may need to use folds instead of maps in order to thread
some shared, immutable linear resource across a sequence of actions.

In the end, I went for the latter approach, as it guarantees more safety in the
implementation itself (the entire safe interface uses `Unsafe.toLinear` only
once), and the process of adapting pre-existing code to work
with the safe interfaces for `inline-java`, `jvm`, and `jni` turned out to be
pretty straightforward in most cases.

## When to delete references?

The second major design point I had to address when designing the safe interface
was that of when references would be deleted. In any interface that deals with the safe
management of some resource, there must necessarily be some place where
the resource is ultimately consumed (or freed, or deleted). Technically speaking, if we have
some value bound in linear `IO`, let's say a reference to a Spark RDD:

```Haskell
Linear.do
  ...
  rdd <- parallelize sc [1,2,3,4]
  ...
```

Then at some point, we need to pass `rdd` to exactly one function that consumes
it linearly. Let's say we want to `filter` the elements in our `RDD`.
Then `filter` should consume `rdd` linearly. The Spark `filter` function also returns an RDD, so we would
probably assign `filter` a type signature as follows:

```Haskell
filter :: (Static (Reify a), Typeable a) => Closure (a -> Bool) -> RDD a %1 -> Linear.IO (RDD a)
```

Ignoring the complexities with distributed closures, this just says that
`filter` takes a filtering function, consumes a reference to an RDD linearly,
then returns a reference to that RDD with the `filter` transformation applied. RDDs are immutable, so
the returned reference refers to a different Java object than `rdd` did, but
this means that we might reasonably still want to do something with the original RDD
referred to by `rdd`! We could manually create another reference to `rdd` before
calling filter:

```Haskell
   (rdd0, rdd1) <- newLocalRef rdd
   filteredRDD <- filter (static (closure (> 3))) rdd0
```

But we might also be equally as justified in making the type signature of
`filter` as follows, instead:

```Haskell
filter :: (Static (Reify a), Typeable a) => Closure (a -> Bool) -> RDD a %1 -> Linear.IO (RDD a, RDD a)
```

In this case we return a reference to the input RDD, as well as a reference to the
new, filtered one. The only problem here is that sometimes we might not need the
reference to the original RDD anymore, in which case we would have to manually
delete it:

```Haskell
   (oldRDD, filteredRDD) <- filter (static (closure (> 3))) rdd
   deleteLocalRef oldRDD
```

So what's the right type signature for `filter`? Both of these possible signatures are
equally expressive, so we need to determine which option is
better in practice.

Ultimately, the answer is somewhat subjective and will likely vary based on
resource usage patterns, but I largely opted for the first option (in which
we do _not_ return a copy of the original reference) in designing
the safe interface for `sparkle`. My reasons for choosing this
approach are that it allows for better composition, compatibility with the
unsafe interface, compatibility with `inline-java`, and practical ease of use.

### Composability

Deleting input references by default allows `sparkle` functions to compose more easily.
For example, imagine that we wish to define a function that takes an RDD of
words and tells us how many of them are palindromes. If we adopt the convention
that `sparkle` functions always return a reference to the input RDD, then this
function would look something like this:

```Haskell
countPalindromes :: RDD Text %1 -> IO (RDD Text, Ur Int64)
countPalindromes rdd =
  filter (static (closure isPalindrome)) rdd >>= \(oldRDD, newRDD) ->
    count newRDD >>= \(newRDD', res) ->
      deleteLocalRef newRDD' >>
        pure (oldRDD, res)
```

Here, we have `countPalindromes` return the input RDD in keeping with the chosen
convention. Using the other convention, however, our function would look like
this:

```Haskell
countPalindromes :: RDD Text %1 -> IO (Ur Int64)
countPalindromes rdd = filter (static (closure isPalindrome)) rdd >>= count
```

As we can see, not having to wrap and unwrap tuples in the output of functions
allows for more seamless composition.

### Compatibility

Additionally, this approach does not
fundamentally alter the return type of any functions, so they can be used in
much the same way as their unsafe counterparts (which makes porting unsafe
code to the safe interface easier). Similarly, our
chosen convention is the same one that
safe `inline-java` uses in its quasi-quotations, so many `sparkle`
functions behave exactly as one would expect their `inline-java`-implemented
analogs to behave. For example, `subtract` can be defined as nothing more than a
wrapper around the corresponding `inline-java` quasiquotation:

```Haskell
subtract :: RDD a %1 -> RDD a %1 -> IO (RDD a)
subtract rdd1 rdd2 = [java| $rdd1.subtract($rdd2) |]
```

### Usage patterns

Finally, this option fits better with common usage patterns in Spark.
A resource like a [file handle](https://hackage.haskell.org/package/linear-base-0.1.0/docs/System-IO-Resource.html#t:Handle),
which the user typically needs to use repeatedly, probably should be returned
from every function that consumes it. But in Spark, pipelines of transformations
and actions on a single entity (RDD, Dataset, etc.) are fairly common (take the
`countPalindromes` function above as a minimal example). That is, it is not
generally the case that one needs an older reference after doing something with
it, so it seems a bit cleaner to
create a few extra references when you need them than to have to clean up unused
references.

Note that the above applies only to references to immutable Spark objects. While
all functions that deal with immutable objects follow this convention, I dealt
with functions taking references to mutable objects on a more case by case basis.
For example, if we perform an action that mutates a mutable object, we would
typically want to use the object for something else afterwards (e.g. setting
a field in a configuration object before initializing a process with that
configuration) so it makes sense to return a reference to that
object.

## Unrestricted Values

In discussing what to return from functions, it's also worth briefly mentioning
what happens when a function returns something other than a reference to a Java
object. In this case, the function would just return a normal Haskell value, and
while everything may be embedded in linear `IO`, we don't care about managing Haskell
values in a linear fashion whatsoever, so I adopted the convention of wrapping
all returned Haskell values in `Ur`, signifying that these values are unrestricted
and may be used any number of times (including none at all). While it's
certainly possible to return Haskell values that aren't wrapped in `Ur`, doing
so would be needlessly limiting (see the section "Escaping Linearity" in [this
post](https://www.tweag.io/blog/2020-02-06-safe-inline-java/)).
Note that this is also the convention suggested by the safe `reify_` function from `jvm`:

```Haskell
reify_ :: (Reify a, MonadIO m) => J (Interp a) %1-> m (Ur a)
```

## Global References

Finally, one of the trickiest points involved in porting `sparkle` over to a safe
interface was the issue of global references.

In some cases, `sparkle` deals with objects that are "global" in some sense. For example
the static, final `BooleanType` field from the Spark `DataTypes` class is
global in the sense that any reference to
this field will refer to the same piece of memory in the JVM, and the
value of this object will never change. For an entity like this one, it seems a bit
unnecessary to have to keep track of a bunch of local reference to it if
it's always the same. It would be simpler to just have some kind of global
reference that we could always use to refer to this object. Ideally, we would
engineer this so that we would be able to avoid unnecessary copying of local
references and unnecessary JNI calls from the Haskell side each time we want to
do something with this object.

At first, simply using global references as
defined in `jni` seems like the most straightforward solution; however, a
mere coercion doesn't work when we want to pass global references to safe
Spark functions.

As it stands, Spark objects are represented in safe `sparkle` as wrappers around
safe local references. For example:

```Haskell
newtype DataType = DataType (J ('Class "org.apache.spark.sql.types.DataType"))
```

And safe local references are themselves just wrappers around unsafe references:

```Haskell
newtype J (a :: JType) = J (Unsafe.J a)
```

A global reference has the same type as a local reference, and many functions from
`jni` can work on both kinds of references. But unfortunately, there are different
calls to delete each of them. We have `deleteLocalRef` and `deleteGlobalRef`, and
the user shall not apply the wrong call for a reference or undefined behavior ensues.

Suppose we want to pass a global reference to a function that takes a `DataType`.
We could try to disguise our unsafe global reference as follows:

```Haskell
-- global reference to `BooleanType`
booleanTypeRef :: Unsafe.J ('Class "org.apache.spark.sql.types.DataType")

-- Takes a safe reference to a DataType and deletes it after using, as is the convention
safeSparkFunction :: DataType %1 -> IO ()

someFunc' = Linear.do
  disguisedRef <- pure $ DataType (Safe.J booleanTypeRef) -- disguise unsafe global ref as safe local ref
  safeSparkFunction disguisedRef                          -- RUNTIME ERROR or UNDEFINED BEHAVIOR
```

In this case, `safeSparkFunction` consumes its argument linearly, meaning that
it will delete any reference passed in, and we would get an error since `inline-java`
would use `deleteLocalRef`, which is invalid to call on a global reference.
Moreover, nothing prevents the library user from using `booleanTypeRef` after it
has been deleted!

So as it stands, there's not really a good way to pass a global reference into a
safe function taking a `DataType`, suggesting that perhaps the definition of `DataType`
is actually what needs to change. There's no way to wrap an unsafe global reference into a `DataType`
without covering up the fact that this reference is global.
Ideally, the global reference would be kept valid for as long as it is needed by the program.
It may be possible to change the internals of the safe `jni`, `jvm`, and
`inline-java` (in particular by making the safe `J` type into a union type)
to allow safe `sparkle` functions to take either global or local references as
arguments safely.

Overall, while the aforementioned potential changes are likely the most optimal
solution, I made the simpler, yet still workable compromise of
just using safe local references anywhere where a global reference might be
preferable. The main downsides of this approach are that the user may have to do
some extra manual reference management with these references where it is not
strictly necessary, and we also lose a few performance optimizations that come
from global references (such as avoiding unnecessary copying or JNI calls).
But the major advantage of this solution is that it is
simple for the user, as they will not have to worry about whether or not a given
reference is local or global (while any solution involving global references
would seek to minimize the degree to which the user needs to care about this,
the distinction is sure to surface somewhere). By treating all references as safe local
references, reference management becomes uniform across the entirety of the
interface, at the cost of some extra verbosity and minor performance hits.

## Closing remarks

We have now seen some of the problems involved when it comes to designing a
library that enforces safe reference management in linear `IO`. We went through
a case-study for linear types in Haskell and the safe interface of `inline-java`,
and I hope that it can serve as a motivation for understanding the importance
of thinking carefully about compatibility and ease-of-use when designing safe
resource-management libraries that use linear types. Indeed, these two factors
will play a large role in the future adoption of linear types in Haskell, and
understanding the relevant design choices will be essential in scaling the safe
resource-management library ecosystem.
