---
title: Capability is about free monads<br/>It's a bird… It's a plane… It's a free monad!
shortTitle: Capability is about free monads
author: Arnaud Spiwack
---

The subject of free monads is [resurfacing of
late][mcguire-free-monads], as it does from time to time. What
prompted this write-up is that Sandy McGuire [followed
up][mcguire-impredicative-free-monads] his post with a discussion about
an impredicative encoding (aka final encoding) of free monads:

```haskell
newtype Freer f a = Freer (forall m. Monad m => (forall t. f t -> m t) -> m a)
```

That is: given a monad `m`, and an interpretation of my operations
(`f`) in that monad, I can build an `m a`.

As far as I know, the subject of impredicative encoding of free monads
was first tackled, as many good things, [by Russell
O'Connor][oconnor-vl-free-monad], who calls them van Laarhoven
free monads. His blog post is a fairly mathy read. But the key bit is this:

```haskell
-- (ops m) is required to be isomorphic to (Π n. i_n -> m j_n)
newtype VLMonad ops a = VLMonad { runVLMonad :: forall m. Monad m => ops m -> m a }
```

Where `Π n. i_n -> m j_n` is mathematician's way of saying `forall n. i n -> m (j
n)`. Up to a very small difference, this is indeed the same
type. O'Connor then proves that this type is isomorphic to the usual
presentation of free monads.

```haskell
data Free f a = Return a | Free (f (Free f a))
```

# Less is more

The comment, in Russell O'Connor's snippet, is crucial in the
proof. Without it you can't establish the isomorphism between
`VLMonad` and the traditional `Free` monad.

That's because not all effects can be represented in free monads. The
prime example is exception handlers. You can make a function

```haskell
handle :: Free MyEffect a -> Free MyEffect a -> Free MyEffect a
```

But it would have the property that `(handle s f) >>= k = handle (s
>>= k) (f >>= k)`: that is exceptions raised after exiting the handler
would still be caught by the handler. It is not a useless function,
but it is not an exception handler. This phenomenon is a property of
the free monad construction. In the impredicative encoding, it can be
thought of as a consequence of the fact that in `forall a. f a -> m
a`, `f` cannot refer to `m`.

So, while being isomorphic to `Free` is a nice theoretical property,
Russell O'Connor's phrasing presents us with an opportunity: if we
simply drop the comment restricting the form of `ops`, we get a less
constrained free monad type which supports more effects. Let's call it
`MyFreeMonad`

```haskell
newtype MyFreeMonad ops a = MkFree { runFree :: forall m. Monad m => ops m -> m a}
```

# Towards capability

Functions in our newfangled `MyFreeMonad` will look, as functions in a monad do, like
 
```haskell
somefunction :: A -> MyFreeMonad Ops B
```

Where `Ops` represent the possible effects. For instance, if you need
a state effect, you would define `Ops` as

```haskell
data Ops m = Ops
  { put :: Int -> m ()
  ; get :: m Int }
```

But, after all, `MyFreeMonad` is simply a newtype: we could very well
inline its definition.

```haskell
somefunction :: A -> forall m. Monad m => Ops m -> m B
```

The ordering of types and argument is not too idiomatic. So let's
rearrange them a little:

```haskell
somefunction :: Monad m => Ops m -> A -> m B
```

This may look like a familiar style of structuring effect, it is, for
instance the style [advertised by Éric Torreborre in a recent blog
post][torreborre-capabilities-as-records]. It's not really so much an
alternative to free monads as a slightly more liberal version of free
monads (remember: we have dropped a restriction along the way, so that
we can have exception-handler effects).

Personally, I find it rather tiresome to explicitly carry around the
capabilities (the `Ops` thing) at every function call. I'd rather keep
my function arguments for the program logic, and leave all the
plumbing to the monad. Therefore, I turn `Ops` into a type class, and
move it “left of the fat arrow”: really in Haskell `A -> B` and `A =>
B` mean the same thing, they only differ in whose responsibility it is
to pass the arguments around.

```haskell
somefunction :: (Monad m, Ops m) => A -> m B
```

The definition of `Ops` for a state effect would, then, become

```haskell
class Ops m where
  put :: Int -> m ()
  get :: m Int
```

This is precisely the style of programming supported by the
[capability][hackage-capability] library (see also our [announcement
blog post][capability-announcement]).

# Closing thoughts

Free monads, capabilities-as-records and capabilities-as-type-classes,
are, essentially, three different flavours of the same thing (with
free monads technically being the more restrictive of the three, as it
can't have exception handling effects).

Choosing between the three is, ultimately, a matter of taste. I really
like capabilities-as-type-classes because it pushes the boilerplate
outside of the program logic.

At the end, what really matters is [the core
idea][mcguire-free-monads] [shared by][capability-annoucement] [these
three approaches][torreborre-capabilities-as-records]: capabilities
should be expressed in terms of the program logic, not in terms of
implementation details.

[mcguire-free-monads]: https://reasonablypolymorphic.com/blog/freer-monads/
[mcguire-impredicative-free-monads]: https://reasonablypolymorphic.com/blog/too-fast-too-free/index.html
[oconnor-vl-free-monad]: http://r6.ca/blog/20140210T181244Z.html
[torreborre-capabilities-as-records]: https://medium.com/barely-functional/freer-doesnt-come-for-free-c9fade793501
[hackage-capability]: http://hackage.haskell.org/package/capability
[capability-announcement]: https://www.tweag.io/posts/2018-10-04-capability.html
