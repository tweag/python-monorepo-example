---
title: "Named Routes in Servant"
description: "Use records to structure your Servant APIs"
author: Gaël Deest
tags: [haskell]
---

Servant 0.19 [was
released](https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md#019)
earlier this month. It features a new combinator, called `NamedRoutes`, which
allows you to structure your APIs with records. Concretely, instead of building
up a tree of anonymous routes with `(:<|>)`:

```haskell
type API =
       PublicRoutes
  :<|> "admin" :> Auth '[BasicAuth] User :> AdminRoutes

type PublicRoutes =
       "version" :> Get '[JSON] Version
  :<|> …

type AdminRoutes =
       "do_stuff" :> ReqBody '[JSON] Request :> Post '[JSON] Response
  :<|> …
```

We can now write:

```haskell
type API = NamedRoutes NamedAPI

data NamedAPI mode = NamedAPI
  { publicRoutes :: mode :- NamedRoutes PublicRoutes
  , adminRoutes  :: mode :-  "admin" :> Auth '[BasicAuth] User :> NamedRoutes AdminRoutes
  }
  deriving Generic

data PublicRoutes mode = PublicRoutes
  { version :: mode :- "version" :> Get '[JSON] Version
  , …
  }
  deriving Generic

data AdminRoutes mode = AdminRoutes
  { doStuff :: mode :- "do_stuff" :> ReqBody '[JSON] Request :> Post '[JSON] Response
  , …
  }
  deriving Generic
```

This is slightly more verbose, but for good reason: named routes are much
nicer to work with than anonymous routes, especially when dealing with complex
route hierarchies exposing dozens of endpoints. To serve the API above, all we
need to do is provide nested records of handlers:

```haskell
app :: Application
app = serveWithContext (Proxy @API) ctx NamedAPI
  { publicRoutes = PublicRoutes
      { version = pure "0.1.0"
      , …
      }
  , adminRoutes = \case
      Authenticated usr -> AdminRoutes
        { doStuff = …
        , …
        }
      _ -> throwAll err401
  }
  where ctx = …
```

Since matching between handlers and servant types is now done by name instead of
position, order of handlers becomes irrelevant, which eliminates an entire
source of frustration for servant users.

But it doesn't stop here: GHC can now generate helpful, more focused error
messages than it could before. For example, if we were to add an endpoint to the
public routes of the anonymous API, like this:

```haskell
type PublicRoutes =
       "version" :> Get '[JSON] Version
  :<|> "give_me_an_int" :> Capture "someInt" Int :> Get '[JSON] Int
  :<|> …

```

But forgot to implement a handler for it, GHC would produce the following error
message:

```
    • Couldn't match type ‘Handler Int’ with ‘[Char]’
      Expected type: Server
                       ((("version" :> Get '[JSON] Version)
                         :<|> ("give_me_an_int" :> Capture "someInt" Int :> Get '[JSON] Int))
                        :<|> ("admin"
                              :> (Auth '[BasicAuth] User
                                  :> ("do_stuff"
                                      :> (ReqBody '[JSON] Request :> Post '[JSON] Response)))))
        Actual type: (Handler [Char] :<|> [Char])
                     :<|> (AuthResult User -> Request -> Handler Response)
    • In the third argument of ‘serveWithContext’, namely
        ‘(public :<|> admin)’
```

The best GHC can do here is to report the discrepancy between the expected type
of the server (computed from the entire API type) and the effective type of the
server provided to `serveWithContext`. This is already not great with small APIs like
this one, but it quickly becomes unmanageable in real-world applications. In
contrast, the same situation with `NamedRoutes` will lead to a simple message
about a missing field, no matter the size of our API:

```
    • Fields of ‘PublicRoutes’ not initialised: giveMeAnInt
    • In the ‘publicRoutes’ field of a record
      In the third argument of ‘serveWithContext’, namely
        ‘NamedAPI
           {publicRoutes = PublicRoutes {version = pure "0.1.0", …},
```

Similarly, if we implement a handler of the wrong type returning — say — a
`String` instead of an `Int`, the compiler will mention the infringing handler
in the error message:

```
    • Couldn't match type ‘[Char]’ with ‘Int’
      Expected type: Servant.Server.Internal.AsServerT Handler
                     :- ("give_me_an_int" :> (Capture "someInt" Int :> Get '[JSON] Int))
        Actual type: Int -> Handler String
    • Possible cause: ‘(.)’ is applied to too many arguments
      In the ‘giveMeAnInt’ field of a record
      In the ‘publicRoutes’ field of a record
```

While GHC could me slightly more helpful by expanding the `:-` type family for
us here, we can easily confirm with GHCi what type it is expecting:

```
> :kind! Servant.Server.Internal.AsServerT Handler :- ("give_me_an_int" :> (Capture "someInt" Int :> Get '[JSON] Int))

Servant.Server.Internal.AsServerT Handler :- ("give_me_an_int" :> (Capture "someInt" Int :> Get '[JSON] Int)) :: *
= Int -> Handler Int
```

Clients are also significantly nicer to work with when using `NamedRoutes`: the
`client` function will generate nested records out-of-the-box, so we no longer
need to pattern-match on its output to retrieve the functions. We can just do:

```haskell
apiClient :: Client ClientM API
apiClient = client (Proxy @API)
```

And use normal field accessors. As a bonus, some operators have been added to
make client usage feel more natural:

```haskell
someClientComputation :: ClientM Int
someClientComputation = do
  version <- apiClient // publicRoutes // version
  _ <- apiClient // adminRoutes /: Token "auth_token" // doStuff /: Request
  …
  apiClient // publicRoutes // giveMeAnInt /: 42
```

## Design

What I just showed isn't _entirely_ new. Servant has had some form of support
for records since Patrick Chilton's early work on
[`servant-generic`](https://github.com/chpatrick/servant-generic) (itself based
on [solga](https://github.com/chpatrick/solga)) in 2017. The `servant-generic`
library was merged into `servant` itself the next year. So, what's the news,
really ?

For starters, `servant-generic` isn't going anywhere ; it is actually the
foundation for `NamedRoutes`. But `NamedRoutes` improves upon it by making
records truly first-class citizens in Servant.

### Inside Servant

Servant is a [type-level
DSL](https://haskell-servant.github.io/posts/2018-07-12-servant-dsl-typelevel.html):
an API is input as a type and not as a value. APIs are defined using a variety
of combinators. The most central ones are:

- `Verb`, which figures at the tip of each endpoint:

```haskell
data Verb (method :: StdMethod) (statusCode :: Nat) (contentTypes :: [*]) (a :: *)
data StdMethod = GET | POST | HEAD | PUT | DELETE | …

type Get = Verb 'GET 200
type Post = Verb 'POST 200
…
```

- `(:>)`, used to add path prefixes and request parameters to a route:

```haskell
data (path :: k) :> (a :: *)
```

- `(:<|>)`, used to combine APIs into larger ones (it may remind you of `(<|>)`
  from the `Alternative` typeclass — this is very much
  intentional). The `(:<|>)` combinator lets us describe APIs with multiple
  endpoints, and structure them into trees.

```haskell
data a :<|> b = a :<|> b
```

The Servant DSL is interpreted through various type classes (`HasServer`,
`HasClient`, `HasLink`, …).
As a concrete example, consider the definition of the `HasServer` type class
(simplified for the sake of exposition):

```haskell
class HasServer api where
  -- | Associated type family computing the type of the server of a given API in the `m` monad
  type ServerT api (m :: * -> *) :: *


  -- | Route a request
  route ::
       Proxy api
    -> Delayed (ServerT api Handler)
    -- ^ Server representation with delayed request checks.
    -> Router

  -- | Hoist a server from one monad to another
  hoistServer
      :: Proxy api
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n
```

Its implementation for `a :<|> b` defines the server for such an API as a pair
of servers (`(:<|>)` is used both as a type and a data constructor here), and
piggy-backs on the instances of `HasServer` for `a` and `b` to implement
hoisting and routing:

```haskell
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa ((\ (a :<|> _) -> a) <$> server))
                              (route pb ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

  hoistServer _ nat (a :<|> b) =
    hoistServer (Proxy :: Proxy a) nt a :<|>
    hoistServer (Proxy :: Proxy b) nt b

```

When serving an API, the `serve` function then ensures that we give it a server
of the appropriate type, by applying the `ServerT` type family to the API type:

```haskell
serve :: HasServer api => Proxy api -> ServerT api Handler -> Application
```

### `servant-generic` Primer

The records of routes one must define with `servant-generic` are exactly the
same as those expected by `NamedRoutes`, e.g.:

```haskell
data BookCRUD mode
  = BookCRUD
  { createBook :: mode :- ReqBody '[JSON] BookData :> Post '[JSON] BookId
  , getBook    :: mode :- Capture "book_id" BookId :> Get '[JSON] Book
  , updateBook :: mode :- Capture "book_id" BookId :> ReqBody '[JSON] BookData :> Put '[JSON] NoContent
  , deleteBook :: mode :- Capture "book_id" BookId :> Delete '[JSON] NoContent
  }
  deriving Generic
```

Until now, we have glossed over the `mode` parameter and the `:-` operator. The
explanation is in the `GenericMode` type class, and its instances:

```haskell
class GenericMode mode where
  type mode :- api :: *

instance GenericMode (AsServerT m) where
  type (AsServerT m) :- api = ServerT api m

instance GenericMode AsApi where
  type AsApi :- api = api
…
```

So, `BookCRUD (AsServerT Handler)` is a record of handlers, and `BookCRUD AsAPI`
is an uninhabited record whose fields have Servant API types.

But servant doesn't know how deal with named products of routes such as
`BookCRUD AsAPI`: it only knows about anonymous `(:<|>)`-trees. So
`servant-generic` introduces a way of converting from vanilla `(:<|>)` products
to records, and vice versa, via yet another type class:

```haskell
class GServantProduct f where
    type GToServant f
    gtoServant   :: f p -> GToServant f
    gfromServant :: GToServant f -> f p
```

`GServantProduct` operates on the Generic representation of the datatype. The
associated type family, `GToServant`, essentially maps `:*:` to `:<|>` and
implements the straightforward conversion:

```haskell
-- Map products (:*:) to (:<|>:)
instance (GServantProduct l, GServantProduct r) => GServantProduct (l :*: r) where
    type GToServant (l :*: r) = GToServant l :<|> GToServant r
    gtoServant   (l :*: r)  = gtoServant l :<|> gtoServant r
    gfromServant (l :<|> r) = gfromServant l :*: gfromServant r

-- Do not transform leaves
instance GServantProduct (K1 i c) where
    type GToServant (K1 i c) = c
    gtoServant   = unK1
    gfromServant = K1
```

Now, the following two functions allow us to convert between records and
`:<|>`-products:

```haskell
-- | Record to :<|>
toServant
    :: GenericServant routes mode
    => routes mode -> ToServant routes mode
toServant = gtoServant . from

-- | :<|> to record
fromServant
    :: GenericServant routes mode
    => ToServant routes mode -> routes mode
fromServant = to . gfromServant
```

We now have all the pieces required to understand how `BooksCRUD` can be served:

- The vanilla servant type can be computed as `GToServant (Rep (BooksCRUD AsApi))`.
- The record of handlers can be converted to a vanilla servant product using toServant.

```haskell
type ToServant routes mode = GToServant (Rep (routes mode))
type ToServantApi routes = ToServant routes AsApi

app :: Application
app = serve (Proxy @(ToServantApi BooksCRUD) (toServant server)
  where server = BooksCRUD { … }
```

### The Problem with `servant-generic`

While `servant-generic` does allow us to implement servers as records (and
similarly, to generate records of client functions) it does not really integrate
into Servant itself: at no point is any naming information available to the core
mechanisms of Servant, as it is all erased using (via `GToServant` /
`gToServant`).

This is not problematic when dealing with non-nested records of routes such as
our `BooksCRUD` example above. But it is much more painful when dealing with
nested APIs such as our original example. At first glance, our API definition
does not change so much, as we just replaced every occurrence of `NamedRoutes`
by `ToServantApi`:

```haskell
type API = ToServantApi NamedAPI

data NamedAPI mode = NamedAPI
  { publicRoutes :: mode :- ToServantApi PublicRoutes
  , adminRoutes  :: mode :-  "admin" :> Auth '[BasicAuth] User :> ToServantApi AdminRoutes
  }
  deriving Generic

…
```

To serve this API, though, we must call the `toServant` function at each level
of the route hierarchy:

```haskell
app :: Application
app = serveWithContext (Proxy @API) ctx $ toServant NamedAPI
  { publicRoutes = toServant PublicRoutes
      { version = pure "0.1.0"
      , …
      }
  , adminRoutes = \case
      Authenticated usr -> toServant AdminRoutes
        { doStuff = …
        , …
        }
      _ -> throwAll err401
  }
  where ctx = …
```

It is cumbersome, and the error messages in case of a forgotten
`toServant` call can be quite confusing and unhelpful.

To put it succinctly, the
`servant-generic` machinery is very demanding: it is the
responsibility of the user
to understand it well enough to perform the necessary conversions. It has been a
recurring [source](https://github.com/haskell-servant/servant/issues/1040) of
[confusion](https://github.com/haskell-servant/servant/issues/1015) for users.

### Here Comes `NamedRoutes`

The idea of `NamedRoutes` is to embed records into Servant APIs using a bona
fide combinator:

```haskell
data NamedRoutes (api :: * -> *)
```

And define:

```haskell
instance (HasServer (ToServantApi api)) => HasServer (NamedRoutes api)
  type ServerT (NamedRoutes api) m = api (AsServerT m)

  …
```

This instance declares the server for `NamedRoutes api` in monad `m` to be
a record of handlers in the same monad (using `AsServerT m` as the supplied
`mode` parameter).

It's trickier that it may sound at first, otherwise `servant-generic`
would have probably done just that to begin with. Our troubles come in
the form of the `hoistServer` method of the `HasServer`
class. Its type is:

```haskell
hoistServer
    :: Proxy api
    -> (forall x. m x -> n x)
    -> ServerT api m
    -> ServerT api n
```

In the case of `NamedRoutes` its type unrolls to:

```haskell
hoistServer
    :: Proxy (NamedRoutes api)
    -> (forall x. m x -> n x)
    -> api (AsServerT m)
    -> api (AsServerT n)
```

So, we have a record of `ServerT _ m` and we need to change it into a record of
`ServerT _ n`. To do so, we convert to the record to its `:<|>`-based equivalent
with `toServant`, hoist it to the new monad, and convert back to a record type
with `fromServant`:

```haskell
hoistServer _ nat = fromServant . hoistServer nat . toServant
```

However, we have a problem. Indeed `toServant` has type `api (AsServerT m) -> ToServant api (AsServerT m)`. But `hoistServer`
expects an argument whose type is of the form `ServerT _ m`. These
types don't align!

But we know that `servant-generic` is designed so that `ToServant api (AsServerT m)` is indeed of the form `ServerT _ m` (specifically, it
is `ServerT (ToServantAPI api) m`). It's just that GHC cannot see that
in general: it can only be verified for concrete API types. So we need
to give GHC a little bit of help and provide this identity (as well as
the corresponding one for `n` needed for `fromServant`):

```haskell
instance
  ( HasServer (ToServantApi api)
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  , ToServant api (AsServerT n) ~ ServerT (ToServantApi api) n
  ) => HasServer (NamedRoutes api) where
  …
```

This doesn't work, though, since neither `m` nor `n` are bound in the
instance definition: they only make sense in the type of
`hoistServer`. But the truth is that `ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m` holds _independently of the choice of
`m`_. So really, what we want to write is

```haskell
instance
  ( HasServer (ToServantApi api)
  , forall m. ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  ) => HasServer (NamedRoutes api) where
  …
```

This is a [quantified
constraint](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/quantified_constraints.html). Quantified
constraints are a recent feature, only available since GHC 8.6
(September 2018). Therefore, at the time `servant-generic` was
conceived, having a `NamedRoutes` combinator was therefore not an
option. In fact, Servant 0.19 is not compatible with GHC versions prior
to 8.6 for this very reason.

Unfortunately, our troubles don't end here. Indeed GHC rejects
our quantified constraint with this error message

```
• Illegal type synonym family application ‘ToServant api (AsServerT m)’ in instance:
    ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
• In the quantified constraint ‘forall m. ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m’
```

GHC complains that `ToServant` (and `ServerT`) is a type family. In
quantified constraints, the usage of type families obeys similar
restrictions as in type-class instances (see [this
discussion](https://gitlab.haskell.org/ghc/ghc/-/issues/17959) for
more details).

Yet, we're almost there: we need one more trick to work around GHC's
restriction. Specifically, we introduce the following type class:

```haskell
class GServer (api :: * -> *) (m :: * -> *) where
  gServerProof :: Dict (ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m)
```

Where the `Dict` type lets us manipulate type classes and identities
as values (as described, for instance, on [this wiki
page](https://gitlab.haskell.org/ghc/ghc/-/wikis/distributed-haskell?version_id=56738617760547619e264e529ce7af8de702937d#the-dict-trick)). Having
the identity be a value requires more work from the programmer, but
it's much more flexible as well. With this we can complete our definition.

In the instance context, we then require `forall m. GServer api m` (which is a
valid quantified constraint) and pattern-match on the `Dict` produced by
`gServerProof @api @n` as needed:

```haskell
instance
  ( HasServer (ToServantApi api)
  , forall m. GServer api m
  ) => HasServer (NamedRoutes api) where
  …

  hoistServer
    :: forall m n. Proxy (NamedRoutes api)
    -> (forall x. m x -> n x)
    -> api (AsServerT m)
    -> api (AsServerT n)
  hoistServer _ nat =
    case (gServerProof @api @m, gServerProof @api @n) of
      (Dict, Dict) -> fromServant . hoistServer nat . toServant
```

## Conclusion

Using records to define Servant routes has been a tantalising option
since `servant-generic` was introduced. Unfortunately it was
finicky to use and most reverted to use anonymous routes.

There was a missing piece to the `servant-generic` puzzle which was
completed with `NamedRoutes`, the implementation of which is
deceptively subtle.

Hopefully, I convinced you that using records to declare Servant APIs has become
a very pragmatic choice in Servant 0.19.

Should you have any kind of feedback, don't hesitate to open a ticket on our
[issue tracker](https://github.com/haskell-servant/servant/pulls)!
