---
redirect_from: [/posts/2019-12-19-asterius-diagrams.html]
title: "Haskell art in your browser  with Asterius"
shortTitle: "Art in browser with Asterius"
author: Sylvain Henry (IOHK), Shao Cheng
tags: [asterius, haskell]
description: "Asterius compiles Haskell code into WebAssembly code to be executed in a browser or in Node.js. It has reached a new milestone by being able to compile the diagrams library and its dependencies."
---

_Note: since Mar 19, 2020, we've changed the JavaScript import syntax: the
`i`-th argument is now `$i` instead of `${i}`. The code snippets in this post
have been adjusted accordingly._

[Asterius][asterius] is an experimental GHC backend targeting WebAssembly, which
makes it possible to run Haskell code in your browser or in a Node.js web
service. Asterius has reached a new milestone: it can now compile the popular
[diagrams][diagrams] library for drawing with Haskell.

In recent months, Asterius has become a collaborative project with fixes and bug
reports from the community, and major contributions from [IOHK][iohk] in
addition to Tweag I/O.

In this post, we'll demonstrate how to run diagrams examples in the
browser. This is the culmination of a lot of groundwork, from better
Cabal support to implementing green threads and many basic concurrency
primitives. More on that later in the post.

## Hilbert in your browser

Our example is about generating and displaying SVG directly in the browser using
`diagrams`. We picked the [Hilbert curve][hilbert] example from
[diagrams's gallery][gallery]. To use it with Asterius, we just have to adapt code provided
in the gallery example as follows.

Let's start with the imports:

```haskell
import Asterius.Types
import Diagrams.Backend.SVG
import Diagrams.Prelude
```

We then define `hilbert` and `example`, exactly as in the original:

```haskell
hilbert :: Int -> Trail V2 Double
hilbert 0 = mempty
hilbert n =
  hilbert' (n - 1)
    # reflectY
    <> vrule 1
    <> hilbert (n - 1)
    <> hrule 1
    <> hilbert (n - 1)
    <> vrule (-1)
    <> hilbert' (n - 1)
    # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1 / 4)

example :: Diagram B
example = frame 1 . lw medium . lc darkred . strokeT $ hilbert 5
```

Next up is `showSVG`, an embedded fragment of JavaScript code that will be
executed in the browser. It's an [immediately invoked function expression](https://en.wikipedia.org/wiki/Immediately_invoked_function_expression)
that appends a `div` element with the given contents to the page body.

```haskell
foreign import javascript
   "(() => {                                    \
   \   const d = document.createElement('div'); \
   \   d.innerHTML = $1;                      \
   \   document.body.appendChild(d);            \
   \ })()"
   showSVG :: JSString -> IO ()
```

Finally, `main` uses standard `diagrams` code to generate an SVG file as a
`String`, then calls `showSVG` to display the element in the browser.

```haskell
main :: IO ()
main = do
  let opts = SVGOptions
        { _size = dims2D 400 400,
          _svgDefinitions = Nothing,
          _idPrefix = mempty,
          _svgAttributes = [],
          _generateDoctype = False
        }
      svg = renderDia SVG opts example
  showSVG (toJSString (show svg))
```

To compile and test this program, we turn it into a package with the help of a
Cabal file. Here is the contents of the `Hilbert.cabal` file:

```
cabal-version: 1.24

name:           Hilbert
version:        0.0.1
license:        BSD3
build-type:     Simple

executable Hilbert
  main-is: Hilbert.hs
  ghc-options: -Wall
  build-depends:
        base
      , text
      , diagrams
      , diagrams-svg
      , diagrams-lib
      , asterius-prelude
      , svg-builder
      , lucid-svg
  default-language: Haskell2010
```

As usual, the quickest way to get started with Asterius is to use our Docker
image:

```bash
$ docker run -it --rm -v $(pwd):/mirror -w /mirror terrorjack/asterius
asterius@hostname:/mirror$
```

This command pulls the `latest` tag of our Docker image, maps the current
working directory as a shared volume at `/mirror`, making it the working
directory of the new container, and then enters a `bash` session.

To build the `Hilbert` project, proceed as follows:

```bash
asterius@hostname:/mirror$ ahc-cabal new-update
# Short update time
asterius@hostname:/mirror$ ahc-cabal new-install . --symlink-bindir .
# Longer build time
```

`ahc-cabal` is a wrapper around the `cabal` executable, which supports almost
all `cabal` commands, including the legacy `v1` build commands and the nix-style
`new` build commands. Here we use `new-install` to build the `Hilbert`
"executable" along with all its dependencies, with each component installed into
the nix-style cabal store. After the build finishes, a `Hilbert` symbolic link
will appear in `/mirror`, which points to the `Hilbert` "executable" we've just
built.

Finally, we need to extract the WebAssembly & JavaScript artifacts
from the `Hilbert` file. In an earlier [post][todomvc], we used the
`ahc-link` wrapper to that effect, but `ahc-link` generates `wasm` and
`mjs` files from individual `.hs` files. Cabal, in contrast, outputs a
single executable file. So we need to use another wrapper, `ahc-dist`,
which generates `wasm` and `mjs` files from such an executable. Except
for the input, `ahc-link` and `ahc-dist` flags are the same:

```bash
asterius@hostname:/mirror$ ahc-dist --browser --input-exe Hilbert
[INFO] Converting linked IR to binaryen IR
[INFO] Running binaryen optimization
[INFO] Validating binaryen IR
[INFO] Writing WebAssembly binary to "./Hilbert.wasm"
[INFO] Writing JavaScript runtime modules to "."
[INFO] Writing JavaScript loader module to "./Hilbert.wasm.mjs"
[INFO] Writing JavaScript req module to "./Hilbert.req.mjs"
[INFO] Writing JavaScript entry module to "./Hilbert.mjs"
[INFO] Writing HTML to "./Hilbert.html"
```

The `--browser` flag indicates that we are targeting the browser
instead of Node.js. It generates the `.wasm` and `.mjs` files along with
an `.html` file which loads and runs the program. Outside the Docker
container, we can use a static web server to serve the artifacts and
load `Hilbert.html` into a browser tab. We recommend `warp` from
the [`wai-app-static`][wai-app-static] package:

```
$ warp -v
Serving directory [...] on port 3000 with ["index.html","index.htm"] index files.

$ firefox "localhost:3000/Hilbert.html"
```

Your browser should display the following image:

![image](./asterius-diagrams-Hilbert.png)

And [here][wasm-hilbert] is a precompiled version you can try right now in your
browser. (Due to an open [issue][safari-issue], this example cannot currently be
used in Safari.)

## A taste of how we got here

To support the example above and many others, we improved Asterius along a
number of dimensions over the last few months, each of which we aim to cover in
its own blog post in the near future:

- **Template Haskell support:** We now have partial TH support, which is enough
  to compile most packages. Splices are compiled to WebAssembly and executed in
  `node`, using pipes to communicate with the host `ahc` process, similar to the
  `iserv` remote interpreter of standard GHCi. The lack of TH support has been a
  major roadblock for Asterius as well as some other Haskell-to-Web solutions
  like [`haste`][haste], since many packages use TH, either via splices or
  annotations (e.g. the `HLINT` annotations), some of which are quite common in
  the dependency graphs of typical Haskell projects.
- **Concurrent runtime:** The Asterius runtime is now
  concurrent, with support for green threads. It supports
  preemptive scheduling of several threads, timers (`threadDelay`),
  `MVar`s and more.
- **`ahc-cabal`:** A lot more packages can be built with `ahc-cabal`. While
  `ahc-link` is still convenient for testing single-file `Main` programs,
  Asterius users can now structure their code as regular Cabal projects, and
  pull dependencies from Hackage.
- **Docker image with prebuilt Stackage packages:** To save time for users to set up
  a local Asterius installation and compile common dependencies, our prebuilt
  Docker image now also ships with around 2k prebuilt packages from a recent Stackage
  LTS snapshot. Due to factors like missing `cbits`, some of them won't work yet
  (e.g. `cryptonite`), but the pure Haskell packages like `diagrams` should work
  fine.
- **Cabal custom setup support:** A lot of packages use custom `Setup.hs` files to
  jailbreak the Cabal build system and practice all forms of dark arts. We now
  have partial support for custom setup which suffices to compile packages like
  `lens`.
- **Improved runtime performance:** A great advantage of having examples running
  in the browser is that we can use the browser-integrated devtools to spot
  performance problems or dig into runtime errors. For example, it helped us
  detect a problem where programs were spending much more time in the collector
  rather than the mutator. We fixed the issue and the garbage collection
  overhead is now much more acceptable.

[asterius]: https://github.com/tweag/asterius
[diagrams]: https://diagrams.github.io
[iohk]: https://iohk.io
[gallery]: https://diagrams.github.io/gallery
[hilbert]: https://diagrams.github.io/gallery/Hilbert.html
[todomvc]: https://www.tweag.io/posts/2018-12-20-asterius-todomvc.html
[wai-app-static]: https://github.com/yesodweb/wai/tree/master/wai-app-static
[wasm-hilbert]: https://tweag.io/wasm-hilbert
[safari-issue]: https://github.com/tweag/asterius/issues/401
[haste]: https://haste-lang.org
