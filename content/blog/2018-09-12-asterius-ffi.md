---
redirect_from: [/posts/2018-09-12-asterius-ffi.html]
title: "Haskell WebAssembly calling JavaScript and back again"
shortTitle: Asterius gets a JavaScript FFI
author: Shao Cheng
tags: [haskell, asterius]
---

_Note: since Mar 19, 2020, we've changed the JavaScript import syntax: the
`i`-th argument is now `$i` instead of `${i}`. The code snippets in this post
have been adjusted accordingly._

[Previously][hello-asterius], we announced the [Asterius](https://github.com/tweag/asterius) compiler, a new GHC-backend that translates Haskell to WebAssembly. Since then, we made a lot of progress; not just by supporting more of Haskell's language features, but also by improving interoperability. Today, we are proud to introduce a critical new feature: Haskell-JavaScript interop via a dedicated foreign function interface (FFI). Why is this important? It means we can finally interact with browser's DOM to in the future create full webapps.

In the style of Haskell's standard FFI, all you need to provide are simple Haskell import & export declarations. To the best of our knowledge, Asterius is the first compiler for a high-level FP language targeting WebAssembly that has reached this critical milestone.

## Haskell calling JavaScript

For any compiler targeting WebAssembly, a natural question arises: how to interact with the JavaScript world and perform side effects — for example to update the browser DOM? In general, applications use application-specific API's and frameworks. Legacy applications want to use existing API's like SDL2 or Cairo to draw inside the browser window. Elm-style web apps are expressed as pure functions transforming state to state. Some applications might cut through all the layers of abstraction and call web API's directly. What all these applications have in common is that at the lowest level of the abstraction stack, there needs to be a common bridge mechanism to call JavaScript from the host language of the application (be it C, Elm or Haskell). This post is about the low-level bridge mechanism.

The spec for the Haskell language already provides for a general
bridging mechanism: `foreign import` and `foreign export`
declarations. We just need to teach the compiler how to handle these
declarations when they're about JavaScript code.

Here is what Asterius lets you do today:

```Haskell
import Control.Monad

foreign import javascript "Math.random()" js_random :: IO Double
foreign import javascript "console.log($1)" js_print_double :: Double -> IO ()
foreign import javascript "new Date()" js_current_time :: IO JSRef
foreign import javascript "console.log($1)" js_print :: JSRef -> IO ()

main :: IO ()
main = do
  replicateM_ 5 $ js_random >>= js_print_double
  js_current_time >>= js_print
```

If you would like to try this at home, put the above snippet in a file
called `jsffi.hs` in the current directory. Then, you can invoke
`ahc-link` to compile it to `.wasm`/`.js` files (using the pre-built
Asterius Docker images, as explained in
the [Asterius documentation][asterius-docs]):

```
$ docker run -it --rm -v $(pwd):/mirror terrorjack/asterius
root@6be395fd9d1e:~# ahc-link --input /mirror/jsffi.hs --run
[INFO] Loading boot library store from "/root/.stack-work/install/x86_64-linux/ghc-8.7/8.7.20180822/share/x86_64-linux-ghc-8.7.20180822/asterius-0.0.1/.boot/asterius_lib/asterius_store"
[INFO] Populating the store with builtin routines
[INFO] Compiling /mirror/jsffi.hs to Cmm
[INFO] Marshalling from Cmm to WebAssembly
[INFO] Marshalling "Main" from Cmm to WebAssembly
[INFO] Attempting to link into a standalone WebAssembly module
[INFO] Invoking binaryen to marshal the WebAssembly module
[INFO] Validating the WebAssembly module
[INFO] Serializing the WebAssembly module to the binary form
[INFO] Writing WebAssembly binary to "/mirror/jsffi.wasm"
[INFO] Writing Node.js script to "/mirror/jsffi.js"
[INFO] Running /mirror/jsffi.js
0.9698822149494266
0.7414732842838012
0.8133696271413504
0.4627748238958229
0.06512700662524917
2018-09-12T09:34:43.088Z
```

We can use the `foreign import javascript` syntax directly, without enabling any additional GHC extensions. The string after the keyword `javascript` needs to be a JavaScript expression, where we use positional arguments `$1`, `$2`, and so forth to refer to the imported function's arguments. The result type of an imported function may be `IO` wrapped, depending on whether the underlying computation is pure and can be safely cached. Our current implementation supports a variety of _primitive types_, such as, `Bool`, `Char`, and `Int` as argument and result types of imported functions. See the [reference documentation](https://tweag.github.io/asterius/jsffi/) for a full list.

Besides primitive types, Asterius supports importing JavaScript references represented by values of type `JSRef` in Haskell land. What is a `JSRef`? After all, the [WebAssembly MVP spec](https://github.com/WebAssembly/design/blob/master/MVP.md) only supports marshalling integers and floating point values.

Under the hood, `JSRef`s are really just `Int`s. The Asterius runtime maintains a table mapping `JSRef`s to actual JavaScript objects. We use the table indices to represent those objects and pass them across the JavaScript-WebAssembly boundary instead. When the runtime invokes the JavaScript computation in a `foreign import javascript` declaration, it decides, based on the type information, whether to pass arguments and the result in their raw form or whether to use an object in the `JSRef` table.

## Marshalling more advanced types

At this point, you may be wondering how we can marshal more advanced types, such as strings and arrays. We are going to take a page out of the standard Haskell FFI playbook and implement more complex marshalling in plain Haskell by building on top of the primitive types supported by plain `foreign import javascript` declarations, entirely without any further, specialised support by the runtime. For example, here is the code to marshal strings:

```Haskell
type JSString = JSRef

toJSString :: String -> JSString
toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

fromJSString :: JSString -> String
fromJSString s = [js_string_tochar s i | i <- [0 .. js_length s - 1]]

foreign import javascript "\"\"" js_string_empty
  :: JSRef

foreign import javascript "$1.concat($2)" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "$1.length" js_length
  :: JSRef -> Int

foreign import javascript "String.fromCodePoint($1)" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "$1.codePointAt($2)" js_string_tochar
  :: JSRef -> Int -> Char
```

Here, we are implementing utility functions for converting between a Haskell `String` and a JavaScript string. Since `Char` is a JavaScript FFI primitive type, supported directly by the JavaScript-WebAssembly bridge, we simply traverse the string, marshalling it character by character and re-assembling it at the other end. Similarly, we can also convert between Haskell lists and JavaScript arrays as well as between Haskell records and JavaScript objects.

## JavaScript calling Haskell

A Haskell-to-JavaScript FFI is not sufficient. We also need to be able to go back. Asterius supports this in two ways.

Firstly, we have `foreign export javascript` declarations. They export a top-level binding as a JavaScript function, much like its cousin `foreign export ccall`, but also supporting `JSRef` as a primitive type. Here is a simple example:

```Haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

This exposes Haskell's multiplication on plain `Int`s as a WebAssembly function `mult_hs`. Now, we just need to be able to call it from JavaScript as well.

All our previous examples assumed the existence of a function `Main.main`. Then, the linker generates a `.js` file that (1) initialises the runtime, (2) runs `Main.main`, and (3) finally exits. When we invoke Haskell code from JavaScript, we cannot rely on this flow of control. In the worst case, JavaScript could try to invoke Haskell code before the Haskell runtime has been initialised. That would sure lead to undesirable behaviour. Hence, we need to inject some custom code at the point where the WebAssembly code has been successfully compiled and instantiated.

The tool `ahc-link` provides a flag `--asterius-instance-callback=`, which takes a JavaScript callback function, which is to be called once an _Asterius instance_ has been successfully initiated. An Asterius instance contains the instantiated WebAssembly module together with mappings from Cmm (GHC's C-like intermediate language) symbols to addresses. Hence, JavaScript code can call exported functions by way of accessing this symbol mapping. Continuing with the above example, in order to call `mult_hs` in JavaScript, the callback that we need to supply would be:

```JavaScript
i => {
    i.wasmInstance.exports.hs_init();
    console.log(i.wasmInstance.exports.mult_hs(6, 7));
}
```

`i.wasmInstance` is the instantiated `WebAssembly.Instance`. We call `i.wasmInstance.exports.hs_init()` to initialise the runtime first before any Haskell computation occurs. After that, we can call any exported function or `main` as many times as we want.

The `--asterius-instance-callback=` flag is suitable for the scenario where we expect that all logic is contained in the JavaScript file output by `ahc-link`. However, this is not always the case. Instead, for a more seamless interaction with other JavaScript libraries, we may wish to encapsulate the Asterius instance and invoke `hs_init` in advance. In that case, it is hard to contain all logic in one JavaScript callback. For now, this remains a limitation as we continue to improve the JavaScript FFI.

Additionally, we need to supply `--export-function=mult_hs` to `ahc-link` in this example, since `Main.main` does not have a transitive dependency on `mult_hs`, without this flag, the function will be stripped from the output WebAssembly binary.

## Using Haskell closures as JavaScript callbacks

The discussed `foreign export javascript` declarations are sufficient when all Haskell functions to be called from JavaScript are statically known. However, we often want to produce closures at runtime (e.g., by partially applying curried functions), and then, export such dynamic runtime-generated closures for use in JavaScript. For instance, when providing a Haskell closure as a JavaScript event handler, the handler often captures some contextual info as free variables, which are unknown at compile time.

We might want to work around that by adding the runtime context as a separate argument to an exported function. Then, the JavaScript code would be in charge of providing the right context when invoking a Haskell function. However, this would be a step back from the convenience of a language with first-class functions and would require substantial boilerplate code. Instead, we want to pass closures directly.

Again, we follow the approach of the standard Haskell FFI, and much as we represent JavaScript references in Haskell via `JSRef` values and a table, we use `StablePtr`s to refer to Haskell closures in JavaScript. GHC’s `StablePtr`s are also table indexes, which serve as a handle to a Haskell object on the heap, which can be passed between Haskell and C, or in our case, Haskell and JavaScript. We can't pass raw addresses, since the storage manager may move objects around. Hence, the `StablePtr` API also maintains a table of heap objects and we use table indexes to refer to them. This enables garbage collection to move objects around, as long as it takes care to also update the `StablePtr` table.

The Asterius JavaScript FFI supports `StablePtr a` as a primitive type, and as usual, we can use `Foreign.StablePtr.newStablePtr` to turn any Haskell closure into a `StablePtr`. However, we cannot directly pass a `StablePtr` to a JavaScript function that expects a callback. Instead, we first need to convert a `StablePtr` into a `JSRef` pointing to a valid JavaScript function which re-enters the Asterius runtime and triggers Haskell evaluation when called.

The Asterius runtime provides special interfaces for this purpose: `makeHaskellCallback` and `makeHaskellCallback1`. They convert arguments of type `StablePtr (IO ())`and `StablePtr (JSRef -> IO ())` into `JSRef`s referring to proper JavaScript functions, which can directly be used as event handlers, etc. This interface can be imported into Haskell like this:

```Haskell
foreign import javascript "__asterius_jsffi.makeHaskellCallback($1)" js_make_hs_callback
  :: StablePtr (IO ()) -> IO JSRef

foreign import javascript "__asterius_jsffi.makeHaskellCallback1($1)" js_make_hs_callback1
  :: StablePtr (JSRef -> IO ()) -> IO JSRef
```

Now, let's put together a complete example using a Haskell closure inside a JavaScript event handler:

```Haskell
import Foreign.StablePtr

foreign import javascript "Math.random()" js_random :: IO Double

foreign import javascript "console.log($1)" js_print_double :: Double -> IO ()

foreign import javascript "__asterius_jsffi.makeHaskellCallback($1)" js_make_hs_callback
  :: StablePtr (IO ()) -> IO JSRef

foreign import javascript "process.on(\"beforeExit\",$1)" js_process_beforeexit
  :: JSRef -> IO ()

main :: IO ()
main = do
  x <- js_random
  newStablePtr (js_print_double x) >>= js_make_hs_callback >>=
    js_process_beforeexit
```

When this example runs, `Main.main` first obtains a random number `x`, then converts the (as of yet unevaluated computation) `(js_print_double x)` into a `StablePtr (IO ())`, then into a `JSRef`, and finally sets it as a handler of the `beforeExit` event of the `node` process. Before the `node` process shuts down, it invokes the handler, which re-enters the Asterius runtime, evaluates the Haskell closure, which in turn invokes `js_print_double` to print the random number we obtained earlier. This is obviously a contrived example, but it does demonstrate the ability to use runtime-computed Haskell closures within JavaScript callbacks.

## What's next?

There are limitations still to the current JavaScript FFI:

- Currently, the marshalling of large objects, such as strings and arrays is fairly costly, as it requires many function calls between Haskell and JavaScript. Hence, we plan to provide custom marshalling for common bulk types (such as, `ByteString`, `Text`, and even `aeson` `Value`s) in the runtime.
- The standard Haskell FFI allows directly marshalling primitive types wrapped in `newtype` declarations. Asterius currently lacks that capability, but we plan to add it in a later version. This would allow us to e.g. make `JSString` a newtype wrapper around `JSRef` above.
- We did not discuss properly freeing `JSRef`s and `StablePtr`s. Besides manual freeing, we plan to look into the usual support for finalisers as well as a `ResourceT`-like mechanism that frees `JSRef`s upon exiting a scope. Even better, we should be able to use GHC's experimental support for [linear types][linear-types].
- We need to consider exception handling at language boundaries and ideally propagate them transparently.
- Finally, we need to look into using other JavaScript packages from Haskell and wrapping Haskell libraries transparently as JavaScript packages?

However, the JavaScript FFI as it stands is already sufficient to
build fairly complete web apps. That will be the topic of our next installment in this series. Stay tuned!

[hello-asterius]: https://www.tweag.io/posts/2018-05-29-hello-asterius.html
[emscripten]: https://github.com/kripken/emscripten
[asterius-docs]: https://tweag.github.io/asterius/
[linear-types]: https://www.tweag.io/posts/2017-03-13-linear-types.html
