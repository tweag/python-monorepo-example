---
title: "Calling into JavaScript in Haskell, and vice versa"
shortTitle: Asterius gets a JavaScript FFI
author: Shao Cheng
---

[Previously][hello-asterius], we announced the [asterius](https://github.com/tweag/asterius) compiler which compiles Haskell to WebAssembly. The project has evolved a lot since then, and in this post, we'll talk about a critical new feature of asterius: the JavaScript FFI mechanism.

# Calling into JavaScript in Haskell

For any compiler which targets WebAssembly, a natural question arises: how to interact with the JavaScript world and perform side-effects? There are several possible answers:

1. For C-like low-level languages, emulate a C runtime in JavaScript, either by hand-writing JavaScript or porting a lightweight C runtime like musl. User code relies on a comprehensive "standard library" to perform side-effects. This is the approach taken by Emscripten.
2. User code focuses on implementing state updates, so it is pure in nature. The language runtime takes care of getting info from the "real world" and applying updates to it. This requires the whole project to be organized in a manner similar to the Elm architecture.
3. Allow user code to directly embed and call JavaScript code.

For maximum flexibility, asterius takes the third approach, and allow Haskell code to call JavaScript code directly via the `foreign import javascript` syntax. Here's a simple example:

```Haskell
import Control.Monad

foreign import javascript "Math.random()" js_random :: IO Double

foreign import javascript "console.log(${1})" js_print_double :: Double -> IO ()

foreign import javascript "new Date()" js_current_time :: IO JSRef

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

main :: IO ()
main = do
  replicateM_ 5 $ js_random >>= js_print_double
  js_current_time >>= js_print
```

Save it to somewhere like `~/mirror/jsffi.hs`, then we can invoke `ahc-link` to compile it to `.wasm`/`.js` files:

```
terrorjack@LAPTOP-A0NTIMCN:~$ docker run -it --rm -v ~/mirror:/mirror terrorjack/asterius
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

In order to call into JavaScript, we can use the `foreign import javascript` syntax directly, without enabling any GHC extension. The source text clause should be a JavaScript expression, but in the expression we can use constructs like `${1}`, `${2}` to refer to the import function's arguments. And users may choose to wrap the result in `IO` or not, depending on whether the underlying computation is "pure" and can be safely cached. Currently the JavaScript FFI support a variety of "basic types" like `Bool`/`Char`/`Int` as argument/result types of imported functions, see [documentation](https://tweag.github.io/asterius/jsffi/) for a full list.

Besides simple value types, asterius can import JavaScript references into Haskell as `JSRef` and passing them back to JavaScript. So what is a `JSRef`? The WebAssembly MVP only supports moving integers and floating point values anyway.

Under the hood, `JSRef`s are really just `Int`s. The asterius runtime maintains a table which maps `JSRef`s to real JavaScript objects. The table indices are passed across JavaScript/WebAssembly boundary just like an ordinary integer. When the runtime invokes computation in a `foreign import javascript` declaration, it decides whether to pass an argument/result in its raw form, or load/store in the `JSRef` table first and only pass the index.

# Marshaling more advanced types

It's also possible to marshal more advanced types like strings and arrays. Using the `foreign import javascript` mechanism described previously, the marshaling functions can be defined in user code without needing to modify the runtime:

```Haskell
type JSString = JSRef

toJSString :: String -> JSString
toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

fromJSString :: JSString -> String
fromJSString s = [js_string_tochar s i | i <- [0 .. js_length s - 1]]

foreign import javascript "\"\"" js_string_empty :: JSRef

foreign import javascript "${1}.concat(${2})" js_concat
  :: JSRef -> JSRef -> JSRef

foreign import javascript "${1}.length" js_length :: JSRef -> Int

foreign import javascript "String.fromCodePoint(${1})" js_string_fromchar
  :: Char -> JSRef

foreign import javascript "${1}.codePointAt(${2})" js_string_tochar
  :: JSRef -> Int -> Char
```

In the code above, we implement utility functions for converting between a Haskell `String` and a JavaScript string. Since `Char` is a JavaScript FFI basic type and can be moved between JavaScript/WebAssembly, we can scan a string from left to right, move the individual `Char`s and re-assemble it to a string at the other end. Similarly, we can convert between a Haskell list and a JavaScript array, a Haskell record and a JavaScript objects, etc.

# Calling into Haskell in JavaScript

Besides calling into JavaScript, we also need the ability to call into Haskell in JavaScript. There are two methods in asterius to achieve that purpose.

The first one is the `foreign export javascript` syntax. It allows exporting a top-level binding to a JavaScript function, and works mostly like its `foreign export ccall` cousin, with support for `JSRef` as a basic type:

```Haskell
foreign export javascript "mult_hs" (*) :: Int -> Int -> Int
```

We have exported the integer multiplier in Haskell as a WebAssembly export named `mult_hs`. Now we just need a way to call it as a JavaScript function.

All our previous examples assume the input Haskell module has `Main.main`, and the `.js` file the linker outputs initiates the runtime, run `Main.main` then exits. When calling into Haskell from JavaScript, this assumption doesn't always hold. Then we need to customize the behavior of that script after WebAssembly code is successfully compiled and instantiated.

`ahc-link` provides a `--asterius-instance-callback=` flag, which allows us to provide a JavaScript callback function which will be called with an asterius instance is successfully initiated. An "asterius instance" contains the instantiated WebAssembly module, and mappings from Cmm symbols to addresses, so JavaScript code can use symbols and call exported functions. In this example, in order to call `mult_hs` in JavaScript, the callback we supply would be:

```JavaScript
i => {
    i.wasmInstance.exports.hs_init();
    console.log(i.wasmInstance.exports.mult_hs(6, 7));
}
```

`i.wasmInstance` is the instantiated `WebAssembly.Instance`. We must call `i.wasmInstance.exports.hs_init()` to initialize the runtime first before any Haskell computation occurs. After that, we can call any exported function or `main` as many times as we want.

# Using Haskell closures as JavaScript callbacks

The `foreign export javascript` syntax is sufficient when the Haskell functions we'd like to export are all "static". However, we often want to produce closures at runtime (e.g. by partially applying curried functions), and export such "dynamic" closures for use in JavaScript. For instance, when providing a Haskell closure as a JavaScript event handler, the handler often captures some contextual info as free variables, which are unknown at compile time.

One simple workaround would be adding the runtime "context" as a separate argument for exported functions. The JavaScript code is in charge of initiating a context and threading it along through the Haskell functions. However, this denies the benifits of first-class functions and requires a lot of boilerplate code. So let's move on and see what we can do about this.

The first step is representing arbitrary Haskell closure in JavaScript. Remember how we represent JavaScript references in Haskell via `JSRef` and a table? The same method works the other way around, we use `StablePtr` to represent a Haskell closure in JavaScript. The `StablePtr` mechanism exists in GHC long before asterius, and it serves as a handle to a Haskell object on the heap which can be passed between Haskell/C. We can't pass raw addresses, since the storage manager may move objects around, so we need to maintain an index of objects, and modify the indexed address if needed during garbage collection.

The asterius JavaScript FFI supports `StablePtr a` as a basic type, and we can call `Foreign.StablePtr.newStablePtr` to turn any Haskell closure to a `StablePtr`. But we can't directly pass a `StablePtr` to a JavaScript function which expects a callback; we need to convert a `StablePtr` to a `JSRef` pointing to a valid JavaScript function which re-enters the asterius runtime and trigger Haskell evaluation when called.

The asterius runtime provides special interfaces for this purpose: `makeHaskellCallback`/`makeHaskellCallback1`. They convert arguments of type `StablePtr (IO ())`/`StablePtr (JSRef -> IO ())` to `JSRef`s of real JavaScript functions which can be used as event handlers, etc. This interface can be imported into Haskell like this:

```Haskell
foreign import javascript "__asterius_jsffi.makeHaskellCallback(${1})" js_make_hs_callback
  :: StablePtr (IO ()) -> IO JSRef

foreign import javascript "__asterius_jsffi.makeHaskellCallback1(${1})" js_make_hs_callback1
  :: StablePtr (JSRef -> IO ()) -> IO JSRef
```

Now, let's put together a complete example which uses a Haskell closure as a JavaScript event handler:

```Haskell
import Foreign.StablePtr

foreign import javascript "console.log(${1})" js_print :: JSRef -> IO ()

foreign import javascript "__asterius_jsffi.makeHaskellCallback1(${1})" js_make_hs_callback1
  :: StablePtr (JSRef -> IO ()) -> IO JSRef

foreign import javascript "process.on(\"beforeExit\",${1})" js_process_beforeexit
  :: JSRef -> IO ()

main :: IO ()
main = newStablePtr js_print >>= js_make_hs_callback1 >>= js_process_beforeexit
```

When this example is run, `Main.main` first converts `js_print` to a `StablePtr`, then to a `JSRef`, finally sets it as a handler of the `beforeExit` event of `node` process, then gracefully exits. Before `node` shuts down, it invokes the handler, which re-enters the asterius runtime and invokes `js_print` to print whatever passed to this handler (in this case, it's the expected process exit code `0`)

# Invoking RTS API directly in JavaScript

For the brave souls who prefer to play with raw pointers instead of syntactic sugar, it's possible to invoke RTS API directly in JavaScript. This grants us the ability to:

* Allocate memory, create and inspect Haskell closures on the heap.
* Trigger Haskell evaluation, then retrieve the results back into JavaScript.
* Use raw Cmm symbols to summon any function, not limited to the "foreign exported" ones.

Here is a simple example. Suppose we have a `Main.fact` function:

```Haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
```

The first step is ensuring `fact` is actually contained in the final WebAssembly binary produced by `ahc-link`. `ahc-link` performs aggressive dead-code elimination (or more precisely, live-code discovery) by starting from a set of "root symbols" (usually `Main_main_closure` which corresponds to `Main.main`), repeatedly traversing ASTs and including any discovered symbols. So if `Main.main` does not have a transitive dependency on `fact`, `fact` won't be included into the binary. In order to include `fact`, either use it in some way in `main`, or supply `--extra-root-symbol=Main_fact_closure` flag to `ahc-link` when compiling.

The next step is locating the pointer of `fact`. The "asterius instance" type we mentioned before contains two "symbol map" fields: `staticsSymbolMap` maps static data symbols to linear memory absolute addresses, and `functionSymbolMap` maps function symbols to WebAssembly function table indices. In this case, we can use `i.staticsSymbolMap.Main_fact_closure` as the pointer value of `Main_fact_closure`. For a Haskell top-level function, there're also pointers to the info table/entry function, but we don't need those two in this example.

Since we'd like to call `fact`, we need to apply it to an argument, build a thunk representing the result, then evaluate the thunk to WHNF and retrieve the result. Assuming we're passing `--asterius-instance-callback=i=>{ ... }` to `ahc-link`, in the callback body, we can use RTS API like this:

```JavaScript
i.wasmInstance.exports.hs_init();
const cap = i.staticsSymbolMap.MainCapability;
const argument = i.wasmInstance.exports.rts_mkInt(cap, 5);
const thunk = i.wasmInstance.exports.rts_apply(cap, i.staticsSymbolMap.Main_fact_closure, argument);
const ret = i.wasmInstance.exports.allocate(cap, 1);
i.wasmInstance.exports.rts_eval(cap, thunk, ret);
console.log(i.wasmInstance.exports.rts_getInt(i.wasmInstance.exports.loadI64(ret)));
```

A line-by-line explanation follows:

* As usual, the first step is calling `hs_init` to initialize the runtime.
* Most RTS API functions requires passing a `Capability` as the first argument, which can be thought as a single processor core for the virtual machine executing Haskell code. Since asterius only has a non-threaded runtime at the moment, we can simply use `MainCapability` as the pointer to the unique global `Capability`.
* Assuming we'd like to calculate `fact 5`, we need to build an `Int` object which value is `5`. We can't directly pass the JavaScript `5`, instead we should call `rts_mkInt`, which properly allocates a heap object and sets up the info pointer of an `Int` value. When we need to pass a value of basic type (e.g. `Int`, `StablePtr`, etc), we should always call `rts_mk*` and use the returned pointers to the allocated heap object.
* Then we can apply `fact` to `5` by using `rts_apply`. It builds a thunk without triggering evaluation. If we are dealing with a curried multiple-arguments function, we should chain `rts_apply` repeatedly until we get a thunk representing the final result.
* Before triggering evaluation, we need to allocate one single word, which serves as the "result pointer". All the `rts_eval*` functions expect a "result pointer", and upon successful evaluation, the result (which is yet another heap object)'s pointer will be written to the place pointed by the "result pointer". If we don't care about the result (e.g. `IO ()`), it's okay to pass `0` there.
* Finally, we call `rts_eval`, which enters the runtime and perform all the evaluation for us. There are different types of evaluation functions:
  * `rts_eval` evaluates a thunk of type `a` to WHNF.
  * `rts_evalIO` evaluates the result of `IO a` to WHNF.
  * `rts_evalLazyIO` evaluates `IO a`, without forcing the result to WHNF. It is also the default evaluator used by the runtime to run `Main.main`.
  * `rts_evalStableIO` evaluates the result of `StablePtr (IO a)` to WHNF, then return the result as `StablePtr a`.
* If we need to retrieve the result back to JavaScript, we must pick an evaluator function which forces the result to WHNF. The `rts_get*` functions assume the objects are evaluated and won't trigger evaluation.
* Finally, we use `loadI64` to retrieve the `Int` object stored in the space pointed by `ret`, then use `rts_getInt` to retrieve the content of that `Int`. The result is the integer value we expect.

Most users probably don't need to use RTS API manually, since the `foreign import`/`export` syntactic sugar and the `makeHaskellCallback` interface should be sufficient for typical use cases of Haskell/JavaScript interaction. Though it won't hurt to know what is hidden beneath the syntactic sugar, `foreign import`/`export` is implemented by automatically generating stub WebAssembly functions which calls RTS API for you.

# Future improvements to the JavaScript FFI

In this post, we demonstrated current capabilities of asterius JavaScript FFI. It's still in early stages, and there is surely a lot of room for improvement:

* Marshaling complex objects like strings and arrays is costly and requires calling into JavaScript a lot of times. This is easily fixed if we move the marshal functions from user code to the runtime, and this can be done for a lot of "common" types, ranging from `ByteString`s/`Text`s to even `Value`s in `aeson`!
* We haven't considered a simple reference leaking problem: in a long running application, we must not forget to properly free `JSRef`s or `StablePtr`s! Besides manual freeing, it shall be possible to:
  * Make use of `Weak#`s and attach finalizers to `JSRef`s, which will be run automatically when it is garbage collected, similar to `ByteString` handlers.
  * Use a `ResourceT`-like mechanism that upon exiting a scope, automatically frees a bunch of `JSRef`s. Or even better, use linear types.
* We haven't demonstrated exception handling of either Haskell or JavaScript. Ideally, the programmer don't need to manually check scheduler status codes, and automatically gets a JavaScript exception when a Haskell one is raised, or vice versa!
* Our examples only makes use of the JavaScript standard library. What if we want to use third-party `npm` packages? Or if we want to package a Haskell library as an `npm` package?
