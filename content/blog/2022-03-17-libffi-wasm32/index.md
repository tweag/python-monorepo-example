---
title: Porting libffi to pure WebAssembly
shortTitle: Porting libffi to pure WebAssembly
author: Cheng Shao
tags: [webassembly, asterius]
description: Cheng demonstrates why it's challenging to port libffi to wasm32, how he implemented it, and how it's useful for cross-compiling the GHC RTS
---

As a part of Tweag's ongoing [effort][ghc-wasm-wiki] to add
WebAssembly code generation GHC, we need to compile GHC's runtime
system to WebAssembly. The [`libffi`][libffi] library to pure
WebAssembly is an essential dependency of the GHC runtime: it is used
to pass Haskell functions as callback to C functions. As the
implementation of `libffi` depends on the platform, we've had to port
it to WebAssembly.

This blog post introduces `libffi`, the challenges to make it
work with WebAssembly, demonstrates our implementation, and also
explains how it's used by GHC runtime. I hope our implementation can
be useful for other people porting projects to WebAssembly (especially
`wasm32-wasi`).

## What `libffi` is about

`libffi` is a C library that provides an interface to perform indirect
function calls, where the function's type signature is only known at
run-time instead of compile-time. This is a common use case when
implementing an interpreter that supports calling C foreign functions
from the interpreted language. GHC's bytecode interpreter is an
example, we will need it to support running GHCi in WebAssembly.

Consider a minimal example. A C function `fib` is exported by the
dynamic library `libfib.so`:

```c
int fib(int n);
```

In order to call `fib` in their language, the user code would provide
the library name, the C function name, and the expected type
signature. Using the system's dynamic linker, it's easy to load the
specified library and obtain the code pointer that corresponds to
`fib`:

```c
// library & function names can't really be literals, this is just for simplicity
void *lib = dlopen("libfib.so", RTLD_LAZY);
void *fib = dlsym(lib, "fib");
```

Now, we have `fib` as an opaque code pointer. How do we invoke the
`fib` function, pass the arguments and obtain the result? Remember,
`fib`'s type signature is not known at compile-time of the
interpreter, so we can't cheat by merely coercing `fib` to the C
function pointer type and then performing the call:

```c
int arg = 5;
int res = fib(arg); // not gonna work
int res = ((int*)(int))fib(arg); // cheating!
```

This is where `libffi` comes to rescue. It allows you to construct
RTTI(run-time type information) for C functions, perform a function
call using that RTTI and a run-time allocated argument vector, then
obtain the result if there is any:

```c
ffi_cif cif; // the run-time type information
ffi_type *arg_tys[] = { &ffi_type_sint }; // the argument types
ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, &ffi_type_sint, arg_tys); // populate the run-time type information

int arg = 5;
void *arg_vals[] = { &arg }; // the argument pointer array
int res; // the result value
ffi_call(&cif, fib, &res, arg_vals); // perform the call

printf("fib(%d)=%d\n", arg, res);
```

## `libffi` and WebAssembly

The implementation of `ffi_call` is deeply platform-dependent. For
each supported platform, it needs to implement to the C ABI's calling
convention: given a function's type signature, where are the
arguments/result placed (in certain registers, and/or on the stack),
how to arrange the return code address, etc.

The problem is, WebAssembly [C ABI][wasm32-cabi] doesn't look like any
other platform!

- There are no global registers for passing arguments or result. Instead, a
  C function of type `void (int, int)` maps to a WebAssembly function
  of type `(i32, i32) -> nil` directly, each C argument is a
  WebAssembly function argument.
- The only way to jump to a function given the code address is the
  [`call_indirect`][wasm32-call_indirect] opcode. `call_indirect`
  requires specifying an expected function type at compile-time. It
  traps at run-time if the expected type doesn't match the actual type
  of the pointed function.

This makes it hard to port `ffi_call` to WebAssembly: even given the
type information at run-time, there's no places where we can move the
arguments/result around, and we can't jump to a function without
knowing it's type signature at compile-time (if we do, that defeats
the purpose of `libffi` in the first place!)

## Code generation to the rescue

Given a code pointer in WebAssembly, we need to know the correct type
signature at compile-time in order to perform an indirect call, but
that information is only available in the RTTI. But it should be
possible to do a run-time pattern match on the RTTI, then in each case
of that pattern match, we know the precise type signature at
compile-time:

```c
void ffi_call (ffi_cif *cif, void *fn, void *rvalue, void **avalues) {
  switch(cif->encoding) {
    // argument & result type is both a signed int
    case (encoding_of((ffi_type_sint)(ffi_type_sint))): {
      // cast to correct function pointer type, then perform the call
      *(ffi_type_sint*)rvalue = (((ffi_type_sint*)(ffi_type_sint))fn)(*(ffi_type_sint*)(avalues[0]));
      return;
    }
    // other function types follow suit
  }
}
```

This way, we avoid the need for fancy logic of moving arguments and result, and adjusting the
stack. We simply coerce the code pointer to the correct function pointer
type and perform the call. It's a pretty intuitive implementation, but
the devil is in the details:

- There are infinite numbers of
  possible type signatures. So we have to live
  with a limitation: restrict the maximum number of arguments to a
  small constant that's sufficient to cover our use cases. What more,
  The case count grows exponentially with maximum argument count.
  Suppose we have `k` non-void value types and no more than `N`
  arguments, then the case count would be `(k^N)*(k+1)`. So `N` must
  be very small (currently we chose `N=4`).
- `libffi`
  supports many value types that model different C types, so even a
  small `N` would require a prohibitively large number of cases!
  Well, according to the WebAssembly C ABI, these value types are
  mapped to one of the four WebAssembly value types, so, for us, `k` is really
  just `4`.
- How do we pattern match on the RTTI? We can encode each type signature
  to a distinct integer and store it in the `ffi_cif` struct. The
  `encoding_of` macro calculates the encoding value, so we can
  implement `ffi_call` with a
  single `switch` statement.

Despite the limitations, writing this dispatch code manually would be
incredibly tedious and error prone. It would also be terribly
difficult to change `N` if the need for larger functions occurs. So
we've implemented a [code generator][libffi-wasm32] for it in
Haskell.

## `libffi` usage in the GHC runtime

In GHC's runtime, `libffi` is used to
support dynamic foreign exports to C. Here is a minimal example:

```c
#include <Rts.h>

// instead of returning the result directly, we take a callback
// function pointer, which will be called with the result as argument.
void fib(HsWord x, void (*cb)(HsWord)) {
  HsWord a = 0, b = 1, c;
  for(int i = 2; i <= x; ++i) {
    c = a + b;
    a = b;
    b = c;
  }
  switch(x) {
    case 0: cb(0); return;
    case 1: cb(1); return;
    default: cb(c); return;
  }
}
```

```haskell
import Foreign

main :: IO ()
main = fib 10

fib :: Word -> IO ()
fib x = do
  cb <- mk_cb $ \r -> putStrLn $ "fib(" <> show x <> ") = " <> show r
  c_fib x cb
  freeHaskellFunPtr cb

foreign import ccall "fib" c_fib :: Word -> FunPtr (Word -> IO ()) -> IO ()

-- Special syntax implemented with libffi
foreign import ccall "wrapper" mk_cb :: (Word -> IO ()) -> IO (FunPtr (Word -> IO ()))
```

The GHC runtime needs to generate the `cb` C function pointer that
wraps a Haskell function, and that Haskell function is a dynamic
closure generated during program run-time. How?

1. When compiling `mk_cb`, GHC generates a C function with the `void () (ffi_cif *cif, void *ret, void **args, void *user_data)`
   prototype. The `cb` callback's type signature will be passed via
   `cif`, its argument/result will be passed via `args`/`ret`. But
   what does this function do, and what's `user_data`? Read on.
2. When `mk_cb` is called in Haskell, a `StablePtr` (immutable pointer
   to any Haskell value) is created and points to the passed Haskell
   function. The GHC runtime then invokes the `libffi` [Closure
   API][libffi-closure] to create the C callback function, passing the
   `StablePtr` as one of the arguments. The resulting function pointer
   is returned to `cb` in Haskell.
3. When `cb` is invoked in C, it calls the GHC-generated C function in
   Step 1. The `StablePtr` created in Step 2 is passed as the
   `user_data` argument, but keep in mind, `user_data` is _not_ a
   argument of `cb`, it was passed to Closure API when generating
   `cb`. Now, that function will locate the real Haskell function from
   `user_data` and then call into Haskell.

Admittedly, the example above is overly complicated for a `fib`
implementation in Haskell, but a lot of real-world C libraries do
expect users to pass C callbacks, so their Haskell bindings rely on
dynamic foreign exports. [Searching][hackage-export-dynamic] for
`foreign import ccall "wrapper"` on the entire Hackage yields
about 4500 matches across 200 packages. It suggests that dynamic
foreign exports is common and important enough for us to support. So
we need to implement `libffi` Closure API as well.

## Implementing the Closure API

The Closure API needs to return a C function pointer which points to a
function that "remembers" certain arguments passed to the Closure API,
but not to itself. This resembles the "closure" concept in functional
programming, where a function may be closed over some environment
value (`user_data` in our case), hence the "Closure API" name.

On platforms which support JIT (Just-In-Time) code generation, there's
a natural way to do it: allocate some executable memory, emit machine
code there and return it as the function pointer. We can either
hard-code the Closure API arguments into the function code, or access
those arguments somewhere else in the memory, either way won't be hard
to implement.

There do exist platforms that prohibit JIT.
In that case, `libffi` has a workaround called [static
trampolines][libffi-static-trampoline]: define a _pool_ of functions
to return, each of which has an associated memory location to record
Closure API arguments. When the function is called later, it reads
`cif` to decide its type and what registers correspond to its
arguments/result. This is similar to `ffi_call`, except `ffi_call`
uses the platform-specific C ABI knowledge as _caller_, but here it
uses that knowledge as _callee_.

Our Closure API implementation follows the idea of static trampolines.
However, the same challenge of implementing `ffi_call` arises again: we
can't have a single pool of functions for all possible types, instead,
for each C function type we support, we need to have a separate pool.
This also means that we can't "allocate" a function without knowing
`cif`, therefore we can only implement a modified version of Closure
API described below:

```c
ffi_status ffi_alloc_prep_closure(ffi_closure **pclosure, // ffi_closure records fun, cif, user_data
                                  ffi_cif *cif, // the resulting function pointer's expected type
                                  void (*fun)(ffi_cif *cif, void *ret, void **args, void *user_data), // the input function being wrapped into the closure
                                  void *user_data, // the closure's environment pointer
                                  void **code // the resulting function pointer
                                  ) {
  *pclosure = NULL;
  switch (cif->encoding) {
    case (encoding_of((ffi_type_sint)(ffi_type_sint))): {
      // XX is the encoded integer of the C function type
      // pass the metadata/function arrays for this type, and the pool size
      // return the metadata location directly, the function location indirectly
      *pclosure = ffi_pool_alloc(ffi_pool_closure_XX, ffi_pool_func_XX, 16, code);
      break;
    }
    // other function types follow suit
  }
  (*pclosure)->cif = cif;
  (*pclosure)->fun = fun;
  (*pclosure)->user_data = user_data;
  return FFI_OK;
}

// the XX type's metadata pool, zero-initialized
static ffi_closure ffi_pool_closure_XX[16];

// the XX type's pool, populated by pointers to generated functions
static void *ffi_pool_func_XX[16] = {
    ffi_pool_func_XX_0, ffi_pool_func_XX_1, ffi_pool_func_XX_2, ...
};

// one generated function in the pool
static ffi_type_sint ffi_pool_func_XX_2(ffi_type_sint a0) {
  void *args[] = {&a0}; // the argument pointer array
  ffi_type_sint ret; // the result value
  // perform the call
  ffi_pool_closure_XX[2].fun(ffi_pool_closure_XX[2].cif, &ret, args,
                                ffi_pool_closure_XX[2].user_data);
  return ret;
}
```

Similar to `ffi_call`, the Closure API C code is generated in Haskell.
When calling the Closure API, we do a `switch` table to pattern match
on the input type information. In each `case`, we know the
corresponding pools to allocate from. The returned function pointer
knows which `ffi_closure` it should be looking at, since each
generated function has a 1-to-1 mapping against the `ffi_closure`
pool.

Compared to the `ffi_call` API described earlier, we need to tune
another constant here: the pool size of each supported type. This
directly corresponds to how many times one may dynamically export a
Haskell function for that type. In typical use cases, there won't be
many, but in case of exceptions, it's easy to increase the pool size
only for a few specific types to avoid code bloating.

## Conclusion

Our `libffi` code generator is implemented in a flexible way: it's
easy to adjust the set of supported `libffi` functions types and the
trampoline pool size, and special cases can always be added to address
project needs.

There are some future improvements we have in mind:

- Measure the code size, link-time and run-time overhead when
  supporting different numbers of arguments.
- Test the basic/closure API on randomly generated function types.

[ghc-wasm-wiki]: https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-backend
[hackage-export-dynamic]: https://hackage-search.serokell.io/?q=foreign+import+ccall+%22wrapper%22
[libffi]: https://sourceware.org/libffi
[libffi-closure]: http://www.chiark.greenend.org.uk/doc/libffi-dev/html/The-Closure-API.html
[libffi-static-trampoline]: https://sourceware.org/pipermail/libffi-discuss/2021/002579.html
[libffi-wasm32]: https://gitlab.haskell.org/ghc/libffi-wasm
[wasm32-cabi]: https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md
[wasm32-call_indirect]: https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-instr-control
