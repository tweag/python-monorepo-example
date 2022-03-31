---
title: "How I stopped worrying and learned to run wasm and native hybrid code"
shortTitle: "Running wasm modules as if they are native executables"
author: Cheng Shao
tags: [webassembly]
description: "Introduce the trick we use to run ghc test suite that needs to exec wasm code"
---

As a part of our ongoing efforts to add WebAssembly support for GHC,
we needed to run the GHC test suite and look for interesting errors to
fix. This blog post introduces a trick I discovered, that allows
running the unmodified GHC test suite while treating WebAssembly
modules like native executables transparently. The trick may come in
handy if one needs to run a combination of host/guest hybrid code in
their codebase.

## A WebAssembly module as an executable

Let's first clarify why we can treat a WebAssembly module as an
executable file.

Despite the "Web" prefix in its name, the WebAssembly core
[specification][wasm-spec] isn't related to browsers at all; it merely
defines a bytecode format that describes some computation. The only
state that wasm code may query or modify are the linear memory and
globals.

Pure WebAssembly is already useful as a numerical computation kernel.
To go beyond this use case, WebAssembly needs access to side effects:
check the system clock, write to the console, fire missiles or
whatever. These side effects can be specified as WebAssembly imports,
which reside in the same namespace of ordinary WebAssembly functions,
conform to the same typing rules, but are backed by host functions
under the hood. This is a core design principle of WebAssembly:
security through capabilities.

What imports should a WebAssembly module contain? This is a matter of
ABI(Application Binary Interface) design, and is closely related to
the embedders and specific use cases. Outside of the core
specification, there is a standard called
[WASI][wasi-snapshot](WebAssembly System Interface) that specifies the
imports to access the "real world" like file system, sockets, clocks,
etc.

If the toolchain emits WebAssembly modules that only use WASI imports,
then these modules can be executed by any wasm engine that conforms to
the WASI spec. In addition to those imports, WASI has an [Application
ABI][wasi-app-abi] that defines a "command module" containing a main
entry function. This is the kind of WebAssembly "executable" we're
talking about: a module that is invoked once and runs to finish, while
having access to the console, file system and any other resources that
are explicitly provided by the engine.

## GHC test suite and cross compilation

```sh
$ /opt/wasi-sdk/bin/clang hello.c -o hello.wasm
$ wasmtime run hello.wasm
Hello world!
```

Here, we've compiled a C program to a self-contained wasm module using
[`wasi-sdk`][wasi-sdk], and ran it using [`wasmtime`][wasmtime]. Our
work-in-progress `wasm32-wasi-ghc` is based on `wasi-sdk`, and can
already emit wasm modules that run fine on `wasmtime`.

Naturally, we want to run the entire GHC [test suite][ghc-test-suite]
in a similar manner: first compile a test case to a `wasm` module,
then use `wasmtime` to run it, finally compare the wasm standard
output/error against expected files. Now we got a problem: the GHC
test suite doesn't know anything about cross compilation or emulators!
It assumes GHC will always emit a native executable that can be
spawned as a child process.

Refactoring that Haskell/Python/Makefile chimera
codebase and adding support for guest code emulators is definitely a
huge amount of [work][ghc-cross-test-suite]! In the future
we may revisit this rabbit hole, but now
we just want to run the whole thing without too much effort.

Does there exist such a shortcut?

## Potential solutions

If the test suite driver expects GHC to emit a native executable, then
just do it! We may take the wasm linker's output module as a starting
point, and find a way to generate a native executable that embeds the
wasm module and performs wasm execution.

We quickly ruled out this first potential solution for a few reasons:

- No easy way to extract wasm from the finally generated native
  executable! We can still run wasm by running that executable, but we
  may also want to check the wasm disassemble results, or try
  different wasm engines.
- It's rather complex, requires hacking the linking step, and likely
  slows it down.

There is some good news! Linux kernel has a feature called
[`binfmt_misc`][binfmt_misc] which allows us to use file extensions or
magic numbers to identify guest executables, then pass them to
user-specified emulators. This allows "mixed-mode" program execution,
where host executables may run guest executables as child processes
transparently.

Is `binfmt_misc` the perfect solution we're looking for? Unfortunately
not:

- Registering an emulator via `binfmt_misc` requires modifying
  `/proc/sys/fs/binfmt_misc`, a system-global state. It doesn't work
  if we don't have `root` privilege.
- System-wide designation of emulators may be undesirable. For
  instance, we may want to kick off two GHC test suite runs at once,
  using either `wasmtime` or `wasmer` as emulator, and compare the
  results. This can't be achieved via `binfmt_misc`.

Ultimately, we're seeking a solution that:

- Doesn't require any hacking in the GHC linking logic or the GHC test
  suite driver
- Doesn't need `root` privilege, works in a sandboxed nix build

## `proot` to the rescue

Yes, [`proot`][proot] can do that! A lot of people have used it to
simulate `chroot` in userspace, but it also implements a lesser-used
functionality: simulate `binfmt_misc` in userspace.

`proot` uses [`ptrace`][ptrace] to intercept system calls of the first
process and all its children. To create a child process from another
executable, one of the `exec` system calls needs to be invoked, at
which point `proot` will intercept it, check whether it's a host
executable, and if not, use an emulator specified via `proot` command
line argument `-q` to run it.

`proot` expects the emulator to be a [QEMU user mode][qemu-user-mode]
emulator, and it passes some QEMU-specific flags which won't make
sense to `wasmtime`. We can workaround this by writing a C wrapper:

```c
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  char *wasmtime_argv_init[] = {
      "wasmtime",
      "run",
  };
  int wasmtime_argv_init_length = (sizeof(wasmtime_argv_init) / sizeof(char *));

  // args before argv[5] are qemu-specific bits
  // arg[5] is "executable name" which is wasm filename
  char *wasmtime_argv[wasmtime_argv_init_length + argc - 5 + 1];

  for (int i = 0; i < wasmtime_argv_init_length; ++i) {
    wasmtime_argv[i] = wasmtime_argv_init[i];
  }
  for (int i = 5; i <= argc; ++i) {
    wasmtime_argv[wasmtime_argv_init_length + i - 5] = argv[i];
  }

  return execvp("wasmtime", wasmtime_argv);
}
```

```sh
$ cc qemu-system-wasm32.c -o qemu-system-wasm32
$ proot -q ./qemu-system-wasm32
$ ./hello.wasm
Hello world!
```

The `qemu-system-wasm32` wrapper discards QEMU-specific flags,
construct a proper argument list, then invokes `wasmtime` for actual
execution. It works with `proot` for this simple example. The real
`qemu-system-wasm32` wrapper used in our tests is slightly more
complex, since it needs to add other `wasmtime` arguments that set up
filesystem mappings and environment variables.

Once we have compiled our `qemu-system-wasm32` wrapper, we just need
to start running `proot -q qemu-system-wasm32 hadrian test ...`, then
leave our chairs for a coffee break. The GHC test suite will call GHC
to emit many wasm modules, and those modules will be transparently run
by `wasmtime`, without needing to hack the test suite driver!

## Conclusion

Running a hybrid of host/guest programs transparently is definitely
possible, and can even be done without relying on `binfmt_misc`, as
described in this post. However, `proot` has its own caveats:

- It assumes there is only a single guest platform, so we can't pass
  multiple emulators to run executables of different platforms.
  Ideally, the `binfmt_misc` configuration format can be reused here.
- It assumes the emulator executable is itself a proper ELF
  executable, and not a shebang-based script. This made us mess with
  strings in C, which is not a pleasant experience.
- It assumes the emulator is a QEMU user mode emulator, which
  complicates our wrapper logic a bit.

If we want to spend some more time for improvement, we can roll a
similar `binfmt_misc` userspace emulation tool that's also based on
`ptrace`, but addresses the pain points above. To spend even more
time, we should still teach the GHC test suite about cross compilation
and emulators. However, getting here is enough for us to look for
interesting errors in the test suite runs, and we hope our trick can
somehow be useful to you if you face a similar challenge :)

[binfmt_misc]: https://en.wikipedia.org/wiki/Binfmt_misc
[ghc-cross-test-suite]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3652
[ghc-test-suite]: https://gitlab.haskell.org/ghc/ghc/-/tree/master/testsuite
[proot]: https://proot-me.github.io/
[ptrace]: https://man7.org/linux/man-pages/man2/ptrace.2.html
[qemu-user-mode]: https://www.qemu.org/docs/master/user/main.html
[wasi-app-abi]: https://github.com/WebAssembly/WASI/blob/main/legacy/application-abi.md
[wasi-sdk]: https://github.com/WebAssembly/wasi-sdk
[wasi-snapshot]: https://github.com/WebAssembly/WASI/blob/main/phases/snapshot/docs.md
[wasm-spec]: https://webassembly.github.io/spec/core/
[wasmtime]: https://wasmtime.dev/
