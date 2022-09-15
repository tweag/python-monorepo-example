---
title: How to keep a Bazel project hermetic?
author: Mark Karpov
tags: [bazel, devops]
description: "A collection of helpful tips for Bazel users."
---

A build is _hermetic_ if it is not affected by details of the environment
where it is performed. Hermeticity is a prerequisite for generally desirable
features like [remote caching][remote-caching] and [remote
execution][remote-execution]. While certain build systems, such as Nix,
impose hermeticity through their design, others rely on their users to do
the extra work and be vigilant to get it. Bazel enforces hermeticity to some
extent, for example through sandboxing, but is less strict about it than
Nix. In this post I'm going to try to enumerate most ways in which
hermeticity of a Bazel project can be compromised.

## Execution strategy

One source of inhermeticity is the file system. If tools, such as compilers,
are invoked in a way that does not limit their access to contents of the
file system, the output of these tools can be influenced by extraneous files
that might be present during the build. One example could be include files
in languages like C or C++. Imagine a shared machine that is used to perform
builds with different configurations. One build might generate some header
files and place it in a directory that might later be specified as an
include directory in a compiler invocation performed by another build. If
the generated header file happens to have the right name it can shadow the
correct header file and lead to a build failure that is hard to reproduce
and understand. This is not a hypothetical example, but a real problem our
client once struggled with. This is why it is important to always use some
form of sandbox for your build actions. Sandboxing also guarantees that all
build inputs are declared correctly, because otherwise the input files will
simply not be available.

The use of sandbox is controlled by choosing an [_execution
strategy_][execution-strategy]. The following execution strategies are
available:

- `local` (or `standalone`, which is the same but deprecated) causes
  commands to be executed as local subprocesses without sandboxing.
- `sandboxed` causes commands to be executed inside a sandbox on the local
  machine.
- `worker` causes commands to be executed using a persistent worker, if
  available.
- `docker` causes commands to be executed inside a docker sandbox on the
  local machine.
- `remote` causes commands to be executed remotely; this is only available
  if a remote executor has been configured separately.

These are set with [`--spawn_strategy`][spawn-strategy] and
[`--strategy`][strategy] flags.

Without going into details of all the strategies mentioned, it must be noted
that `local` should be avoided if the build is to stay hermetic.
In addition to the strategy flags there are several ways to choose local
execution:

- Using a tag with special meaning such as [`"no-sandbox"`][no-sandbox-tag]
  or [`"local"`][local-tag].
- Setting the [`local` attribute][local-attribute] to `1` or `True`.

It should also be noted that, as of this writing, [Windows has no support
for sandboxing][windows-sandboxing]. Therefore build hermeticity on Windows
cannot be enforced at that level.

### With persistent workers

Another pitfall is related to the [`worker` strategy][persistent-workers].
While using persistent workers can have performance benefits, these workers
will not use sandboxed execution by default. It must be enabled manually by
using the [`--worker_sandboxing`][worker-sandboxing] flag.

## Environment

Environment variables can also be a source of inhermeticity. There are many
ways to inherit the environment of the machine that executes the build:

- Setting the [`use_default_shell_env`][use-default-shell-env] attribute to
  `True` in invocations of [`action.run`][action-run] or
  [`action.run_shell`][action-run-shell].
- Setting [`env_inherit`][env-inherit] to `True` in test attributes.
- Not using
  [`--incompatible_strict_action_env`][incompatible-strict-action-env].
- Using the [`--action_env`][action-env] flag to inherit the value of a
  given environmental variable. This option can also be used with the
  `--action_env=name=value` syntax. Extra care must be taken in that case to
  guarantee that `value` stays reasonably stable (e.g. it is not an absolute
  path which can vary from machine to machine).

Whenever the environment of host machine is inherited it becomes an input to
the respective build actions and since it is very hard to ensure identical
environments on different machines, especially developer machines, features like remote caching have no
chance to work.

## Rules

While most modern Bazel rules will provide a way to pin the toolchain that
is used for the build, others will default to simply picking up binaries
from the `PATH`. Nothing prevents these binaries to vary from machine to
machine. The built-in C and C++ rules are notorious for this kind of
behavior. It is worth paying attention to what kind of rules you are using
and what their guarantees with respect to hermeticity and reproducibility
are.

## Workspace status

Not a bug, but a feature—[workspace status][workspace-status] is in the gray
area with respect to hermeticity. Activated by the
`--workspace_status_command` command line option, it allows users to call an
arbitrary program before the build begins and then use its output to stamp
build results (e.g. status command could return git commit hash or time
stamp). If an action directly depends on the output of the status command,
typically stored as `bazel-out/stable-status.txt`, then it will likely be invalidated and rebuilt more often than intended and
not benefit much from remote caching. Extra care must be exercised so as to pick only
relevant bits of information from `stable-status.txt`, put them in a
separate file, and depend on that file only when truly necessary.

## Other things to watch for

Unfortunately, there is always a new way to shoot yourself in the foot. Here
are some examples:

- Repository rules can execute arbitrary code outside of the sandbox, they
  can potentially break hermeticity. For example, `pip_install` or
  `npm_install` may build native components with whichever compiler is in
  `PATH`, linking against whichever system libraries are found.
  Avoiding such dependencies, importing them in a reproducible way, for example through [rules_nixpkgs](https://github.com/tweag/rules_nixpkgs), or carefully controlling the environment during fetch may be solutions to this problem.
- Performing any non-deterministic actions. Creating archives (zip, tar,
  etc.) is a good example: The order of directory listings as well as
  timestamps are usually non-deterministic. The [reprodubile-builds project(https://reproducible-builds.org/docs/archives/) is a great resources to learn about these issues and how to circumvent them.

## Detecting hermeticity issues

In general, detecting hermeticity issues is hard. The best strategy, it
seems, is to attempt building your project in different environments and
have Bazel write execlogs. An execlog is the ground truth about what is
going on during the build. [This page about troubleshooting remote cache
hits][troubleshooting-cache-hits] describes how to make Bazel write
execlogs. Let's summarize it:

1. Execute `bazel clean` in order to force the subsequent build command to
   perform all necessary actions so that they end up in the execlog.
2. Execute `bazel build //your:target --execution_log_binary_file=/tmp/exec1.log`. This will produce a binary
   execution log.
3. Re-run the build (preceding it with a `bazel clean` invocation) in a
   different environment or even in the same environment if there is a
   reason to suspect that something could change between two runs in the
   same environment.
4. Compare execution logs following the instructions from [this
   section][comparing-execution-logs]. The procedure involves building a
   special parser that can convert binary execlogs produced by Bazel into
   text and then diffing the obtained text files with a tool like `diff`.
   Differences found in this way will reveal sources of inhermeticity.

With this approach the main question becomes “how to choose the environments
in which builds are performed so as to detect all hermeticity issues.” There
is no answer to this question that works in all cases. Varying host name and
user name might catch some problems, while others may only reveal themselves
in specific circumstances. If you already know what might be a source of
potential problems that could help with choosing the right build
environments for these tests. From a pragmatic point of view, choosing
environments that are already typically used to perform builds (remote
workers, build agents, local developer machines) is probably a good first
step.

## Conclusion

It is likely true that virtually all users of Bazel wish their builds be
hermetic. The blog post summarizes most ways in which hermiticity can be
violated and provides some suggestions about how to avoid the common
pitfalls and debug hermeticity issues.

[remote-caching]: https://bazel.build/remote/caching
[remote-execution]: https://bazel.build/remote/rbe
[execution-strategy]: https://bazel.build/docs/user-manual#execution-strategy
[spawn-strategy]: https://bazel.build/docs/user-manual#spawn-strategy
[strategy]: https://bazel.build/docs/user-manual#strategy
[no-sandbox-tag]: https://bazel.build/reference/be/common-definitions#common.tags
[local-tag]: https://bazel.build/reference/be/common-definitions#common.tags
[local-attribute]: https://bazel.build/reference/be/common-definitions#test.local
[windows-sandboxing]: https://github.com/bazelbuild/bazel/issues/5136
[persistent-workers]: https://bazel.build/docs/persistent-workers
[worker-sandboxing]: https://bazel.build/reference/command-line-reference#flag--worker_sandboxing
[use-default-shell-env]: https://bazel.build/rules/lib/actions#run.use_default_shell_env
[action-run]: https://bazel.build/rules/lib/actions#run
[action-run-shell]: https://bazel.build/rules/lib/actions#run_shell
[env-inherit]: https://bazel.build/reference/be/common-definitions#test.env_inherit
[incompatible-strict-action-env]: https://bazel.build/reference/command-line-reference#flag--incompatible_strict_action_env
[action-env]: https://bazel.build/reference/command-line-reference#flag--action_env
[workspace-status]: https://bazel.build/docs/user-manual#workspace_status
[troubleshooting-cache-hits]: https://bazel.build/docs/remote-execution-caching-debug#troubleshooting-cache-hits
[comparing-execution-logs]: https://bazel.build/docs/remote-execution-caching-debug#compare-logs
