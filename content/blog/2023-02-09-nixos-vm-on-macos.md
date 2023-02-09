---
title: "Running a NixOS VM on macOS"
author: Yuriy Taraday
description: |
  Make your NixOS development on macOS easier with this simple trick...
tags: [nix]
---

In this post I want to explore the current issues with developing parts of NixOS on
macOS and how we can make this task easier.

## Why would I want to run a NixOS virtual machine on macOS?

My colleague at Tweag, Dominic Steinitz, asked me this question after I shared
my first minor achievement in this area, and it struck me that I have never
described why exactly I run virtual machines (VMs) on my laptop and why I want
to make it easier for myself (and others).

Like many members of the Nix community – more than 25% according to the [Nix
community survey][2022_survey_report] – I rely on macOS to provide me a stable
shiny desktop environment. However NixOS is based on Linux and so many pieces
that are used for developing and testing it require running it in VMs. I'll
describe some major use cases for it below.

[2022_survey_report]: https://discourse.nixos.org/t/2022-nix-survey-results/18983
[local-remote-builder]: #local-remote-builder

### Local remote builder

macOS is a great desktop environment, but as soon as you move beyond your
machine, be it to a remote server, VM or container, you most likely end up in
Linux, which is a different system. Practically speaking, this means that
a derivation for any of them requires a Linux machine with Nix to be
built.

Therefore, you have to set up a [remote builder][remote_builder], either on
some other machine (in the cloud, some datacenter, or under your desk), or in a
VM on your machine. If you have a powerful enough machine, you might want to
opt for the latter, reaching for either running a complete Linux VM in
VirtualBox, UTM, Parallels or vmWare, or using [nix-docker][nix-docker] to run
it in Docker's VM or [linuxkit-nix][linuxkit-nix] to spawn a minimal Linux VM
with only Nix on board.

Most of these options require you to install some app that will manage VMs for
you and most of those are proprietary. As for [linuxkit-nix][linuxkit-nix],
[Gabriella Gonzales][gabriella439] wrote in her
[blog post][gabriella-blog-post] a great overview of how it is lacking in
different areas.

[nixpkgs_tiers]: https://github.com/NixOS/rfcs/blob/master/rfcs/0046-platform-support-tiers.md
[remote_builder]: https://nixos.org/manual/nix/stable/advanced-topics/distributed-builds.html
[nix-docker]: https://github.com/LnL7/nix-docker#running-as-a-remote-builder
[linuxkit-nix]: https://github.com/nix-community/linuxkit-nix

### Test NixOS configurations locally

Now that you can build and run packages for your target Linux system,
you might want to start deploying NixOS to remote machines.
When you are running Linux locally, you can easily run `nixos-rebuild build-vm`
to generate a script that will start a local QEMU virtual machine. This allows
you to verify if the configuration works without affecting any actual system.
It is very lightweight because it mounts your `/nix/store` directory directly
into the VM and boots the VM straight from it, with no additional image
overhead. You can even rely on it to wrap some services into a VM and configure
it to run as a daemon on your host by using the `system.build.vm` attribute of
any NixOS configuration. It uses a NixOS configuration variant from
`virtualisation.vmVariant` that imports the `virtualisation/qemu-vm.nix` module
to build the VM.

However, on macOS none of that will work. `nixos-rebuild` by default makes the
assumption that the local machine is Linux and links to Linux Bash and QEMU
binaries, so you can't run it locally. Hence, an alternative approach is
needed.

### Run NixOS tests locally

NixOS provides [a powerful framework][framework] for testing different
services and modules in VMs. You just provide it with a piece of NixOS
configuration for all machines that you need and it provides a neat API to run
them, interact with them and check their state. This framework is based on the
same mechanism as other NixOS VMs, so it doesn't work on macOS out of the box
either. Here, we also need an alternative.

[framework]: https://nixos.org/manual/nixos/stable/#sec-nixos-tests

## What has been done?

Since the conventional means of working on NixOS configurations relies on a
Linux host, multiple changes to the status quo are needed to make it
comfortable to work on them on macOS, presented in the following sections.

### Build VMs from NixOS configurations

The first issue that needed to be fixed is the lack of a simple interface for
building and running NixOS VMs on macOS. Starting with a Linux host before
migrating our workflow to macOS, you could have a NixOS configuration defined
in a flake like this:

```nix
{
  outputs = {
    self,
    nixpkgs,
  }: {
    nixosModules.base = {pkgs, ...}: {
      system.stateVersion = "22.05";

      # Configure networking
      networking.useDHCP = false;
      networking.interfaces.eth0.useDHCP = true;

      # Create user "test"
      services.getty.autologinUser = "test";
      users.users.test.isNormalUser = true;

      # Enable passwordless ‘sudo’ for the "test" user
      users.users.test.extraGroups = ["wheel"];
      security.sudo.wheelNeedsPassword = false;
    };
    nixosConfigurations.linuxBase = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.base
      ];
    };
  };
}
```

Note that this configuration lacks some key settings, like filesystems or
bootloader configuration required for it to be actually deployed, but there is
just enough to build a VM out of it.

Still on Linux, you can use `nixos-rebuild` to create a VM using this
configuration and run the result:

```shell
$ nixos-rebuild --flake .#linuxBase build-vm
building the system configuration...
warning: creating lock file '.../flake.lock'

Done.  The virtual machine can be started by running /nix/store/3ad15806lclvmfzxkppabncp5sq9i93n-nixos-vm/bin/run-nixos-vm
$ ./result/bin/run-nixos-vm
Formatting '.../nixos.qcow2', fmt=qcow2 cluster_size=65536 extended_l2=off compression_type=zlib size=1073741824 lazy_refcounts=off refcount_bits=16
```

It will start a VM, with an external window emulating its graphical terminal
screen, where the test user will be automatically logged in and at their
shell prompt. It's easier to follow this article along without switching
between the terminal and external windows, so let's change that in a separate
module specific to the VM in our flake. We'll instruct `vmVariant` of our NixOS
configuration to not use graphics for virtualisation. That would force it to
use a text based serial terminal output linked directly to the terminal from
which we're running the script.

```nix
{
  outputs = {...}: {
    # ...
    nixosModules.vm = {...}: {
      # Make VM output to the terminal instead of a separate window
      virtualisation.vmVariant.virtualisation.graphics = false;
    };
    nixosConfigurations.linuxVM = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.base
        self.nixosModules.vm
      ];
    };
  };
}
```

Now let's build it with `nixos-rebuild` to run it in our terminal this time:

```shell
$ nixos-rebuild --flake .#linuxVM build-vm
building the system configuration...
warning: Git tree '.../blog' is dirty

Done.  The virtual machine can be started by running /nix/store/f9a3ac5ydcm6anv9zpf2sqykzivwccaw-nixos-vm/bin/run-nixos-vm
$ ./result/bin/run-nixos-vm
SeaBIOS (version rel-1.16.0-0-gd239552ce722-prebuilt.qemu.org)
...
[test@nixos:~]$ uname -a
Linux nixos 5.15.74 #1-NixOS SMP Sat Oct 15 05:59:05 UTC 2022 x86_64 GNU/Linux

[test@nixos:~]$ sudo poweroff
...
[   75.762105] reboot: Power down
```

Note that you can use `sudo poweroff` to turn the machine off, or the
`Ctrl-A X` QEMU shortcut. The latter might leave your terminal in a weird
state, so you might need to run `reset` to fix it.

Also note that I've created a Git repo and added `flake.nix` and `flake.lock`
files to it with `git init && git add flake.*`. Outside a Git repository, Nix
considers all files in the directory (including any temporary files, like
`nixos.qcow2` or `result`) relevant to the flake and pulls them into the build
process. When Nix detects that its target is a Git repository, it only
considers files that Git knows about and ignores all untracked files.

So far we've been using `nixos-rebuild` to build our VMs, but it doesn't
provide flexibility to build on other systems. Since we've imported the
`qemu-vm.nix` module, we can use its output to run the VM instead:

```shell
$ nix run .#nixosConfigurations.linuxVM.config.system.build.vm
warning: Git tree '.../blog' is dirty
SeaBIOS (version rel-1.16.0-0-gd239552ce722-prebuilt.qemu.org)
...
```

We could also add it as a package to our flake so that we don't have to spell
it out every time:

```nix
{
  outputs = {...}: {
    # ...
    packages.x86_64-linux.linuxVM = self.nixosConfigurations.linuxVM.config.system.build.vm;
  };
}
```

Now we can run it with a short command:

```shell
$ nix run .#linuxVM
warning: Git tree '.../blog' is dirty
SeaBIOS (version rel-1.16.0-0-gd239552ce722-prebuilt.qemu.org)
...
```

So far we haven't been doing anything that hasn't been possible on any old
version of Nixpkgs. Let's move to our macOS machine and try running this VM:

```shell
$ nix run .#linuxVM
warning: Git tree '.../blog' is dirty
error: flake '.../blog' does not provide attribute 'apps.aarch64-darwin.linuxVM', 'packages.aarch64-darwin.linuxVM', 'legacyPackages.aarch64-darwin.linuxVM' or 'linuxVM'
$ nix run .#nixosConfigurations.linuxVM.config.system.build.vm
warning: Git tree '.../blog' is dirty
error: a 'x86_64-linux' with features {} is required to build '/nix/store/297gh5gn4ihnd0av0qbfx14vg0azly8x-append-initrd-secrets.drv', but I am a 'x86_64-darwin' with features {benchmark, big-parallel, hvf, nixos-test}
```

We don't have an output named `linuxVM` that would work on `aarch64-darwin`,
and even if we were to make it, it would require a Linux builder to be built.
You can spin up a NixOS VM somewhere in a cloud or on your local machine and
[configure][remote_builder] your Nix daemon to use it. You can run it
locally using one of the methods described [above][local-remote-builder] or
read on to [the next section][bootstrapping-a-local-builder-for-linux] for
a new one.

Assuming you have a Linux builder configured, let's go one step further:

```shell
$ nix run .#nixosConfigurations.linuxVM.config.system.build.vm --options builders ssh-ng://some-linux-builder
warning: Git tree '.../blog' is dirty
/nix/store/f9a3ac5ydcm6anv9zpf2sqykzivwccaw-nixos-vm/bin/run-nixos-vm: line 7: /nix/store/4vjigg3pr8bns6id4af51mza5p73l9lx-coreutils-9.1/bin/readlink: cannot execute binary file
```

The error output suggests that you cannot run this on macOS, likely because
this output is only expected to be run on Linux.

Looking for a solution for this I found an old [issue][nixos-vms-issue]
where [Silvan Mosberger][infinisil] started a discussion about running NixOS
VMs on macOS and developed a [prototype solution][prototype]. It allows us to
specify a different package set for the host where the VM is supposed to be
running. Several patches to support using sharing directories from the host and
running on Apple Silicon hardware have landed in QEMU master since, and using
the 7.0.0 release plus [a couple patches][qemu-9p-fix] it finally worked. I've
[adapted][qemu-pkgs-pr] the prototype and [backported][qemu-9p-fix-backport]
the required QEMU fixes to 7.0.0 in Nixpkgs.

[nixos-vms-issue]: https://github.com/NixOS/nixpkgs/issues/108984
[infinisil]: https://github.com/infinisil
[prototype]: https://github.com/infinisil/nixpkgs/commit/4d244410ee0f3e3ece5494533217bbafbd95d9b3
[qemu-9p-fix]: https://gitlab.com/qemu/qemu/-/commit/f5643914a9e8f79c606a76e6a9d7ea82a3fc3e65
[qemu-pkgs-pr]: https://github.com/NixOS/nixpkgs/pull/180222
[qemu-9p-fix-backport]: https://github.com/NixOS/nixpkgs/pull/180221

With all these changes, we can specify `virtualisation.host.pkgs` to run a VM
directly on macOS:

```nix
{
  outputs = {...}: {
    # ...
    nixosConfigurations.darwinVM = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        self.nixosModules.base
        self.nixosModules.vm
        {
          virtualisation.vmVariant.virtualisation.host.pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        }
      ];
    };
    packages.aarch64-darwin.darwinVM = self.nixosConfigurations.darwinVM.config.system.build.vm;
  };
}
```

Note that I've changed it to `aarch64-linux` because I'm running on Apple
Silicon and want to take advantage of its hardware assisted virtualisation.
I've also set `virtualisation.host.pkgs` for `vmVariant` of this NixOS
configuration that is used to build the VM. Now let's run it:

```shell
$ nix run .#darwinVM
warning: Git tree '.../blog' is dirty
Formatting '.../blog/nixos.qcow2', fmt=qcow2 cluster_size=65536 extended_l2=off compression_type=zlib size=1073741824 lazy_refcounts=off refcount_bits=16
[    0.152640] armv8-pmu pmu: hw perfevents: failed to probe PMU!
...
[test@nixos:~]$ uname -a
Linux nixos 5.15.74 #1-NixOS SMP Sat Oct 15 05:59:05 UTC 2022 aarch64 GNU/Linux

[test@nixos:~]$ sudo poweroff
...
[   11.565895] reboot: Power down
```

[bootstrapping-a-local-builder-for-linux]: #bootstrapping-a-local-builder-for-linux

### Bootstrapping a local builder for Linux

As I mentioned before, to build any Linux configuration on macOS you need a
remote Linux builder. There are some existing options for that:
[nix-docker][nix-docker] that relies on Docker running and essentially
providing a Linux VM on its own, or [linuxkit-nix][linuxkit-nix] that creates a
custom Runit-based system with very little room for customisation.

With the [pull request providing `virtualisation.host.pkgs`][qemu-pkgs-pr]
option merged, [Gabriella Gonzales][gabriella439] created a pure NixOS-based
[builder][macos-builder], which has been [merged into
Nixpkgs][darwin-builder-pr]. Now you can [start using the local
builder][darwin-builder-manual] straight from the upstream Nix cache, then
modify its [configuration][macos-builder-profile] and rebuild it locally. See
[Gabriella's blog post][gabriella-blog-post] for more information.

[gabriella439]: https://github.com/Gabriella439
[macos-builder]: https://github.com/Gabriella439/macos-builder
[darwin-builder-pr]: https://github.com/NixOS/nixpkgs/pull/206951
[darwin-builder-manual]: https://nixos.org/manual/nixpkgs/stable/#sec-darwin-builder
[macos-builder-profile]: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/macos-builder.nix
[gabriella-blog-post]: https://www.haskellforall.com/2022/12/nixpkgs-support-for-linux-builders.html

### Running NixOS tests on macOS

There is an overarching issue on [running NixOS tests on
different virtualisation technologies on different platforms][vm-tests-issue].
[Robert Hensing][roberth] summarises the latest changes in this area pretty
well in his [comment][vm-tests-roberth-comment]. They include his
[work][modular-tests] on making NixOS tests modular and a [draft
implementation][darwin-tests] of providing an option to run them on macOS.

[roberth]: https://github.com/roberth
[vm-tests-issue]: https://github.com/NixOS/nixpkgs/issues/5241
[vm-tests-roberth-comment]: https://github.com/NixOS/nixpkgs/issues/5241#issuecomment-1260721556
[modular-tests]: https://github.com/NixOS/nixpkgs/pull/191540
[darwin-tests]: https://github.com/NixOS/nixpkgs/pull/193336

## What else can be done?

### Making nixos-rebuild aware of Darwin

On Linux `nixos-rebuild` allows you to build a NixOS system configuration, run
it in a VM or apply it to a remote NixOS system. On Darwin you can use it to
build a configuration, although it's hard to get man pages that are distributed
[with NixOS itself][nixos-manpages], rather than [in the `nixos-rebuild`
package][nixos-rebuild]. You can also build a VM with `nixos-rebuild build-vm`,
but it will only run on Linux. We could detect what platform `nixos-rebuild` is
running on and provide a VM compatible with said platform or allow the user to
specify it.

[nixos-manpages]: https://github.com/NixOS/nixpkgs/blob/21.11/nixos/doc/manual/default.nix#L235-L253
[nixos-rebuild]: https://github.com/NixOS/nixpkgs/tree/21.11/pkgs/os-specific/linux/nixos-rebuild

When managing remote NixOS systems from macOS, you have to be aware that all
derivations have to be built on a Linux platform, so you might want to put your
remote system in `--build-host` to avoid having to fetch all dependencies
locally, then copying dependencies and derivations to the builder, then copying
the NixOS configuration with all its dependencies from the builder to the local
machine, and finally copying them to your remote system. You also have to use
`--fast` because by default `nixos-rebuild` builds Nix and itself from the
NixOS configuration that you provide to it, and those are Linux binaries. With
all these parameters on macOS you have to specify a command line like this:

```shell
$ nixos-rebuild --build-host user@remote-host --target-host user@remote-host --use-remote-sudo --fast ...
```

We could detect that `nixos-rebuild` is not running on Linux and default to
building on the target host and either not use newly built binaries at all or
build them for the platform it is running on.

### Reducing number of system-dependent derivations

If you try running `nixos-rebuild dry-build` on a new NixOS configuration, you
will see a lot of paths that will be fetched from the cache, but also at least
200 derivations that will be built, all requiring Linux to do so. It would
seem logical that a Linux system requires Linux packages, but all packages are
already built by Hydra and available from the cache. All these derivations
produce different configuration files like systemd services, symlink trees
like `/etc` and some shell scripts like `nixos-rebuild` itself, that are built
by substituting a bunch of strings in a text file. Aside from being
[penalised for using `mkDerivation`][mkderivation-benchmark] for such simple
steps, all those actions don't really depend on the platform they are running
on. Nix requires us to guarantee that the result of the derivation will not
change by specifying a concrete platform and a concrete builder that will run
on that platform, which is Bash in all these cases. We could lean onto some
virtual machine to handle this guarantee and run the same builder binaries on
all supported platforms instead. The current idea is to [provide a
`wasm32-wasi` platform][system-agnostic-builders-issue] that will run builders
compiled to WASM, and allow building most of those derivations locally.

[mkderivation-benchmark]: https://discourse.nixos.org/t/benchmarking-stdenv-mkderivation-vs-derivation-for-trivial-builds/24298
[system-agnostic-builders-issue]: https://github.com/NixOS/nix/issues/6697

## Conclusion

We want to make macOS a first class citizen in the Nix ecosystem, and this post
shows some of the weaknesses that macOS users face when working with Nix and
NixOS. We as a community should strive to overcome these difficulties, and it's
great to see a lot of progress in this area. There's still a lot to be done
though and I hope to be able to help moving this forward.
