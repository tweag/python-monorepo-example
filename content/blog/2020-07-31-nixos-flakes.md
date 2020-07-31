---
title: "Nix Flakes, Part 3: Managing NixOS systems"
shortTitle: "Nix Flakes (3): Managing NixOS systems"
author: Eelco Dolstra
tags: [nix, devops]
description: "How to manage NixOS systems using Nix flakes."
---

This is the third in a series of blog posts about [_Nix
flakes_](https://github.com/NixOS/rfcs/pull/49). The [first
part](../2020-05-25-flakes) motivated why we developed flakes — to
improve Nix's reproducibility, composability and usability — and gave
a short tutorial on how to use flakes. The [second
part](../2020-06-25-eval-cache) showed how flakes enable reliable
caching of Nix evaluation results. In this post, we show how flakes
can be used to manage NixOS systems in a reproducible and composable
way.

## What problems are we trying to solve?

### Lack of reproducibility

One of the main selling points of NixOS is reproducibility: given a
specification of a system, if you run `nixos-rebuild` to deploy it,
you should always get the same _actual_ system (modulo mutable state
such as the contents of databases). For instance, we should be able to
reproduce in a production environment the exact same configuration
that we've previously validated in a test environment.

However, the default NixOS workflow doesn't provide reproducible
system configurations out of the box. Consider a typical sequence of
commands to upgrade a NixOS system:

- You edit `/etc/nixos/configuration.nix`.

- You run `nix-channel --update` to get the latest version of the
  `nixpkgs` repository (which contains the NixOS sources).

- You run `nixos-rebuild switch`, which evaluates and builds a
  function in the `nixpkgs` repository that takes
  `/etc/nixos/configuration.nix` as an input.

In this workflow, `/etc/nixos/configuration.nix` might not be under
configuration management (e.g. point to a Git repository), or if it
is, it might be a dirty working tree. Furthermore, `configuration.nix`
doesn't specify what Git revision of `nixpkgs` to use; so if somebody
else deploys the same `configuration.nix`, they might get a very
different result.

### Lack of traceability

The ability to reproduce a configuration is not very useful if you
can't tell what configuration you're actually running. That is, from a
running system, you should be able to get back to its
specification. So there is a lack of _traceability_: the ability to
trace derived artifacts back to their sources. This is an essential
property of good configuration management, since without it, we don't
know _what_ we're actually running in production, so reproducing or
fixing problems becomes much harder.

NixOS currently doesn't not have very good traceability. You can ask a
NixOS system what version of Nixpkgs it was built from:

```console
$ nixos-version --json | jq -r .nixpkgsRevision
a84b797b28eb104db758b5cb2b61ba8face6744b
```

Unfortunately, this doesn't allow you to recover `configuration.nix`
or any other external NixOS modules that were used by the
configuration.

### Lack of composability

It's easy to enable a package or system service in a NixOS
configuration if it is part of the `nixpkgs` repository: you just add
a line like `environment.systemPackages = [ pkgs.hello ];` or
`services.postgresql.enable = true;` to your `configuration.nix`. But
what if we want to use a package or service that _isn't_ part of
Nixpkgs? Then we're forced to use mechanisms like `$NIX_PATH`,
`builtins.fetchGit`, imports using relative paths, and so on. These
are not standardized (since everybody uses different conventions) and
are inconvenient to use (for example, when using `$NIX_PATH`, it's the
user's responsibility to put external repositories in the right
directories).

Put another way: NixOS is currently built around a _monorepo_ workflow
— the entire universe should be added to the `nixpkgs` repository,
because anything that isn't, is much harder to use.

It's worth noting that any NixOS system configuration already violates
the monorepo assumption: your system's `configuration.nix` is not part
of the `nixpkgs` repository.

## Using flakes for NixOS configurations

In the previous post, we saw that flakes are (typically) Git
repositories that have a file named `flake.nix`, providing a
standardized interface to Nix artifacts. We saw flakes that provide
packages and development environments; now we'll use them to provide
_NixOS system configurations_. This solves the problems described
above:

- _Reproducibility_: the entire system configuration (including
  everything it depends on) is captured by the flake and its lock
  file. So if two people check out the same Git revision of a flake
  and build it, they should get the same result.

- _Traceability_: `nixos-version` prints the Git revision of the
  top-level configuration flake, not its `nixpkgs` input.

- _Composability_: it's easy to pull in packages and modules from other
  repositories as flake inputs.

## Prerequisites

Flake support has been added as an experimental feature to NixOS
20.03. However, flake support is not part of the current stable
release of Nix (2.3). So to get a NixOS system that supports flakes,
you first need to switch to the `nixUnstable` package and enable some
experimental features. This can be done by adding the following to
`configuration.nix`:

```nix
nix.package = pkgs.nixUnstable;
nix.extraOptions = ''
  experimental-features = nix-command flakes
'';
```

## Creating a NixOS configuration flake

Let's create a flake that contains the configuration for a NixOS
container.

```console
$ git init my-flake
$ cd my-flake
$ nix flake init -t templates#simpleContainer
$ git commit -a -m 'Initial version'
```

Note that the `-t` flag to `nix flake init` specifies a _template_
from which to copy the initial contents of the flake. This is useful
for getting started. To see what templates are available, you can run:

```console
$ nix flake show templates
```

For reference, this is what the initial `flake.nix` looks like:

```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  outputs = { self, nixpkgs }: {

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules =
        [ ({ pkgs, ... }: {
            boot.isContainer = true;

            # Let 'nixos-version --json' know about the Git revision
            # of this flake.
            system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;

            # Network configuration.
            networking.useDHCP = false;
            networking.firewall.allowedTCPPorts = [ 80 ];

            # Enable a web server.
            services.httpd = {
              enable = true;
              adminAddr = "morty@example.org";
            };
          })
        ];
    };

  };
}
```

That is, the flake has one input, namely `nixpkgs` - specifically the
20.03 branch. It has one output, `nixosConfigurations.container`, which
evaluates a NixOS configuration for tools like
`nixos-rebuild` and `nixos-container`. The main argument is `modules`,
which is a list of NixOS configuration modules. This takes the place
of the file `configuration.nix` in non-flake deployments. (In fact,
you can write `modules = [ ./configuration.nix ]` if you're converting
a pre-flake NixOS configuration.)

Let's create and start the container! (Note that `nixos-container`
currently requires you to be `root`.)

```console
# nixos-container create flake-test --flake /path/to/my-flake
host IP is 10.233.4.1, container IP is 10.233.4.2

# nixos-container start flake-test
```

To check whether the container works, let's try to connect to it:

```console
$ curl http://flake-test/
<html><body><h1>It works!</h1></body></html>
```

As an aside, if you just want to _build_ the container without the
`nixos-container` command, you can do so as follows:

```console
$ nix build /path/to/my-flake#nixosConfigurations.container.config.system.build.toplevel
```

Note that `system.build.toplevel` is an internal NixOS option that
evaluates to the "system" derivation that commands like
`nixos-rebuild`, `nixos-install` and `nixos-container` build and
activate. The symlink `/run/current-system` points to the output of
this derivation.

## Hermetic evaluation

One big difference between "regular" NixOS systems and flake-based
NixOS systems is that the latter record the Git revisions from which
they were built. We can query this as follows:

```console
# nixos-container run flake-test -- nixos-version --json
{"configurationRevision":"9190c396f4dcfc734e554768c53a81d1c231c6a7"
,"nixosVersion":"20.03.20200622.13c15f2"
,"nixpkgsRevision":"13c15f26d44cf7f54197891a6f0c78ce8149b037"}
```

Here, `configurationRevision` is the Git revision of the repository
`/path/to/my-flake`. Because evaluation is hermetic, and the lock file
locks all flake inputs such as `nixpkgs`, knowing the revision
`9190c39…` allows you to completely reconstruct this configuration at
a later point in time. For example, if you want to deploy this
particular configuration to a container, you can do:

```console
# nixos-container update flake-test \
    --flake /path/to/my-flake?rev=9190c396f4dcfc734e554768c53a81d1c231c6a7
```

## Dirty configurations

It's not required that you commit all changes to a configuration
before deploying it. For example, if you change the `adminAddr` line
in `flake.nix` to

```nix
adminAddr = "rick@example.org";
```

and redeploy the container, you will get:

```console
# nixos-container update flake-test
warning: Git tree '/path/to/my-flake' is dirty
...
reloading container...
```

and the container will no longer have a configuration Git revision:

```console
# nixos-container run flake-test -- nixos-version --json | jq .configurationRevision
null
```

While this may be convenient for testing, in production we really want
to ensure that systems are deployed from clean Git trees. One way is
to disallow dirty trees on the command line:

```console
# nixos-container update flake-test --no-allow-dirty
error: --- Error -------------------- nix
Git tree '/path/to/my-flake' is dirty
```

Another is to require a clean Git tree in `flake.nix`, for instance by
adding a check to the definition of `system.configurationRevision`:

```nix
system.configurationRevision =
  if self ? rev
  then self.rev
  else throw "Refusing to build from a dirty Git tree!";
```

## Adding modules from third-party flakes

One of the main goals of flake-based NixOS is to make it easier to use
packages and modules that are not included in the `nixpkgs`
repository. As an example, we'll add
[Hydra](https://github.com/NixOS/hydra/blob/master/flake.nix) (a
continuous integration server) to our container.

Here's how we add it to our container. We specify it as an additional input:

```nix
  inputs.hydra.url = "github:NixOS/hydra";
```

and as a corresponding function argument to the `outputs` function:

```nix
  outputs = { self, nixpkgs, hydra }: {
```

Finally, we enable the NixOS module provided by the `hydra` flake:

```nix
      modules =
        [ hydra.nixosModules.hydraTest

          ({ pkgs, ... }: {
            ... our own configuration ...

            # Hydra runs on port 3000 by default, so open it in the firewall.
            networking.firewall.allowedTCPPorts = [ 3000 ];
          })
        ];
```

Note that we can discover the name of this module by using `nix flake show`:

```console
$ nix flake show github:NixOS/hydra
github:NixOS/hydra/d0deebc4fc95dbeb0249f7b774b03d366596fbed
├───…
├───nixosModules
│   ├───hydra: NixOS module
│   ├───hydraProxy: NixOS module
│   └───hydraTest: NixOS module
└───overlay: Nixpkgs overlay
```

After committing this change and running `nixos-container update`, we
can check whether `hydra` is working in the container by visiting
http://flake-test:3000/ in a web browser.

## Working with lock files

There are a few command line flags accepted by `nix`, `nixos-rebuild`
and `nixos-container` that make updating lock file more
convenient. A very common action is to update a flake input to the
latest version; for example,

```console
$ nixos-container update flake-test --update-input nixpkgs --commit-lock-file
```

updates the `nixpkgs` input to the latest revision on the
`nixos-20.03` branch, and commits the new lock file with a commit
message that records the input change.

A useful flag during development is `--override-input`, which allows
you to point a flake input to another location, completely overriding
the input location specified by `flake.nix`. For example, this is how
you can build the container against a local Git checkout of Hydra:

```console
$ nixos-container update flake-test --override-input hydra /path/to/my/hydra
```

## Adding overlays from third-party flakes

Similarly, we can add Nixpkgs overlays from other flakes. (Nixpkgs
overlays add or override packages in the `pkgs` set.) For example,
here is how you add the overlay provided by [the `nix`
flake](https://github.com/NixOS/nix/blob/flakes/flake.nix):

```nix
  outputs = { self, nixpkgs, nix }: {
    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      ...
      modules =
        [
          ({ pkgs, ... }: {
            nixpkgs.overlays = [ nix.overlay ];
            ...
          })
        ];
    };
  };
}
```

## Using `nixos-rebuild`

Above we saw how to manage NixOS containers using flakes. Managing
"real" NixOS systems works much the same, except using `nixos-rebuild`
instead of `nixos-container`. For example,

```console
# nixos-rebuild switch --flake /path/to/my-flake#my-machine
```

builds and activates the configuration specified by the flake output
`nixosConfigurations.my-machine`. If you omit the name of the
configuration (`#my-machine`), `nixos-rebuild` defaults to using the
current host name.

## Pinning Nixpkgs

It's often convenient to _pin_ the `nixpkgs` flake to the exact
version of `nixpkgs` used to build the system. This ensures that
commands like `nix shell nixpkgs#<package>` work more efficiently
since many or all of the dependencies of `<package>` will already be
present. Here is a bit of NixOS configuration that pins `nixpkgs` in
the system-wide flake registry:

```nix
nix.registry.nixpkgs.flake = nixpkgs;
```

Note that this only affects commands that reference `nixpkgs` without
further qualifiers; more specific flake references like
`nixpkgs/nixos-20.03` or
`nixpkgs/348503b6345947082ff8be933dda7ebeddbb2762` are unaffected.

## Conclusion

In this blog post we saw how Nix flakes make NixOS configurations
hermetic and reproducible. In a future post, we'll show how we can do
the same for cloud deployments using NixOps.

Acknowledgment: The development of flakes was partially funded by
Target Corporation.
