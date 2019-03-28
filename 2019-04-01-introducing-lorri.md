---
title: "Introducing lorri, <br/>your project's nix-env"
author: Graham Christensen, Philip Patsch
---

Today we're announcing lorri, a new tool for NixOS, Linux, and macOS
that makes developing with Nix even nicer:

<asciinema-player src="../anims/2019-04-01-lorri-demo.cast" cols="70" rows="30"></asciinema-player>

When people try lorri, we often hear that it is better than they
expected.

## What is lorri?

lorri is a nix-shell replacement for project development. lorri is
based around fast [direnv][direnv] integration for robust CLI and
editor integration.

The project is about experimenting with and improving the developer's
experience with Nix. A particular focus is managing your project's
external dependencies, editor integration, and quick feedback.

## How is lorri different from a nix-shell?

Nix's shells are a nice tool for environment management, but have
some pain points.

Let's look at three ways lorri changes the experience.

### Channel updates aren't disruptive

Do you use `import <nixpkgs> {}`? Update your channels and your Nix
shells turn stale! Opening a new `nix-shell` means downloading
all new dependencies before you're able to get back to work.

Lorri is different. When your channel updates, `lorri watch`
automatically begins re-evaluating your project's dependencies in
the background, outside of your work shell. If you enter the shell
before the evaluation completes, the last completed evaluation is
loaded instead. When the new one is ready, your environment updates
automatically.

### `nix-collect-garbage` without fear

Nix shells are not protected from garbage collection. This is good for
one-off shells (`nix-shell -p fortune --run fortune`) but not as nice
for your large project which integrates dozens of Nixpkgs dependencies.
Having your project's tooling disappear on a low-bandwidth connection
is a pain most Nix users know.

lorri captures development dependencies and creates garbage collection
roots automatically. Switching to lorri means you are always ready
to get to work on your projects.

### Editor integration is fast

A big hassle for Nix users is editor integration and tools like
language servers.

Adding [direnv][direnv] to the tool stack makes this better: direnv is
like Nix's secret weapon for editor integration. Many editors support
direnv, direnv supports Nix, and thus: many editors support Nix!

Out of the box, however, direnv's Nix integration is slow --
continuously, unnecessarily evaluating Nix expressions. In some
editors the expressions are re-evaluated on every file switch. This
is painful, especially if the evaluation takes even half a second!

lorri's direnv integration doesn't call Nix. Instead, the
integration always selects the last cached evaluation, ensuring a
lightning-fast editor and shell experience.

When entering a project directory your tools just appear, ready to
use.

# Using lorri

lorri is used in two parts:

 - direnv integration sets up your shell and editor integration with
   project-specific dependencies
 - `lorri watch` monitors your project and automatically regenerates
   your environment

A typical workflow for us is to spawn a terminal running
`lorri watch`, minimize it, and open a second terminal for my work
shell. If I'm not editing dependencies, sometimes I'll sometimes even
skip the `lorri watch` and just use the cached evaluation.

lorri was built by Tweag for [Target][target], and we are so excited to introduce
it to the public. lorri is beta, open source (Apache-2.0) and
[a tutorial is available at Target/lorri][tutorial]. Give it a try!

<script src="/web-stuff/asciinema.js"></script>

[direnv]: https://direnv.net/
[tutorial]: https://github.com/target/lorri#tutorial
[target]: https://target.com

<script src="../asciinema/asciinema-player.js"></script>
