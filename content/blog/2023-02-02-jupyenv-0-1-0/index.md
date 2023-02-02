---
title: announcing jupyenv 0.1.0
author: Daniel Baker
tags: [jupyenv, jupyter, nix]
description: "jupyterWith gets a new name and updates."
---

In November 2022, we released an API update to jupyterWith.
That was the first step in improving the user experience and paved the way for many future enhancements.
Today we are announcing another update to the API, a new project name, jupyenv, and new features on the site.
You can now find the new site at [jupyenv.io][jupyenv-site].

Why the name change?
We want to reach a bigger audience.
If you are a regular Nix user, jupyterWith is a good name.
It is analogous to the `python.withPackages` function in nixpkgs.
But outside of the Nix ecosystem, the `with` naming scheme is non-existent.
To better target people outside the Nix ecosystem, we are changing the name that emphasizes Jupyter environments (hence jupyenv).

## New API

### Module System

jupyenv now uses [NixOS modules][nixos-modules] to configure the JupyterLab environment and kernels.
The previous API update reduced complexity, but using modules reduces it even further without sacrificing capability.
A minimal working Python kernel used to be configured like so.

```nix
# Old interface
{
  availableKernels,
  name,
  extraArgs,
}:
availableKernels.python {
  name = "${name}-example";
  inherit (extraArgs) system;
}
```

While this is not terribly complicated, it exposes the user to the internal machinery of jupyenv.
What is `availableKernels`?
What is `extraArgs` and what else can I pass in?
Is there something special about the `name` attribute?
(_Spoiler: Yes! They have to be unique and without spaces._)

Now with modules, we can hide all the inner workings.
The following is equivalent to the previous example, but uses the new modules API.

```nix
# New interface
{...}: {
  kernel.python.example.enable = true;
}
```

Using modules not only simplifies configuring kernels, but it also simplifies how they can be organized.
You are no longer _required_ to have separate kernel directories.
You can if you prefer, but using modules provides the flexibility to configure all the kernels in one location.

The following is a working example configuration with two different Python kernels, a Bash kernel, and an OCaml kernel.

```nix
{...}: {
  kernel.python.minimal.enable = true;

  kernel.python.science.enable = true;
  kernel.python.science.extraPackages = ps: [
    ps.numpy
    ps.scipy
  ];

  kernel.bash.minimal.enable = true;

  kernel.ocaml.science.enable = true;
  kernel.ocaml.science.ocamlPackages = {
    hex = "*";
    owl = "*";
  };
}
```

In the previous examples, we used names like `example`, `minimal`, and `science` to configure a specific kernel.
Those names are purely descriptive and used so you can configure multiple kernels of the same type (e.g. `python.minimal` and `python.science`), and have them both available in your JupyterLab environment.
For more information, see the [Kernels][jupyenv-kernels] section on the How To page.

### Documentation

One of the perks of using NixOS modules is their ability to create documentation.
Though the previous update was an improvement, the user would have to read the source code to find what arguments were available and how to use them.
Ideally, documentation would be available on the site, but it cannot be done manually, since it would quickly go out of date.
Now, with modules, the documentation is part of the option definitions and automatically uploaded to the jupyenv site as it changes.

We have taken great care to ensure the new modules documentation is tidy and easy to navigate.
With a bit of pre-processing and styling, child options are nested under their parents making the structure clearly visible.
Additionally, with a small amount of JavaScript, each option can be expanded.
You can see a demo of the new [Options documentation][jupyenv-options] in the video below.

`video: title: "Options Demo": ./jupyenv-options-demo.mp4`

If you are not a fan of JavaScript or use a Text-Mode Web Browser, the page will still render nicely.
It is also navigable without the use of a mouse and should have the relevant context for screen readers.

## Blog

With the last API update, we found it was difficult to inform our users that things were changing.
Sure, there is the Tweag blog, and it does well for major announcements, but there are times when we want to inform our users without having to write a major blog post.
To keep users in the loop, we have added a Blog tab to the site.
Here we will post about new releases, interface changes, and bug fixes.

## Releases

The aforementioned new releases are going to be available as Git annotated tags in our GitHub repository
It is not so useful to mention a new release without some reference to a point in time in the repository.
So to help users not only see what changed, but when also when it changed, we will begin versioning jupyenv and announcing it on the blog.

## Community

We want to create a community of jupyenv users, but so far there was no space for them to communicate directly.
Where can they ask questions?
Where can they collaborate with other users?
To answer this, we have created a Matrix Space which you can find in the Community tab on the site or [here][matrix-space].

## Conclusion

There are a lot of new updates.
A new name, a new module based API, updated and improved documentation, a blog, tagged releases, and a matrix space.
If you are a current jupyenv user, we hope the changes improve your user experience.
If you have never tried it but are interested in reproducible Jupyter environments or reproducible data science, please give it a try.
Join us on the journey to make Jupyter reproducible.

[jupyenv-kernels]: https://jupyenv.io/documentation/how-to/#kernels
[jupyenv-options]: https://jupyenv.io/documentation/options/
[jupyenv-site]: https://jupyenv.io
[matrix-space]: https://matrix.to/#/#jupyenv:matrix.org
[nixos-modules]: https://nixos.org/manual/nixos/stable/index.html#sec-writing-modules
