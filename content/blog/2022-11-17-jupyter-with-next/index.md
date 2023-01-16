---
title: JupyterWith Next
author: Daniel Baker, Rok Garbas
tags: [nix, jupyter]
description: "Announcing the next version of jupyterWith."
---

[JupyterWith][jupyter-with-docs] has been around for several years with growing popularity.
Over the years, we found that researchers struggled with the Nix language and jupyterWith API.
Since researchers are our primary target audience, we decided to improve the usability of jupyterWith.

Today, we are proud to announce the release of a new version!
The new simplified API makes jupyterWith easier to use and provides more options for creating kernels.

## What is jupyterWith?

JupyterLab is a web-based interactive development environment for notebooks, code, and data.
These notebooks can be shared with other users and the residing code can be rerun providing repeatability.

The Jupyter ecosystem allows users to produce and repeat research and results, but it lacks in facilitating reproducible results.
There may not appear to be a difference between repeatable and reproducible, but there is a meaningful difference; reproducibility guarantees that our code and results will be exactly the same while repeatability does not.

While many Jupyter kernels are available as Python packages, just as many are not (e.g. haskell and julia).
Projects such as [PDM][pdm-homepage] and [JupyterLab Requirements][jupyterlab-requirements] can create reproducible environments but are restricted to the Python kernels.

jupyterWith was [announced][original-blog-post] in early 2019 and provides a Nix-based framework for declarative and reproducible JupyterLab environments with configurable kernels.
It actively supports over a dozen kernels and provides example setups and notebooks for users to try out.
jupyterWith can create entirely reproducible JupyterLab environments for any kernel.

## Why jupyterWith?

If you can run an experiment multiple times in the same environment and get to the same conclusion, you have repeatability.
In our case, running the same code on the same machine should give the same outputs.
Consider what would happen if you handed off your code to another user and they ran it on their system.
Different operating systems or different versions of the same operating system may fetch different versions of the same package.
Fetching the same package at different times may not return the same version due to patch or security updates.
If you can guarantee the same outputs given all that has changed, then you have reproducibility.

With repeatability, we cannot guarantee that the packages and dependencies of our code will remain constant.
Using jupyterWith we can give that guarantee and ensure that on any system, run by any user, and given identical inputs, the code will produce identical outputs.
This guarantee is what makes our code and therefore our research reproducible.

## What is new?

This release focuses on helping users quickly and easily get their project started, and making it easier to extend kernels to fit their needs.

### New templates

The new version of jupyterWith provides new kernel templates which makes it easier for users to bootstrap their project using Nix flakes.
They are small, easily digestible, and ready to be customized.

### Better Python kernels

It used to be difficult to select particular Python packages because we were tied to `nixpkgs`.
jupyterWith now uses [Poetry][poetry-homepage] and [poetry2nix][poetry2nix-repo] to install kernels that are packaged with Python and their dependencies.
Poetry allows users to easily select the desired version of a package and can resolve dependencies.
poetry2nix greatly simplifies the kernel files, which helps with readability and maintainability.

### Better kernel definition interface

Finally, we have simplified and standardized the interfaces for kernel files.
This makes it easier for users to implement completely new kernels.

## Getting Started

The following code will initialize a new project directory with a flake template from the jupyterWith repository and start the JupyterLab environment.
With a renewed focus on user ease, this is all that is necessary to get started.

```shell
$ mkdir my-project
$ cd my-project
$ nix flake init --template github:tweag/jupyterWith
$ nix run
```

Each kernel provided will generally only have the standard libraries and packages available, but there is a readme provided with the template with instructions on extending existing kernels, creating a custom kernel, and installing extensions.

## Migration

If you have used jupyterWith in the past, you are probably used to seeing kernel files like the ipython kernel example below.
The version of Python used and the packages available to the kernel, can be set using the `python3` and `packages` attributes respectively.

_Old interface_

```nix
{
  iPython = iPythonWith {
    # Identifier that will appear on the Jupyter interface.
    name = "nixpkgs";
    # Libraries to be available to the kernel.
    packages = p: with p; [ numpy pandas ];
    # Optional definition of `python3` to be used.
    # Useful for overlaying packages.
    python3 = pkgs.python3Packages;
    # Optional value to true that ignore file collisions inside the packages environment
    ignoreCollisions = false;
  };
}
```

The new interface is similar but there are a few key differences.
All kernels are provided through `availableKernels` and the kernels are named by the language rather than the kernel project name.
For example, before there was `iPythonWith` and `iHaskellWith`, and now it is `availableKernels.python` and `availableKernels.haskell`.
The version of Python uses is passed through the `python` attribute and additional packages are provided with the `extraPackages` attribute.
There is one new attribute, `editablePackageSources`, which is used by poetry2nix, to add packages to the environment in editable mode.

_New interface!_

```nix
{
  pkgs,
  availableKernels,
  kernelName,
}:
availableKernels.python.override {
  name = "python-with-numpy"; # must be unique
  displayName = "python with numpy"; # name that appears in JupyterLab Web UI
  python = pkgs.python3;
  extraPackages = ps: [ ps.numpy ];
  editablePackageSources = {};
}
```

Both of these are still subject to the package versions available in `nixpkgs`.
However, with Poetry, we can create a completely custom kernel with a `pyproject.toml` file and specify exactly which package versions we want.
The full details are available in the **How To** and **Tutorials** sections of the [documentation][jupyter-with-docs].

## Conclusion

Usability has been improved, but there is much more to do.
The next major items on the roadmap include:

- Updating and improving the flake templates.
- Updating and improving documentation on configuring existing kernels and packaging new kernels.
- Providing better MacOS support.
- Adding new and improving existing kernels.
- Create a website indexing kernels that can be used and configured in jupyterWith.

Join us in contributing to the project.
You can find the repository [here][jupyter-with-repo].

[jupyter-with-docs]: https://jupyterwith.tweag.io
[jupyter-with-repo]: https://github.com/tweag/jupyterWith
[jupyterlab-requirements]: https://github.com/thoth-station/jupyterlab-requirements
[jw-howto]: https://github.com/tweag/jupyterWith/blob/c0941fe9a1a93e29e5ddc9228f9eacb263aa28cd/docs/HOWTO.md
[jw-tutorials]: https://github.com/tweag/jupyterWith/blob/c0941fe9a1a93e29e5ddc9228f9eacb263aa28cd/docs/TUTORIALS.md
[original-blog-post]: https://www.tweag.io/blog/2019-02-28-jupyter-with/
[pdm-homepage]: https://pdm.fming.dev/latest/
[poetry-homepage]: https://python-poetry.org/
[poetry2nix-repo]: https://github.com/nix-community/poetry2nix
