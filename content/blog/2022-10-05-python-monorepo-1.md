---
title: "Python monorepo: an example, 1/2"
shortTitle: "Python monorepo: an example, 1/2"
author: Guillaume Desforges, Clément Hurlin
tags: [python]
description: "How to build your python monorepo from scratch: structure and tooling"
---

In an organization, delivering interdependent software across teams can be quite challenging.
Changes in a package can require changes in one of its dependencies.
To do that, it is usually required to first make the changes in the dependency and publish a new release of it.
Only then can the original package be update accordingly.
In the context of a bigger organization with many interdependent packages, those processes can quickly add up, leading to numerous cascading Pull Requests (PRs) across many repos.

An alternative is to "live at HEAD": instead of waiting for releases and cascading PRs, a developer can work on all related packages at once in one PR and publish them all in one go.
Such a development workflow requires the use of a monorepo.

Starting a monorepo from scratch can be challenging: there are lot of choices to make and these choices will impact the development process of many engineers.
In this post, we describe the design of a Python monorepo: how we structure it, which tool we favore, alternatives that were considered, and some possible improvements.

## Projects and libraries

In python, there is no significant difference between projects and libraries.
Projects are simply pieces of code that are not meant to be reused by other projects.
Because of that, we make no difference between the two, except that the monorepo structure has two top-level folders for projects and libraries:

```
├── libs
└── projects
```

To create a project or a library, you should populate its folder with the following:

- A `pyproject.toml` file.
  It contains loose non-transitive dependencies of your package.
- A `requirements.txt` file.
  It contains pinned transitive dependencies of your package.
  This file serves as the basis for creating local sandboxes for developers and also as the default testing environment in continuous integration (CI).
- A `README.md` file.
  This file's purpose is to list the owners of this package:
  the persons to contact if the package needs to evolve or is broken.
  This file also contains a short description of what the package is about and example commands to run the code or test it.
  It's supposed to be a gentle introduction to newcomers to this package.

In our scenario, we chose to stick to [pip](https://pypi.org/project/pip/) for populating sandboxes, because [poetry](https://python-poetry.org/) still doesn't work [100% smoothly](https://github.com/python-poetry/poetry/issues/6409) with a popular data-science: [pytorch](https://pytorch.org/).
We refer the reader to the [poetry](#poetry) section below for a solution using `poetry`.

For reproducibility reasons, we pin the version of `pip` thanks to a top-level `pip-requirements.txt` file that contains a single line: the version of pip to use.

```
# Install a specific version of pip before installing any other package.
pip==22.2.2
```

## Formatting and linting

For formatting source code, we choose [black](https://github.com/psf/black), because it is easy to use and is easily accepted by most developers.
Our philosophy for choosing a formatter is simple: pick one and don't discuss it for too long.

For linting source code, we choose [flake8](https://flake8.pycqa.org/en/latest/), [isort](https://github.com/PyCQA/isort), and [pylint](https://pypi.org/project/pylint/).
We use `flake8` and `isort` without any tuning.
Again, the rationale was that the default checks are good and easily accepted.
Regarding `pylint`, we use it only for checking that public symbols are documented.
Because `pylint` is more intrusive, activating more checks would have required more lengthy discussions, which we decided not worthwhile in the early days of our monorepo.

To make all the tools work well together, we need a little configuration:

```toml
> cat pyproject.toml
[tool.black]
line-length = 100
target-version = ['py38']

[tool.pylint."messages control"]
disable = "all"
enable = ["empty-docstring", "missing-module-docstring", "missing-class-docstring", "missing-function-docstring"]
ignore = ["setup.py", "__init__.py"]

[tool.isort]
profile = "black"
```

```toml
> cat tox.ini
[flake8]
max-line-length = 100
# required for compatibility with black:
extend-ignore = E203
exclude = .venv
```

We need to use both `pyproject.toml` and `tox.ini`, because at the time of writing, `flake8` doesn't support having its configuration be it in `pyproject.toml`, hence the presence of `tox.ini` above.

In addition, we need `pyproject.toml` files in each project and library, because `pyproject.toml` are used to list direct dependencies of a package.
We will make sure in the [CI](#ci) that nested `pyproject.toml` files and the top-level `pyproject.toml` file agree on their common subset.

To pin the versions of all these tools for the entire monorepo, we have a top-level `dev-requirements.txt` file that contains the following:

```
black==22.3.0
flake8==4.0.1
isort==5.10.1
```

To recap what we described so far, at this point our monorepo's structure is:

```shell
├── dev-requirements.txt
├── libs
├── pip-requirements.txt
└── projects
```

## Typechecking

To typecheck code, we choose Microsoft's [pyright](https://github.com/microsoft/pyright).
In our benchmarks, it proved noticeably faster than [mypy](http://mypy-lang.org/) and seems more widely used than Facebook's [pyre](https://pyre-check.org/).
Compared to `mypy`, `pyright` also has the advantage that it can execute as you type: it gives feedback without requiring to save the file being edited.
Because `mypy` has a noticeable startup time, this made for a significant difference in user experience, consolidating our choice in favor of `pyright`.

`pyright` has different levels of checking.
We stick to the default settings called `base`.
These settings make the tool very easily accepted: if your code is not annotated, `pyright` will mostly remain silent.
If your code is annotated, in our experience, `pyright` reports only errors that are relevant.
In the rare cases where it reported false positives (i.e. reporting an error while there isn't one),
the reason made sense.
This happens for example if type-correctness of an assignment depends on the right-hand side of the assignment being not `None` because some previous loop always executes at least once.

`pyright` also works really well no matter the amount of annotations that external dependencies (i.e. libraries outside the monorepo) have.
If external dependencies are not annotated, `pyright` infers types by crawling their source code, and we witness that it return really good types, even for data-science libraries with a lot of union types (such as `pandas`).

To turn typechecking with `pyright`, we specified a pinned version in the shared top-level `dev-requirements.txt` as follows:

```
pyright==1.1.239
```

and we specify it the `pyproject.toml` file of every project and library:

```toml
> cat pyproject.toml
...
[tools.pyright]
...
```

Like for `pip` and other tools, pinning `pyright`'s version helps making local development and the CI deterministic.

## Testing

To test our code, we choose [pytest](https://docs.pytest.org/en/7.1.x/).
This is an obvious choice, because all concerned developers had experience with it and there was no contender.

Among `pytest`'s qualities, we can cite its good progress reporting while the tests are running and easy integration with test coverage.

To make `pytest` available, we again specified a pinned version in the shared top-level `dev-requirements.txt` as follows:

```
pytest==7.0.1
pytest-cov==3.0.0  # Coverage extension
```

## Sandboxes

With all of the above in place, we are now able to create sandboxes to obtain comfortable development environments.
For example suppose we have one library named `fancylib`, making the monorepo structure as follows:

```shell
├── dev-requirements.txt
├── libs
│   └── fancylib
│       ├── pyproject.toml
│       ├── README.md
│       └── requirements.txt
├── pip-requirements.txt
├── projects
├── pyproject.toml
└── tox.ini
```

To create `fancylib`'s development environment, go to directory `libs/fancylib` and execute:

```shell
python3 -m venv .venv  # Calls python "venv" module, to create the ".venv" folder
source .venv/bin/activate  # Make the sandbox active in the current shell session
# Install pinned pip first
pip install -r $(git rev-parse --show-toplevel)/pip-requirements.txt
# Install shared development dependencies and project/library-specific dependencies
pip install -r $(git rev-parse --show-toplevel)/dev-requirements.txt -r requirements.txt
```

This can be shortened by defining a [git alias](https://git-scm.com/book/en/v2/Git-Basics-Git-Aliases) to return the root a repository, by adding the following to `$HOME/.gitconfig`:

```shell
[alias]
	root = rev-parse --show-toplevel
```

This makes it possible to use `git root` in the snippet above instead of `git rev-parse --show-toplevel`, making it slightly more amenable.
This can shortened even more by using `nox`, as explained in the [improvements](#improvements) section below.

## Configuration of one package

We use a common namespace in all projects and libraries.
This avoids one level of nesting by avoiding the `src` folder (which is the historical way of doing, called the [src layout](https://packaging.python.org/en/latest/tutorials/packaging-projects/#a-simple-project)).

Supposing that we choose `mycorp` as the namespace, this means that the code of library `libs/fancylib` lives in directory `libs/fancylib/mycorp` and the `pyproject.toml` of the library must contain:

```toml
packages = [
  { include = "mycorp" }
]
```

We now come to a very important topic: the difference between `pyproject.toml` and `requirements.txt` for declaring dependencies.
TODO.

  <!-- [SO explanation](https://stackoverflow.com/questions/14399534/reference-requirements-txt-for-the-install-requires-kwarg-in-setuptools-setup-py/33685899#33685899) -->
