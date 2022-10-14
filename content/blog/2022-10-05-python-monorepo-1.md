---
title: "Python monorepo: an example, 1/2"
shortTitle: "Python monorepo: an example, 1/2"
author: Guillaume Desforges, Clément Hurlin
tags: [python]
description: "How to build your Python monorepo from scratch: structure and tooling"
---

Delivering interdependent software across teams of an organization is challenging.
When changes in a package require changes in one of its dependencies, it is usually required to first make the changes in the dependency and publish a new release of it, only then can the original package be update accordingly.
In the context of a bigger organization with many interdependent packages, those processes can quickly add up, leading to numerous cascading Pull Requests (PRs) across many repos, which is hard to manage and adds friction to the development workflow.

An alternative is to "live at HEAD": instead of waiting for releases and cascading PRs, a developer works on all related packages at once in one PR and publishes them all in one go.
Such a development workflow requires to go from numerous code repositories to a single repository, called "monorepo".
However, designing a monorepo can be challenging as it impacts the development workflow of all engineers.

In this post, we describe a design for a Python monorepo: how we structure it, which tool we favor, alternatives that were considered, and some possible improvements.

## Python environments: one global vs many local

Working on a Python project requires a Python environment (a.k.a a sandbox), with a Python interpreter and the right Python dependencies (packages).
When working on multiple projects, one can either use a single shared sandbox for all projects, or many specific ones, for each project.

On one hand, a single sandbox for all projects makes it trivial to ensure that all developers and projects use a common set of dependencies.
This is desirable as it reduces the scope of things to manage when implementing and debugging.
Also, it ensures that all employees are working towards a shared, common knowledge about its software.

On the other hand, it makes it impossible for different projects to use different versions of external dependencies.
It is also mandatory to install the dependencies for all projects, even when they only need a subset of them to work on a single project.

In order not to lose flexibility when needed, we decided to use _multiple sandboxes_, one per project.
We can later improve the consistency of external dependencies across Python environments with some tooling.

A sandbox can be created anytime with Nix in any folder, for a specific Python version (here 3.9):

```
$ nix shell nixpkgs#python39 --command python -m venv .venv --copies
$ source .venv/bin/activate
(.venv) $ which python
/some/path/.venv/bin/python
```

We will later describe how this is put to use.

## Choosing a Python package manager

In our scenario, we chose to stick to [pip](https://pypi.org/project/pip/) for populating sandboxes, because [poetry](https://python-poetry.org/) still doesn't work [100% smoothly](https://github.com/python-poetry/poetry/issues/6409) with [pytorch](https://pytorch.org/), a core library in the data ecosystem.
We refer the reader to the [poetry](#poetry) section below for a solution using `poetry`.

For the past years, pip has undergone many important breaking changes, such as [PEP 660](https://peps.python.org/pep-0660/).
To improve reproducibility, we pin the version of `pip` in a top-level `pip-requirements.txt` file, with the exact version of pip to use.

`pip-requirements.txt`
```
# Install a specific version of pip before installing any other package.
pip==22.2.2
```

It will be important to update pip to this version before installing anything else.

## Creating projects and libraries

In an organization, each team will be owner of its own projects.
For instance, there could be a Web API, a collection of data processing job, and Machine Learning training pipelines.
While each team is working on its own projects, it is most likely that a portion of their code is shared.
Following the DRY (Don't Repeat Yourself) principle, it is best to refactor those shared portions into libraries.

We have found that, in Python, there is no significant difference between projects and libraries: they are Python packages.
Because of that, we make no difference between the two.
However, for the sake of clarity, we split the monorepo structure into two top-level folders, one for projects and one for libraries:

```
├── libs
└── projects
```

This top-level organization highlights that libraries are to be shared across the entire organization.

To create a project or a library, a folder needs to be created in one or the other.
It should be then populated with the following:

- A `pyproject.toml` file which defines the Python package.
  It contains its meta data (name, version, description) and the list of dependencies for dependency resolution.
- A `requirements.txt` file which serves as the basis for creating local sandboxes for developers and also as the default testing environment in continuous integration (CI), in the same way as a lock file.
  It has to list all dependencies, direct and transitive, frozen at a specific version, in pip's "requirement file" format.
- A `README.md` file.
  This file's purpose is to list the owners of this package:
  the persons to contact if the package needs to evolve or is broken.
  This file also contains a short description of what the package is about and example commands to run the code or test it.
  It's supposed to be a gentle introduction to newcomers to this package.

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
This is an obvious choice, because all concerned developers had experience with it and there was no other contender.

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
nix shell nixpkgs#python39 --command python -m venv .venv --copies # Calls Python "venv" module, to create the ".venv" folder
source .venv/bin/activate # Make the sandbox active in the current shell session
# Install pinned pip first
pip install -r $(git rev-parse --show-toplevel)/pip-requirements.txt
# Install shared development dependencies and project/library-specific dependencies
pip install -r $(git rev-parse --show-toplevel)/dev-requirements.txt -r requirements.txt
```

This can shortened by using `nox`, as explained in the [improvements](#improvements) section below.

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
