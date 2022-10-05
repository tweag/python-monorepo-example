---
title: "Python monorepo: an example, 2/2"
shortTitle: "Python monorepo: an example, 2/2"
author: Guillaume Desforges, Clément Hurlin
tags: [python]
description: "How to build your python monorepo from scratch: CI/CD and improvements"
---

In the [previous article](TODO link),
we described our python monorepo structure and tooling.
We continue by describing the CI/CD.

## CI/CD

The CI is composed of two parts:

- A pipeline that applies to the entire content of the monorepo and executes
  from its root directory. This pipeline only needs the development dependencies to be installed,
  but nothing project/library specific.
- A pipeline per project and library, that executes from the project or library's
  folder. This pipeline executes in a context where the dependencies
  of the concerned project or library are installed.

The top-level CI pipeline is:

```yaml
---
name: CI top-level
on: # yamllint disable-line rule:truthy
  workflow_dispatch:
  pull_request:
  push:
    branches: main # Comment this line if you want to test the CI before opening a PR

jobs:
  ci:
    runs-on: ubuntu-latest
    timeout-minutes: 10
    steps:
      - name: Checkout
        uses: actions/checkout@v2
      - name: Install Python 3.8
        uses: actions/setup-python@v3
        timeout-minutes: 5
        with:
          python-version: "3.8"
          cache: "pip"
          cache-dependency-path: |
            pip-requirements.txt
            dev-requirements.txt
      - name: Install Python dependencies
        run: |
          pip install -r pip-requirements.txt
          pip install -r dev-requirements.txt

      - name: Imports Format
        run: |
          isort --check-only $(git ls-files "*.py")

      - name: Format
        run: |
          black --check $(git ls-files "*.py")

      - name: Lint
        run: |
          flake8 $(git ls-files "*.py")

      # Are all public symbols documented? (see pyproject.toml configuration)
      - name: Doc
        run: |
          pylint $(git ls-files "*.py")
```

Because the monorepo contains many python projects and libraries that
are all typechecked and tested similarly, we want to share the definition
of the CI pipelines. For this we define a
[GitHub reusable workflow](https://docs.github.com/en/actions/using-workflows/reusing-workflows):

```yaml
---
name: Generic python workflow

on: # yamllint disable-line rule:truthy
  workflow_call:
    inputs:
      working-directory:
        required: true
        type: string
      install-packages:
        description: "Space seperated list of packages to install using apt-get."
        default: ""
        type: string
      # To avoid being billed 360 minutes if a step does not terminate
      # (we've seen the setup-python step below do so!)
      ci-timeout:
        description: "The timeout of the ci job. Default is 25min"
        default: 25
        type: number

jobs:
  py-template:
    runs-on: ubuntu-latest
    timeout-minutes: ${{ inputs.ci-timeout }}
    defaults:
      run:
        working-directory: ${{ inputs.working-directory }}
    steps:
      - name: Checkout
        uses: actions/checkout@v3
      - uses: actions/setup-python@v3
        timeout-minutes: 5 # Fail fast to minimize billing if this step freezes (it happened!)
        with:
          python-version: "3.8"
          cache: "pip"
          cache-dependency-path: |
            dev-requirements.txt
            pip-requirements.txt
            ${{ inputs.working-directory }}/requirements.txt
      - name: Install extra packages
        if: ${{ inputs.install-packages != ''}}
        run: |
          sudo apt-get install -y ${{ inputs.install-packages }}
      - name: Install dependencies
        run: |
          pip install -r ${{ github.workspace }}/pip-requirements.txt
          pip install -r ${{ github.workspace }}/dev-requirements.txt -r requirements.txt

      # No need to check formatting and linting, it's done by the top-level CI

      - name: Typechecking
        run: |
          pyright $(git ls-files "*.py")

      - name: Test
        run: |
          python3 -m pytest tests/  # Assume that tests are in folder "tests/"
```

## CI scaling

To prepare the monorepo for scaling, we wanted the CI to be modular from the start.
By modular, we mean that the CI should execute the pipelines of things that changed.
In the context of a pull request, the changes are made of the list of files and
directories that are affected by commits of the pull request.

GitHub actions have an easy mechanism for this:
the [paths](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#onpushpull_requestpull_request_targetpathspaths-ignore) keyword.
It applies to an entire pipeline and lists the monorepo's
paths that should trigger this pipeline's execution. In other words,
if files and directories affected by a PR are exclusive with `paths`'s content,
then the pipeline is not executed.

TODO project pipeline/library pipeline.

- each their CI workflow file that uses ⇒ workflow template or matrix

- use [`paths`](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#onpushpull_requestpull_request_targetpathspaths-ignore) to have CI run only when needed
  - for mylib, only mylib
  - for myproject, myproject + mylib

## Using poetry instead of pip {poetry}

- external deps ⇒ as usual
- internal deps ⇒ add path in “dev” dependency group
- lock files instead of `requirements.txt`

## Improvements

- script to check consistency across TOML files
- make a template to facilitate
- use poetry2nix for 100% reproducibility
- nox
