# python-monorepo-example

This repository is a template for a Python monorepo with projects-specific
virtual environments. It uses [Pip](https://pypi.org/project/pip/) for installing dependencies.

Because this repository is a [GitHub template](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository),
you can duplicate it by using
[_Creating a repository from a template_](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-repository-from-a-template) in the GitHub UI. It will create
a copy of this repository that is not a fork.

This repository's design is explained in the Tweag blogpost
[Python monorepo; part 1](https://www.tweag.io/blog/2023-04-04-python-monorepo-1/).
The design strives to be simple, to work well in a startup environment where
CI specialists are not yet available, and yet to achieve a great deal
of [reproducibility](https://reproducible-builds.org/) to prepare for scaling.

We use a virtual environment per library/project, to allow dependencies to
diverge if you need to. Another alternative is to have a single virtual environment
for the entire repository, to maximize uniformity. Choose what suits you best.

We use [editable installs](https://setuptools.pypa.io/en/latest/userguide/development_mode.html)
for dependencies _within_ this repository, so that changes to a library are reflected
immediately in code depending on the said library. This implements
the _live at HEAD_ workflow, a term made popular by
[Titus Winters](https://www.youtube.com/watch?v=tISy7EJQPzI) from Google.
