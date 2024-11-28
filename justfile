set shell := ["bash", "-uc"]
export PATH := join(justfile_directory(), ".venv/bin") + ":" + env_var('PATH')

import 'just/private.just'

# Python related jobs
mod py 'just/py.just'

mod base 'metavision-base/justfile'

# Install uv environement
install: _check-uv _check-jfrog
    @echo "🚀 Creating virtual environment using uv"
    uv venv --allow-existing
    uv sync --all-extras --all-packages --no-install-project --index https://$JFROG_USERNAME:$JFROG_PASSWORD@artifactory.devops.devops-psee.net/artifactory/api/pypi/prophesee-cimaging-pypi/simple
    @just _setup-conan
    @just _init-git

# Check the lockfile, run precommit & deptry
check:
    @echo "🚀 Checking uv lock file consistency with 'pyproject.toml': Running uv check --lock"
    uv sync --locked --all-extras --all-packages
    @echo "🚀 Linting code: Running pre-commit"
    pre-commit run -a

# Execute any command in the just environment
x *cmd:
    #!/bin/bash
    eval {{cmd}}
