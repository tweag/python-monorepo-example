set shell := ["bash", "-uc"]
export PATH := join(justfile_directory(), ".venv/bin") + ":" + env_var('PATH')

import 'private.just'

_clean-build:
    rm -rf dist

# Build the python project, will at least generate a wheel and maybe .tar.gz distribution if the project is pure python
[no-cd]
build: _clean-build
    @echo "🚀 Building project"
    uv build --all-packages

# Test with pytest
[no-cd]
test package *flags:
    @echo "🚀 Testing code: Running pytest"
    uv sync --all-extras --all-packages --index https://$JFROG_USERNAME:$JFROG_TOKEN@artifactory.devops.devops-psee.net/artifactory/api/pypi/prophesee-cimaging-pypi/simple
    pytest {{package}} -vv --cov --cov-config=pyproject.toml --cov-report xml:{{package}}/coverage.xml --junitxml={{package}}/report.xml {{flags}}

# Package the python
[no-cd]
package: build

# Publish python distribution on Artifactory
[no-cd]
publish *flags: _check-jfrog package
    @echo "🚀 Publishing wheels"
    uv publish \
        --publish-url https://artifactory.devops.devops-psee.net/artifactory/api/pypi/prophesee-cimaging-pypi \
        -u ${JFROG_USERNAME} \
        -p ${JFROG_TOKEN} \
        dist/*
