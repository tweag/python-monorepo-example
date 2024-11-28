_init-git:
    #!/bin/bash
    if ! test -d ".git"; then
    	echo "🚀 Initializing git..."
        git init
        git branch -m main
    	echo "🚀 Installing pre-commit hooks..."
        pre-commit install
    fi

_check-uv:
    #!/bin/bash
    if ! command -v uv &> /dev/null; then
    	echo "🚀 uv is not installed, installing..."
    	curl -LsSf https://astral.sh/uv/install.sh | sh
    else
    	echo "🚀 uv is already installed, skipping..."
    fi
    expecting_in_path="$HOME/.cargo/bin"
    if echo $PATH | grep -q $expecting_in_path; then
        echo "🚀 uv is ready to be used"
    else
        echo "🚧 Adding $expecting_in_path to PATH, you should add it permanently"
        export PATH=$expecting_in_path:$PATH
    fi

_setup-conan:
    #!/bin/bash
    echo "🚀 Disabling conan installation since no C++ is required"

_check-jfrog:
    #!/bin/bash
    if [[ -z "${JFROG_USERNAME}" || -z ${JFROG_TOKEN} ]]; then
        echo "⛔ JFROG_USERNAME or JFROG_TOKEN is undefined"
        exit 1
    fi

_check-file file:
    #!/bin/bash
    if [ ! -f {{file}} ]; then
        echo "⛔ Required file {{file}} is missing"
        exit 1
    fi

_check-sonartoken:
    #!/bin/bash
    if [ -z "${SONAR_TOKEN}" ]; then
    	echo "⛔ No environment variable SONAR_TOKEN exist on the system so no analysis can be run."
    	echo "⛔ Go on sonarqube, generate a user token and save it as SONAR_TOKEN then rerun"
    	exit 1
    fi
