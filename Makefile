.DEFAULT_GOAL := help

install: ## Install the necessary packages and builds the website.
	npm install
.PHONY: install

build: ## Build the website.
	npm run build
.PHONY: build

develop: ## Run a server for the website in development mode.
	npm run develop
.PHONY: develop

prettier: ## Run prettier on the codebase.
	npm run prettier -- --write
.PHONY: prettier

check: ## Run the linter in the codebase and build.
	npm run lint
	npm run build
.PHONY: lint

################################################################################
# Help
#
# This generates a sort of index for the Makefile. Running "make help" shows a
# list of commands, but only those that are documented - that is, those that
# have the following shape:
#
#   target: subtarget ## Some documentation.
#
# which will be shown as:
#
#   target               Some documentation
#
# in the index.

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
