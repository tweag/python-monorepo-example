"""Script to generate the CI a new library."""

MODULE_NAME: str = "{{ cookiecutter.module_name }}"
PACKAGE_NAME: str = "{{ cookiecutter.package_name }}"
CI_FILE_PATH: str = f"../../.github/workflows/ci_{MODULE_NAME}.yml"

with open(CI_FILE_PATH, "w") as handle:
    handle.writelines(
        f"""---
name: CI libs/base

on:
  pull_request:
    paths:
      - 'dev-requirements.txt'
      - 'pip-requirements.txt'
      - '.github/workflows/ci_python_reusable.yml'
      - '.github/workflows/ci_{MODULE_NAME}.yml'
      - 'libs/{MODULE_NAME}/**'
  workflow_dispatch:  # Allows to trigger the workflow manually in GitHub UI

jobs:
  ci-libs-{PACKAGE_NAME}:
    uses:
      ./.github/workflows/ci_python_reusable.yml
    with:
      working-directory: libs/{MODULE_NAME}
    secrets: inherit"""
    )
