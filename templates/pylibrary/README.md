# Python library template

A cookiecutter template to create new Python libraries in the monorepo.

## How to use

### Without Nix

Load the monorepo's top-level sandbox, that contains cookiecutter:

```
# From the monorepo root
# Create the sandbox (only do that the first time)
python3 -m venv .venv
pip install -r pip-requirements.txt
pip install -r dev-requirements.txt
# Load the sandbox (todo every time)
source .venv/bin/activate
```

Then run cookiecutter _from the root directory of the monorepo_:

```
cookiecutter templates/pylibrary --output-dir libs/
```

Notes:

- When entering a package name or module name, do not include a 'mycorp-' prefix.
- When entering your github handle, include the '@' prefix

### With Nix

Run cookiecutter _from the root directory of the monorepo_:

```
nix run nixpkgs#cookiecutter -- templates/pylibrary --output-dir libs/
```

