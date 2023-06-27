# {{ cookiecutter.package_name }}

{{ cookiecutter.package_description }}

The project owner is [{{ cookiecutter.owner_github_handle }}](https://github.com/{{ cookiecutter.owner_github_handle.replace('@', '') }}).

## Development

If not already in a virtual environement, create and use one.
Read about it in the Python documentation: [venv — Creation of virtual environments](https://docs.python.org/3/library/venv.html).

```
python3 -m venv .venv
source .venv/bin/activate
```

Install the pinned pip version:

```
pip install -r $(git rev-parse --show-toplevel)/pip-requirements.txt
```

Finally, install the dependencies:

```
pip install -r $(git rev-parse --show-toplevel)/dev-requirements.txt -r requirements.txt
```

## Testing

Execute tests from the library's folder (after having loaded the virtual environment,
see above) as follows:

```
python3 -m pytest tests/
```

Execute the library's CI locally with [act](https://github.com/nektos/act) as follows:

```
act -j ci-libs-{{ cookiecutter.package_name }}
```
