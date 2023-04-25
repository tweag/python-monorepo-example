"""
Check that the template works as intended.
"""

import pathlib
import tempfile

import cookiecutter.main

TEMPLATE_DIRECTORY = str(pathlib.Path(__file__).parent.parent)


def test_template():
    """
    Check that the template works with a vanilla cookiecutter call.
    """
    with tempfile.TemporaryDirectory(suffix="test-template") as tmp_dir:
        cookiecutter.main.cookiecutter(
            template=TEMPLATE_DIRECTORY,
            output_dir=str(tmp_dir),
            no_input=True,
            accept_hooks=False,  # Don't test the hooks as these dont work on github
        )

        output_paths = {
            str(path.relative_to(tmp_dir)) for path in pathlib.Path(tmp_dir).glob("**/*")
        }

        assert "library_name/pyproject.toml" in output_paths
