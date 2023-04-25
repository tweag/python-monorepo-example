"""
Shared configuration and fixtures for pytest.
"""

import pytest


@pytest.fixture
def some_text() -> str:
    """
    Some fixture which can be reused in all test modules.
    """
    return "Some text"
