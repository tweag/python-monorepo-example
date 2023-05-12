"""
Tests of adder2
"""

from mycorp import base


def test_zero() -> None:
    """Tests the neutral element"""
    assert base.adder2.add2(0, 0) == 0
    assert base.adder2.add2(0, 3) == 3
    assert base.adder2.add2(0, 3) == base.adder2.add2(3, 0)


def test_some() -> None:
    """Some unit tests of add2"""
    assert base.adder2.add2(1, 3) == 4
    assert base.adder2.add2(1, 3) == base.adder2.add2(3, 1)
    assert base.adder2.add2(100, 2) == (base.adder2.add2(100, 1) + base.adder2.add2(1, 0))


# In a true repository we would add property tests here,
# using hypothesis:
# https://hypothesis.readthedocs.io/en/latest/
