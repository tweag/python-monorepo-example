"""
Tests of adder3
"""

# from mycorp import base, fancy # don't! This makes pyright not able to link
# one of the two packages (is it https://github.com/microsoft/pyright/issues/2882?)
from mycorp import base
from mycorp import fancy


def test_zero() -> None:
    """Test of add3 with zero"""
    assert fancy.adder3.add3(0, 0, 0) == 0
    assert fancy.adder3.add3(0, 1, 0) == 1
    assert fancy.adder3.add3(0, 1, 2) == 3
    assert fancy.adder3.add3(1, 2, 0) == 3


def test_add2_add3() -> None:
    """Test relation between add2 and add3"""
    assert fancy.adder3.add3(1, 2, 3) == (base.adder2.add2(1, 2) + base.adder2.add2(0, 3))
    assert fancy.adder3.add3(1, 2, 0) == base.adder2.add2(1, 2)


def test_some() -> None:
    """Some unit tests of add3"""
    assert fancy.adder3.add3(1, 2, 3) == 6
    assert fancy.adder3.add3(1, 2, 3) == fancy.adder3.add3(3, 2, 1)
    assert fancy.adder3.add3(100, 2, 3) == 105


# In a true repository we would add property tests here,
# using hypothesis:
# https://hypothesis.readthedocs.io/en/latest/
