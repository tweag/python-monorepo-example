"""Example module of the fancy library, consuming the base library"""

from mycorp import base


def add3(x: int, y: int, z: int) -> int:
    """
    Adds three integers

    Args:
        x: the left operand
        y: the middle operand
        z: the right operand
    Returns:
        The sum of x, y, and z
    """
    return base.adder2.add2(base.adder2.add2(x, y), z)
