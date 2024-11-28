# Copyright (c) Prophesee S.A. - All Rights Reserved
#
# Subject to Prophesee Metavision Licensing Terms and Conditions ("License T&C's").
# You may not use this file except in compliance with these License T&C's.
# A copy of these License T&C's is located in the "licensing" folder accompanying this file.

import metavision.base


def random_list_float(shape, n=1):
    """Get a random float array"""
    assert n > 0
    return [metavision.base.random_float(shape) for _ in range(n)]


def random_list_int(shape, n=1):
    """Get a random int array"""
    assert n > 0
    return [metavision.base.random_int(shape) for _ in range(n)]
