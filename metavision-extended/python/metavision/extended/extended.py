# Copyright (c) Prophesee S.A. - All Rights Reserved
#
# Subject to Prophesee Metavision Licensing Terms and Conditions ("License T&C's").
# You may not use this file except in compliance with these License T&C's.
# A copy of these License T&C's is located in the "licensing" folder accompanying this file.

from typing import List, Tuple

import metavision.base
import numpy as np


def random_list_float(shape: Tuple[int, ...], n: int = 1) -> List[np.ndarray]:
    """Get a random float array"""
    assert n > 0
    return [metavision.base.random_float(shape) for _ in range(n)]


def random_list_int(shape: Tuple[int, ...], n: int = 1) -> List[np.ndarray]:
    """Get a random int array"""
    assert n > 0
    return [metavision.base.random_int(shape) for _ in range(n)]
