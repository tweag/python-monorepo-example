# Copyright (c) Prophesee S.A. - All Rights Reserved
#
# Subject to Prophesee Metavision Licensing Terms and Conditions ("License T&C's").
# You may not use this file except in compliance with these License T&C's.
# A copy of these License T&C's is located in the "licensing" folder accompanying this file.

from typing import Tuple

import numpy as np


def random_float(shape: Tuple[int, ...]) -> np.ndarray:
    """Get a random float array"""
    return np.random.random(shape).astype(np.float32)


def random_int(shape: Tuple[int, ...]) -> np.ndarray:
    """Get a random int array"""
    return np.random.random(shape).astype(np.int32)
