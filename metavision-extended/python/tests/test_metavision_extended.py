# Copyright (c) Prophesee S.A. - All Rights Reserved
#
# Subject to Prophesee Metavision Licensing Terms and Conditions ("License T&C's").
# You may not use this file except in compliance with these License T&C's.
# A copy of these License T&C's is located in the "licensing" folder accompanying this file.

import metavision.extended


def test_random_list_float() -> None:
    a = metavision.extended.random_list_float((5, 10), 2)
    assert len(a) == 2
    for arr in a:
        assert arr.shape == (5, 10)


def test_random_list_int() -> None:
    a = metavision.extended.random_list_int((5, 10), 2)
    assert len(a) == 2
    for arr in a:
        assert arr.shape == (5, 10)
