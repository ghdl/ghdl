# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
#  Authors:
#    Patrick Lehmann
#
# Package module:   Python binding and low-level API for shared library 'libghdl'.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================

from typing import Callable, List

from pydecor import export


@export
def EnumLookupTable(cls) -> Callable:
    """
    Decorator to precalculate a enum lookup table (LUT) for enum position to
    enum literal name.

    .. todo:: Make compatible to chained decorators

    :param cls: Enumerator class for which a LUT shall be pre-calculated.
    """

    def decorator(func) -> Callable:
        def gen() -> List[str]:
            d = [e for e in dir(cls) if e[0] != "_"]
            res = [None] * len(d)
            for e in d:
                res[getattr(cls, e)] = e
            return res

        __lut = gen()

        def wrapper(id: int) -> str:
            # function that replaces the placeholder function
            return __lut[id]

        return wrapper

    return decorator
