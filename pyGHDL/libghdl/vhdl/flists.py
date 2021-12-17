# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:
#   Tristan Gingold
#   Patrick Lehmann
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
from ctypes import c_int32

from pyTooling.Decorators import export

from pyGHDL.libghdl._decorator import BindToLibGHDL


__all__ = ["Flist_Type", "Ffirst"]

Flist_Type = c_int32  #: First index of a ``FList``.

Ffirst = 0


@export
@BindToLibGHDL("vhdl__flists__flast")
def Flast(FList: int) -> int:
    """
    Last index of :obj:`FList`.

    .. hint:: Could be used to iterate.

    :param FList: List to query.
    :return:      Index of the last element in the list.
    """
    return 0


@export
@BindToLibGHDL("vhdl__flists__length")
def Length(FList: int) -> int:
    """
    Get the length of :obj:`FList`.

    :param FList: List to query.
    :return:      Number of elements in the list.
    """
    return 0


@export
@BindToLibGHDL("vhdl__flists__get_nth_element")
def Get_Nth_Element(FList: int, N: int) -> int:
    """
    Get the N-th element of :obj:`FList`.

    First element has index 0.

    :param FList: List to query.
    :return:      Type: ``El_Type``
    """
    return 0
