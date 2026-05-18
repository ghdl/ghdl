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

from ctypes import c_int32, c_char_p, c_void_p

from pyTooling.Decorators import export

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
@BindToLibGHDL("vhdl__disp_tree__disp_tree")
def Disp_Tree(Tree: Iir, Flat: bool = False) -> None:
    """
    Disp a tree for debugging.

    :param Tree:      The node to display.
    """


@export
@BindToLibGHDL("vhdl__disp_tree__disp_iir")
def Disp_Iir(N: Iir, Indent: int, Depth: int) -> None:
    """
    Disp a node for debugging.

    :param N:      The node to display.
    """


def debug_iir(N: Iir) -> None:
    Disp_Iir(N, 0, 1)
