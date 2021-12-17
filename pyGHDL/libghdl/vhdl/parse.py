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

from ctypes import c_bool

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl._decorator import BindToLibGHDL


__all__ = ["Flag_Parse_Parenthesis"]


Flag_Parse_Parenthesis = c_bool.in_dll(libghdl, "vhdl__parse__flag_parse_parenthesis")


@export
@BindToLibGHDL("vhdl__parse__parse_design_file")
def Parse_Design_File() -> Iir:
    """
    Parse a file.

    ..note:: The scanner must have been initialized as for parse_design_unit.

    :return: Return :obj:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir` in case of error. Type: ``Iir_Design_File``
    """
    return 0
