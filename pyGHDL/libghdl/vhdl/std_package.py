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

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import (
    LocationType,
    Iir_Package_Declaration,
    Iir_Enumeration_Type_Definition,
)


__all__ = ["Std_Location", "Standard_Package", "Character_Type_Definition"]


Std_Location: LocationType = c_int32.in_dll(libghdl, "vhdl__std_package__std_location")
"""Virtual location for the ``std.standard`` package. Use ``.value`` to access this variable inside libghdl."""

Standard_Package: Iir_Package_Declaration = c_int32.in_dll(libghdl, "vhdl__std_package__standard_package")
"""Virtual package ``std.package``. Use ``.value`` to access this variable inside libghdl."""

Character_Type_Definition: Iir_Enumeration_Type_Definition = c_int32.in_dll(
    libghdl, "vhdl__std_package__character_type_definition"
)
"""Predefined character. Use ``.value`` to access this variable inside libghdl."""
