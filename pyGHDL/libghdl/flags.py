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

from ctypes import c_bool, sizeof
from enum import unique, IntEnum

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl

__all__ = [
    "Flag_Elocations",
    "Verbose",
    "MB_Comment",
    "Explicit",
    "Relaxed",
    "Flag_Elaborate_With_Outdated",
    "Flag_Force_Analysis",
    "AMS_Vhdl",
    "Flag_Gather_Comments",
]

assert sizeof(c_bool) == 1


@export
@unique
class VhdlStandard(IntEnum):
    """An enumeration representing libghdl's internal ``Vhdl_Std_Type`` enumeration type."""

    Vhdl_87 = 0  #: VHDL'87
    Vhdl_93 = 1  #: VHDL'93
    Vhdl_00 = 2  #: VHDL'2000
    Vhdl_02 = 3  #: VHDL'2002
    Vhdl_08 = 4  #: VHDL'2008
    Vhdl_19 = 5  #: VHDL'2019


Flag_Elocations = c_bool.in_dll(libghdl, "flags__flag_elocations")

Verbose = c_bool.in_dll(libghdl, "flags__verbose")  #: Internal boolean flag representing :option:`-v`.
MB_Comment = c_bool.in_dll(libghdl, "flags__mb_comment")  #: Internal boolean flag representing :option:`--mb-comment`.
Explicit = c_bool.in_dll(libghdl, "flags__flag_explicit")  #: Internal boolean flag representing :option:`-fexplicit`.
Relaxed = c_bool.in_dll(
    libghdl, "flags__flag_relaxed_rules"
)  #: Internal boolean flag representing :option:`-frelaxed`.

Flag_Elaborate_With_Outdated = c_bool.in_dll(libghdl, "flags__flag_elaborate_with_outdated")

Flag_Force_Analysis = c_bool.in_dll(libghdl, "flags__flag_force_analysis")

AMS_Vhdl = c_bool.in_dll(libghdl, "flags__ams_vhdl")  #: Internal boolean flag representing :option:`-ams`.

Flag_Gather_Comments = c_bool.in_dll(libghdl, "flags__flag_gather_comments")
