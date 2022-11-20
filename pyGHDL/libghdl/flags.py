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

from pyGHDL.libghdl import libghdl

__all__ = [
    "Flag_Elocations",
    "Verbose",
    "Flag_Elaborate_With_Outdated",
    "Flag_Force_Analysis",
    "Flag_Gather_Comments",
]

assert sizeof(c_bool) == 1

Flag_Elocations = c_bool.in_dll(libghdl, "flags__flag_elocations")

Verbose = c_bool.in_dll(libghdl, "flags__verbose")

Flag_Elaborate_With_Outdated = c_bool.in_dll(libghdl, "flags__flag_elaborate_with_outdated")

Flag_Force_Analysis = c_bool.in_dll(libghdl, "flags__flag_force_analysis")

Flag_Gather_Comments = c_bool.in_dll(libghdl, "flags__flag_gather_comments")
