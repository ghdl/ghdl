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
#
# Package package:  Python binding and low-level API for shared library 'libghdl'.
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

from ctypes import c_int

from pyGHDL.libghdl import libghdl

__all__ = ["Std_Logic_1164_Pkg", "Std_Logic_Type", "Std_Logic_Vector_Type"]

Std_Logic_1164_Pkg = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__std_logic_1164_pkg")

# Get value
Std_Logic_Type = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__std_logic_type")

# Get value
Std_Logic_Vector_Type = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__std_logic_vector_type")

# Get value
# Rising_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__rising_edge")

# Get value
# Falling_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__falling_edge")
