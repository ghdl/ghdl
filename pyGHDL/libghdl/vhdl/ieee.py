# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:          Tristan Gingold
#
# Package package:  Python binding and low-level API for shared library 'libghdl'.
#
# License:
# ============================================================================
# Copyright (C) 2019-2020 Tristan Gingold
#
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
#
from pyGHDL.libghdl import libghdl
from ctypes import c_int


Std_Logic_1164_Pkg = c_int.in_dll(
    libghdl, "vhdl__ieee__std_logic_1164__std_logic_1164_pkg"
)

# Get value
Std_Logic_Type = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__std_logic_type")

# Get value
Std_Logic_Vector_Type = c_int.in_dll(
    libghdl, "vhdl__ieee__std_logic_1164__std_logic_vector_type"
)

# Get value
# Rising_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__rising_edge")

# Get value
# Falling_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__falling_edge")
