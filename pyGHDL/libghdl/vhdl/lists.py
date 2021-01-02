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
from ctypes import c_int32, c_bool, POINTER, Structure


List_Type = c_int32


class Iterator(Structure):
    _fields_ = [("chunk", c_int32), ("chunk_idx", c_int32), ("remain", c_int32)]


Iterate = libghdl.vhdl__lists__iterate
Iterate.argstype = [List_Type]
Iterate.restype = Iterator

Is_Valid = libghdl.vhdl__lists__is_valid
Is_Valid.argstype = [POINTER(Iterator)]
Is_Valid.restype = c_bool

Next = libghdl.vhdl__lists__next
Next.argstype = [POINTER(Iterator)]
Next.restype = None

Get_Element = libghdl.vhdl__lists__get_element
Get_Element.argstype = [POINTER(Iterator)]
Get_Element.restype = c_int32

Get_Nbr_Elements = libghdl.vhdl__lists__get_nbr_elements
Get_Nbr_Elements.argtype = [List_Type]
Get_Nbr_Elements.restype = c_int32

Create_Iir_List = libghdl.vhdl__lists__create_list

Destroy_Iir_List = libghdl.vhdl__lists__destroy_list
