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
#
from ctypes import c_char_p

from pyTooling.Decorators import export

from pyGHDL.libghdl._types import String8Id
from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
@BindToLibGHDL("str_table__string8_address")
def _String8_Address(Id: String8Id) -> c_char_p:
    """"""
    return ""


@export
def Get_String8_Ptr(Id: String8Id, Length: int) -> str:
    """
    Get the address of string8 ID. Note that as soon as a character is appended
    (using Append_String8) or a string8 is resized (using Resize_String8), an
    address previously returned is not valid anymore.

    :param Id: String8Id for the string to query.
    :return:   String8 as string.
    """
    return _String8_Address(Id)[:Length].decode("utf-8")
