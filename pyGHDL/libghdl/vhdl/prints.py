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

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
@BindToLibGHDL("vhdl__prints__print_string")
def Print_String(File: int, Handle: c_void_p) -> None:
    """
    Reindent all lines of F between [First_Line; Last_Line] to :obj:`Handle`.

    :param File:      File to indent lines within. Type: ``Iir_Design_File``
    :param Handle:    undocumented. Type: ``Vstring_Acc``
    :param FirstLine: undocumented.
    :param LastLine:  undocumented.
    """


@export
@BindToLibGHDL("vhdl__prints__allocate_handle")
def Allocate_Handle() -> c_void_p:
    """
    .. todo:: Undocumented in Ada code.

    :return: undocumented. Type: ``Vstring_Acc``
    """


@export
@BindToLibGHDL("vhdl__prints__get_length")
def Get_Length(Handle: c_void_p) -> int:
    """
    .. todo:: Undocumented in Ada code.

    :param Handle: undocumented. Type: ``Vstring_Acc``
    :return:       undocumented.
    """


@export
@BindToLibGHDL("vhdl__prints__get_c_string")
def Get_C_String(Handle: c_void_p) -> c_char_p:
    """
    .. todo:: Undocumented in Ada code.

    :param Handle: undocumented. Type: ``Vstring_Acc``
    :return:       Type: ``Grt.Types.Ghdl_C_String``
    """


@export
@BindToLibGHDL("vhdl__prints__free_handle")
def Free_Handle(Handle: c_void_p) -> None:
    """
    .. todo:: Undocumented in Ada code.

    :param Handle: undocumented. Type: ``Vstring_Acc``
    """
