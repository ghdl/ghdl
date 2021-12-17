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

from ctypes import c_int, c_bool

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import SourceFileEntry, NameId
from pyGHDL.libghdl._decorator import BindToLibGHDL


__all__ = ["Current_Token", "Flag_Comment"]

# This is a c_int, so you want to use its .value
Current_Token = c_int.in_dll(libghdl, "vhdl__scanner__current_token")
Flag_Comment = c_bool.in_dll(libghdl, "vhdl__scanner__flag_comment")


@export
@BindToLibGHDL("vhdl__scanner__set_file")
def Set_File(SourceFile: SourceFileEntry) -> None:
    """
    Initialize the scanner with file :obj:`SourceFile`.

    :param SourceFile: File to scan.
    """


@export
@BindToLibGHDL("vhdl__scanner__close_file")
def Close_File() -> None:
    """Finalize the scanner."""


@export
@BindToLibGHDL("vhdl__scanner__scan")
def Scan() -> None:
    """Get a new token."""


@export
@BindToLibGHDL("vhdl__scanner__get_current_line")
def Get_Current_Line() -> int:
    """
    Get the current location, or the location of the current token.

    Since a token cannot spread over lines, file and line of the current token are
    the same as those of the current position. The offset is the offset in the current line.

    :return: Current token's line.
    """
    return 0


@export
@BindToLibGHDL("vhdl__scanner__get_token_offset")
def Get_Token_Offset() -> int:
    """
    Get the current token's offset in the current line.

    :return: Current token's offset.
    """
    return 0


@export
@BindToLibGHDL("vhdl__scanner__get_token_position")
def Get_Token_Position():
    """
    Get the current token's position.

    :return: Current token's position. Type: ``Source_Ptr``
    """
    return 0


@export
@BindToLibGHDL("vhdl__scanner__get_position")
def Get_Position():
    """
    Get the current position.

    :return: Current position. Type: ``Source_Ptr``
    """
    return 0


@export
@BindToLibGHDL("vhdl__scanner__current_identifier")
def Current_Identifier() -> NameId:
    """
    When :attr:`~pyGHDL.libghdl.vhdl.scanner.Current_Token` is an
    ``tok_identifier``, ``tok_char`` or ``tok_string``, its name_id can be
    retrieved via this function.

    :return: NameId of the current token.
    """
    return 0
