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
from ctypes import c_char, c_char_p

from pyTooling.Decorators import export

from pyGHDL.libghdl._types import NameId
from pyGHDL.libghdl._decorator import BindToLibGHDL

__all__ = ["Null_Identifier"]

Null_Identifier = 0


@export
@BindToLibGHDL("name_table__get_name_length")
def Get_Name_Length(Id: NameId) -> int:
    """
    Get the length of an identifier denoted by a ``NameId``.

    :param Id: NameId for the identifier to query.
    :return:   Length of the identifier.
    """
    return 0


# @export
@BindToLibGHDL("name_table__get_name_ptr")
def _Get_Name_Ptr(Id: NameId) -> c_char_p:
    """"""
    return ""


@export
def Get_Name_Ptr(Id: NameId) -> str:
    """
    Get the string corresponding to identifier ID. The address is valid until
    the next call to Get_Identifier (which may reallocate the string table).
    The string is NUL-terminated (this is done by get_identifier).

    :param Id: NameId for the identifier to query.
    :return:   Identifier as string.
    """
    return _Get_Name_Ptr(Id).decode("utf-8")


# @export
@BindToLibGHDL("name_table__get_character")
def _Get_Character(Id: NameId) -> c_char:
    """"""
    return 0


@export
def Get_Character(Id: NameId) -> str:
    """
    Get the string corresponding to character identifier ID.

    .. note::

       This is used for character literals and enumeration literals.

    :param Id: NameId for the identifier to query.
    :return:   Get the character of the identifier.
    """
    return _Get_Character(Id).decode("utf-8")


# @export
@BindToLibGHDL("name_table__get_identifier_with_len")
def _Get_Identifier(string: c_char_p, length: int) -> NameId:
    """"""
    return 0


@export
def Get_Identifier(string: str) -> NameId:
    """
    Get or create an entry in the name table.

    .. note::

       * an identifier is represented in all lower case letter,
       * an extended identifier is represented in backslashes, double internal
         backslashes are simplified.

    :param string: String to create or lookup.
    :return:       Id in name table.
    """
    string = string.encode("utf-8")
    return _Get_Identifier(c_char_p(string), len(string))
