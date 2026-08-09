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
"""
Python binding for the Ada package ``Name_Table`` in *libghdl*.

*libghdl* interns every identifier in a table and refers to it by a :data:`~pyGHDL.libghdl._types.NameId`. These
functions convert between an identifier and its Python string.
"""

from ctypes import c_char, c_char_p

from pyTooling.Decorators import export

from pyGHDL.libghdl import ENCODING
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
    return 0  # pragma: no cover


# @export
@BindToLibGHDL("name_table__get_name_ptr")
def _Get_Name_Ptr(Id: NameId) -> c_char_p:
    """
    Raw binding returning the identifier as a C string.

    Use :func:`Get_Name_Ptr` instead, which decodes it to a Python string.

    :param Id: The identifier to read.
    :returns:   The identifier as a C string.

    """
    """"""
    return ""  # pragma: no cover


@export
def Get_Name_Ptr(Id: NameId) -> str:
    """
    Get the string corresponding to identifier ID. The address is valid until
    the next call to Get_Identifier (which may reallocate the string table).
    The string is NUL-terminated (this is done by get_identifier).

    :param Id: NameId for the identifier to query.
    :return:   Identifier as string.
    """
    return _Get_Name_Ptr(Id).decode(ENCODING)


# @export
@BindToLibGHDL("name_table__get_character")
def _Get_Character(Id: NameId) -> c_char:
    """
    Raw binding returning a character literal's value as a C string.

    Use :func:`Get_Character` instead, which decodes it.

    :param Id: The identifier of the character literal.
    :returns:   The character as a C string.

    """
    """"""
    return 0  # pragma: no cover


@export
def Get_Character(Id: NameId) -> str:
    """
    Get the string corresponding to character identifier ID.

    .. note::

       This is used for character literals and enumeration literals.

    :param Id: NameId for the identifier to query.
    :return:   Get the character of the identifier.
    """
    return _Get_Character(Id).decode(ENCODING)


# @export
@BindToLibGHDL("name_table__get_identifier_with_len")
def _Get_Identifier(string: c_char_p, length: int) -> NameId:
    """
    Raw binding interning a C string and returning its identifier.

    Use :func:`Get_Identifier` instead, which encodes a Python string first.

    :param string: The string to intern, encoded.
    :param length: The number of characters in ``string``.
    :returns:                The identifier of the interned string.
    """
    """"""
    return 0  # pragma: no cover


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
    string = string.encode(ENCODING)
    return _Get_Identifier(c_char_p(string), len(string))
