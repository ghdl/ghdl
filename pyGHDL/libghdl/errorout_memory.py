# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
#  Authors:
#    Tristan Gingold
#    Patrick Lehmann
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

from ctypes import c_int8, c_int32, c_char_p, Structure

from pydecor import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import ErrorIndex


@export
class Error_Message(Structure):
    """
    Id : Msgid_Type
      Message error/warning id

    Group : Group_Type;
      Whether this is an single message or a related one.

    File : Source_File_Entry;
      Error soure file.

    Line : Natural;
      The first line is line 1, 0 can be used when line number is not relevant.

    Offset : Natural;
      Offset in the line.  The first character is at offset 0.

    Length : Natural;
      Length of the location (for a range). It is assumed to be on the same line;
      use 0 when unknown.
    """

    _fields_ = [
        ("id", c_int8),
        ("group", c_int8),
        ("file", c_int32),
        ("line", c_int32),
        ("offset", c_int32),
        ("length", c_int32),
    ]


# Values for group:
Msg_Single = 0
Msg_Main = 1
Msg_Related = 2
Msg_Last = 3


@export
def Install_Handler() -> None:
    """Install the handlers for reporting errors."""
    libghdl.errorout__memory__install_handler()


@export
def Get_Nbr_Messages() -> ErrorIndex:
    """
    Get number of error messages available.

    :return: Number of messages available.
    """
    return libghdl.errorout__memory__get_nbr_messages()


@export
def Get_Error_Record(Idx: ErrorIndex) -> Error_Message:
    """
    Get error messages by index :obj:`Idy` as structure :class:`Error_Message`.

    :param Idx: Index from 1 to ``Nbr_Messages`` See :func:`Get_Nbr_Messages`.
    :return:    Type: ``Error_Message``
    """
    func = libghdl.errorout__memory__get_error_record
    func.argstypes = [c_int32]
    func.restype = Error_Message

    return func(Idx)


@export
def Get_Error_Message(Idx: ErrorIndex) -> str:
    """
    Get error messages by index :obj:`Idy` as string.

    :param Idx: Index from 1 to ``Nbr_Messages`` See :func:`Get_Nbr_Messages`.
    :return:    Type: ``Error_Message``
    """
    func = libghdl.errorout__memory__get_error_message_addr
    func.argstype = [c_int32]
    func.restype = c_char_p

    return func(Idx).decode("utf-8")


@export
def Clear_Errors() -> None:
    """Remove all error messages."""
    libghdl.errorout__memory__clear_errors()
