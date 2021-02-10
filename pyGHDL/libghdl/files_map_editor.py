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

from ctypes import c_int32, c_char_p, c_bool

from pydecor import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import SourceFileEntry


@export
def Replace_Text(
    File: SourceFileEntry,
    Start_Line: int,
    Start_Offset: int,
    End_Line: int,
    End_Offset: int,
    Text_Pointer,
    Text_Length: int,
) -> bool:
    """Replace [START; END) by TEXT.

    .. todo:: Replace ``Text_Pointer`` and ``Text_Length`` with Python string

    :param File:         File where to replace a text section.
    :param Start_Line:
    :param Start_Offset:
    :param End_Line:
    :param End_Offset:
    :param Text_Pointer: Type: ``File_Buffer_Ptr``
    :param Text_Length:  Type: ``Source_Ptr``
    :return:             Return True in case of success, False in case of failure (the gap is too small).
    """
    func = libghdl.files_map__editor__replace_text_ptr
    func.argstype = [c_int32, c_int32, c_int32, c_int32, c_char_p, c_int32]
    func.restype = c_bool

    return func(
        File, Start_Line, Start_Offset, End_Line, End_Offset, Text_Pointer, Text_Length
    )


@export
def Fill_Text(File: SourceFileEntry, Text_Pointer, Text_Length: int) -> None:
    """Replace the content of :obj:`File` with TEXT.

    .. todo:: Replace ``Text_Pointer`` and ``Text_Length`` with Python string

    :param File:         File where to replace the content.
    :param Text_Pointer: Type: ``File_Buffer_Ptr``
    :param Text_Length:  Type: ``Source_Ptr``
    """
    libghdl.files_map__editor__fill_text_ptr(File, Text_Pointer, Text_Length)


@export
def Check_Buffer_Content(
    File: SourceFileEntry, String_Pointer, String_Length: int
) -> None:
    """
    Check that content of :obj:`File` is STR[1 .. STR_LEN].

    .. todo:: Replace ``String_Pointer`` and ``String_Length`` with Python string

    :param File:           File to check the content.
    :param String_Pointer: Type: ``File_Buffer_Ptr``
    :param String_Length:  Type: ``Source_Ptr``
    """
    libghdl.files_map__editor__check_buffer_content(File, String_Pointer, String_Length)


@export
def Copy_Source_File(Dest: SourceFileEntry, Src: SourceFileEntry) -> None:
    """
    Copy content of :obj:`Src` to :obj:`Dest`.

    .. warning:: The size of :obj:`Dest` must be large enough.

    Clear lines table of :obj:`Dest`.
    """
    return libghdl.files_map__editor__copy_source_file(Dest, Src)
