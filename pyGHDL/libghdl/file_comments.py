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

from ctypes import c_uint32
from typing import TypeVar

from pyTooling.Decorators import export

from pyGHDL.libghdl._types import SourceFileEntry, SourcePtr
from pyGHDL.libghdl._decorator import BindToLibGHDL
import pyGHDL.libghdl.files_map as files_map

__all__ = [
    "Comment_Index",
    "No_Comment_Index",
]

Comment_Index = TypeVar("Comment_Index", bound=c_uint32)

No_Comment_Index = 0


@export
@BindToLibGHDL("file_comments__find_first_comment")
def Find_First_Comment(File: SourceFileEntry, N: c_uint32) -> Comment_Index:
    """
    Get the first comment associated to a node.
    :param N:    Node
    :param File: Source file for node
    :return:     The first comment index, or No_Comment_Index if none.
    """
    return 0


@export
@BindToLibGHDL("file_comments__get_comment_start")
def Get_Comment_Start(File: SourceFileEntry, Idx: Comment_Index) -> SourcePtr:
    """
    Get the start of comment
    """


@export
@BindToLibGHDL("file_comments__get_comment_last")
def Get_Comment_Last(File: SourceFileEntry, Idx: Comment_Index) -> SourcePtr:
    """
    Get the end of comment
    """


def Get_Comment(File: SourceFileEntry, Idx: Comment_Index) -> str:
    """
    Get a comment
    """
    buf = files_map.Get_File_Buffer(File)
    s = Get_Comment_Start(File, Idx)
    l = Get_Comment_Last(File, Idx)
    return buf[s : l + 1].decode("iso-8859-1")


@export
@BindToLibGHDL("file_comments__get_next_comment")
def Get_Next_Comment(File: SourceFileEntry, Idx: Comment_Index) -> Comment_Index:
    """
    Get the next comment
    """
