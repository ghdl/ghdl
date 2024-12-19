# =============================================================================
#               ____ _   _ ____  _          _
#  _ __  _   _ / ___| | | |  _ \| |      __| | ___  _ __ ___
# | '_ \| | | | |  _| |_| | | | | |     / _` |/ _ \| '_ ` _ \
# | |_) | |_| | |_| |  _  | |_| | |___ | (_| | (_) | | | | | |
# | .__/ \__, |\____|_| |_|____/|_____(_)__,_|\___/|_| |_| |_|
# |_|    |___/
# =============================================================================
# Authors:
#   Patrick Lehmann
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
"""
Document object model (DOM) for :mod:`pyGHDL.libghdl` based on :doc:`pyVHDLModel <vhdlmodel:index>`.
"""
from pathlib import Path

from pyTooling.Decorators import export
from pyTooling.MetaClasses import ExtendedType

from pyGHDL import GHDLBaseException
from pyGHDL.libghdl import files_map, name_table
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes


@export
class Position:
    """Represents the source code position of a IIR node in a source file."""

    _filename: Path
    _line: int
    _column: int

    def __init__(self, filename: Path, line: int, column: int) -> None:
        self._filename = filename
        self._line = line
        self._column = column

    @classmethod
    def parse(cls, node: Iir) -> "Position":
        """Return the source code position of a IIR node."""
        if node == nodes.Null_Iir:
            raise ValueError("Position.parse(): Parameter 'node' must not be 'Null_iir'.")

        location = nodes.Get_Location(node)
        file = files_map.Location_To_File(location)
        fileNameId = files_map.Get_File_Name(file)
        fileName = name_table.Get_Name_Ptr(fileNameId)
        line = files_map.Location_File_To_Line(location, file)
        column = files_map.Location_File_Line_To_Offset(location, file, line)

        return cls(Path(fileName), line, column)

    @property
    def Filename(self) -> Path:
        """
        Read-only property to access the filename this source code position referres to (:attr:`_filename`).

        :returns: The source code position's filename.
        """
        return self._filename

    @property
    def Line(self) -> int:
        """
        Read-only property to access the line number this source code position referres to (:attr:`_line`).

        :returns: The source code position's line.
        """
        return self._line

    @property
    def Column(self) -> int:
        """
        Read-only property to access the column this source code position referres to (:attr:`_column`).

        :returns: The source code position's column.
        """
        return self._column

    def __str__(self):
        return f"{self._filename}:{self._line}:{self._column}"


@export
class DOMMixin(metaclass=ExtendedType, mixin=True):
    _iirNode: Iir
    _position: Position

    def __init__(self, node: Iir) -> None:
        self._iirNode = node
        self._position = None

    @property
    def Position(self) -> Position:
        """
        Cached read-only property to access an IIR's position in source code (:attr:`_position`).

        If :attr:`_position` is None, resolve the position object from :attr:`_iirNode`

        :returns: The IIR's position in the source file.
        """
        if self._position is None:
            self._position = Position.parse(self._iirNode)

        return self._position


@export
class DOMException(GHDLBaseException):
    pass
