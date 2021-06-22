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
# Package module:   DOM: Interface items (e.g. generic or port)
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
from pyGHDL.libghdl._types import Iir
from pydecor import export

from pyVHDLModel.VHDLModel import Mode

from pyGHDL.libghdl import LibGHDLException, name_table, files_map
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom.Misc import Position


__all__ = []

__MODE_TRANSLATION = {
    nodes.Iir_Mode.In_Mode: Mode.In,
    nodes.Iir_Mode.Out_Mode: Mode.Out,
    nodes.Iir_Mode.Inout_Mode: Mode.InOut,
    nodes.Iir_Mode.Buffer_Mode: Mode.Buffer,
    nodes.Iir_Mode.Linkage_Mode: Mode.Linkage,
}


@export
def GetIirKindOfNode(node: Iir) -> nodes.Iir_Kind:
    kind: int = nodes.Get_Kind(node)
    return nodes.Iir_Kind(kind)


@export
def GetNameOfNode(node: Iir) -> str:
    """Return the python string from node :obj:`node` identifier"""
    identifier = nodes.Get_Identifier(node)
    return name_table.Get_Name_Ptr(identifier)


@export
def GetModeOfNode(node: Iir) -> Mode:
    """Return the mode of a :obj:`port`."""
    try:
        return __MODE_TRANSLATION[nodes.Get_Mode(node)]
    except KeyError:
        raise LibGHDLException("Unknown mode.")


@export
def GetPositionOfNode(node: Iir) -> Position:
    location = nodes.Get_Location(node)
    file = files_map.Location_To_File(location)
    fileName = name_table.Get_Name_Ptr(files_map.Get_File_Name(file))
    #    position = files_map.Location_File_To_Pos(location, file)
    line = files_map.Location_File_To_Line(location, file)
    column = files_map.Location_File_Line_To_Offset(location, file, line)

    return Position(fileName, line, column)
