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
# Package module:   DOM: IIR helper functions
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
from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import Mode

from pyGHDL.libghdl import LibGHDLException, name_table, errorout_memory
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes, utils
from pyGHDL.libghdl.vhdl.nodes import Null_Iir
from pyGHDL.dom import DOMException

__all__ = []

__MODE_TRANSLATION = {
    nodes.Iir_Mode.In_Mode: Mode.In,
    nodes.Iir_Mode.Out_Mode: Mode.Out,
    nodes.Iir_Mode.Inout_Mode: Mode.InOut,
    nodes.Iir_Mode.Buffer_Mode: Mode.Buffer,
    nodes.Iir_Mode.Linkage_Mode: Mode.Linkage,
}


@export
def CheckForErrors() -> None:
    errorCount = errorout_memory.Get_Nbr_Messages()
    errors = []
    if errorCount != 0:
        for i in range(errorCount):
            rec = errorout_memory.Get_Error_Record(i + 1)
            # FIXME: needs help from @tgingold
            fileName = ""  # name_table.Get_Name_Ptr(files_map.Get_File_Name(rec.file))
            message = errorout_memory.Get_Error_Message(i + 1)

            errors.append(f"{fileName}:{rec.line}:{rec.offset}: {message}")

        raise DOMException("Error raised in libghdl.") from LibGHDLException("libghdl: Internal error.", errors)


@export
def GetIirKindOfNode(node: Iir) -> nodes.Iir_Kind:
    """Return the kind of a node in the IIR tree."""
    if node == Null_Iir:
        raise ValueError("GetIirKindOfNode: Parameter 'node' must not be 'Null_iir'.")

    kind: int = nodes.Get_Kind(node)
    return nodes.Iir_Kind(kind)


@export
def GetNameOfNode(node: Iir) -> str:
    """Return the python string from node :obj:`node` identifier."""
    if node == Null_Iir:
        raise ValueError("GetNameOfNode: Parameter 'node' must not be 'Null_iir'.")

    identifier = utils.Get_Source_Identifier(node)
    return name_table.Get_Name_Ptr(identifier)


@export
def GetModeOfNode(node: Iir) -> Mode:
    """Return the mode of a :obj:`node`."""
    if node == Null_Iir:
        raise ValueError("GetModeOfNode: Parameter 'node' must not be 'Null_iir'.")

    try:
        return __MODE_TRANSLATION[nodes.Get_Mode(node)]
    except KeyError:
        raise LibGHDLException("Unknown mode.")
