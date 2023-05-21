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
#  Copyright (C) 2019-2022 Tristan Gingold
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

from pyVHDLModel.Base import Mode

from pyGHDL.libghdl import LibGHDLException, name_table, errorout_memory, files_map, file_comments
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes, utils
from pyGHDL.libghdl.vhdl.nodes import Null_Iir
from pyGHDL.dom import DOMException

__MODE_TRANSLATION = {
    nodes.Iir_Mode.In_Mode: Mode.In,
    nodes.Iir_Mode.Out_Mode: Mode.Out,
    nodes.Iir_Mode.Inout_Mode: Mode.InOut,
    nodes.Iir_Mode.Buffer_Mode: Mode.Buffer,
    nodes.Iir_Mode.Linkage_Mode: Mode.Linkage,
}
"""
Translation table if IIR modes to pyVHDLModel mode enumeration values.
"""


@export
def CheckForErrors() -> None:
    """
    Check if an error occurred in libghdl and raise an exception if so.

    **Behavior:**

    1. read the error buffer and clear it afterwards.
    2. convert it into a list of internal messages for a :exc:`LibGHDLException`.
    3. raise a :exc:`DOMException` with a nested :exc:`LibGHDLException` as a ``__cause__``.

    :raises DOMException: If an error occurred in libghdl.
    """
    errorCount = errorout_memory.Get_Nbr_Messages()
    if errorCount != 0:
        errors = []
        for i in range(errorCount):
            rec = errorout_memory.Get_Error_Record(i + 1)
            # FIXME: needs help from @tgingold
            fileName = "????"  # name_table.Get_Name_Ptr(files_map.Get_File_Name(rec.file))
            message = errorout_memory.Get_Error_Message(i + 1)

            errors.append(f"{fileName}:{rec.line}:{rec.offset}: {message}")

        errorout_memory.Clear_Errors()

        raise DOMException("Error raised in libghdl.") from LibGHDLException("libghdl: Internal error.", errors)


@export
def GetIirKindOfNode(node: Iir) -> nodes.Iir_Kind:
    """Return the kind of a node in the IIR tree.

    :returns:           The IIR kind of a node.
    :raises ValueError: If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    """
    if node == Null_Iir:
        raise ValueError("GetIirKindOfNode: Parameter 'node' must not be 'Null_Iir'.")

    kind: int = nodes.Get_Kind(node)
    return nodes.Iir_Kind(kind)


@export
def GetNameOfNode(node: Iir) -> str:
    """Return the Python string from node ``node`` identifier.

    :raises ValueError: If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    """
    if node == Null_Iir:
        raise ValueError("GetNameOfNode: Parameter 'node' must not be 'Null_Iir'.")

    identifier = utils.Get_Source_Identifier(node)
    return name_table.Get_Name_Ptr(identifier)


@export
def GetDocumentationOfNode(node: Iir) -> str:
    file = files_map.Location_To_File(nodes.Get_Location(node))
    idx = file_comments.Find_First_Comment(file, node)
    documentation = []
    while idx != file_comments.No_Comment_Index:
        documentation.append(file_comments.Get_Comment(file, idx))
        idx = file_comments.Get_Next_Comment(file, idx)

    return "\n".join(documentation)


@export
def GetModeOfNode(node: Iir) -> Mode:
    """Return the mode of a ``node``.

    :raises ValueError:   If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    :raises DOMException: If mode returned by libghdl is not known by :data:`__MODE_TRANSLATION`.
    """
    if node == Null_Iir:
        raise ValueError("GetModeOfNode: Parameter 'node' must not be 'Null_Iir'.")

    try:
        return __MODE_TRANSLATION[nodes.Get_Mode(node)]
    except KeyError as ex:
        raise DOMException(f"Unknown mode '{ex.args[0]}'.") from ex
