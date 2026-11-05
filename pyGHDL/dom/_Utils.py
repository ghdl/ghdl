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
"""
This module offers helper functions to read an IIR node's kind, name, label, mode and documentation.
"""

from __future__ import annotations

from typing import Optional as Nullable

from pyTooling.Decorators import export

from pyVHDLModel.Base import Mode

from pyGHDL.libghdl import ENCODING, LibGHDLException, name_table, errorout_memory, files_map, file_comments
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes, nodes_meta, utils
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
    """
    Return the kind of a node in the IIR tree.

    :param node:        The IIR node to read the kind of.
    :returns:           The IIR kind of a node.
    :raises ValueError: If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    """
    if node == Null_Iir:
        raise ValueError("GetIirKindOfNode: Parameter 'node' must not be 'Null_Iir'.")

    kind: int = nodes.Get_Kind(node)
    return nodes.Iir_Kind(kind)


@export
def GetNameOfNode(node: Iir) -> str:
    """
    Return the Python string from node ``node`` identifier.

    A character literal (``'0'``) is returned as its character, without quotes (``0``).

    :param node:        The IIR node to read the identifier of.
    :returns:           The node's identifier as written in the source file.
    :raises ValueError: If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    """
    if node == Null_Iir:
        raise ValueError("GetNameOfNode: Parameter 'node' must not be 'Null_Iir'.")

    identifier = nodes.Get_Identifier(node)
    if name_table.Is_Character(identifier):
        return name_table.Get_Character(identifier)

    identifier = utils.Get_Source_Identifier(node)
    return name_table.Get_Name_Ptr(identifier)


@export
def GetLabelOfNode(node: Iir) -> Nullable[str]:
    """
    Return the Python string from node ``node``'s optional label.

    A statement carries its label in ``Label``, while a generate-statement body - the branch of an
    if-generate or case-generate statement - carries it in ``Alternative_Label``. No node kind has
    both, so the field is chosen from the node's kind.

    The label is returned as written in the source file (``U1``), read at the node's location, which is
    the label's.

    :param node:        The IIR node to read the label of.
    :returns:           The label, or ``None`` if the node has no label field or no label was given
                        in the source.
    :raises ValueError: If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    """
    kind = GetIirKindOfNode(node)
    if nodes_meta.Has_Label(kind):
        label = nodes.Get_Label(node)
    elif nodes_meta.Has_Alternative_Label(kind):
        label = nodes.Get_Alternative_Label(node)
    else:
        return None

    if label == name_table.Null_Identifier:
        return None

    normalized = name_table.Get_Name_Ptr(label)
    sourceText = GetSourceTextOfNode(node, len(normalized))
    if sourceText.lower() == normalized.lower():
        return sourceText
    else:
        return normalized


@export
def GetSourceTextOfNode(node: Iir, length: Nullable[int] = None) -> str:
    """
    Reads the source text starting at an IIR node's location.

    :param node:   The IIR node, whose location the text starts at.
    :param length: Optional, number of bytes to read. Default: ``None``, up to the end of the source file.
    :returns:      The source text, decoded with :data:`~pyGHDL.libghdl.ENCODING`.
    """
    location = nodes.Get_Location(node)
    sourceFile = files_map.Location_To_File(location)
    start = files_map.Location_File_To_Pos(location, sourceFile)
    end = files_map.Get_File_Length(sourceFile) if length is None else start + length

    return files_map.Get_File_Buffer(sourceFile)[start:end].decode(ENCODING)


@export
def GetDocumentationOfNode(node: Iir) -> Nullable[str]:
    """
    Collects the doc-comment lines attached to an IIR node.

    *libghdl* only gathers comments if :data:`~pyGHDL.libghdl.flags.Flag_Gather_Comments` was enabled
    before parsing, which :class:`~pyGHDL.dom.NonStandard.Design` does.

    :param node: The IIR node to read the comments of.
    :returns:    The comment lines joined by newlines, or ``None`` if the node has no comment.
    """
    file = files_map.Location_To_File(nodes.Get_Location(node))
    idx = file_comments.Find_First_Comment(file, node)
    documentation = []
    while idx != file_comments.No_Comment_Index:
        documentation.append(file_comments.Get_Comment(file, idx))
        idx = file_comments.Get_Next_Comment(file, idx)

    if len(documentation) == 0:
        return None

    return "\n".join(documentation)


@export
def GetDocumentationOfDesignUnit(unitNode: Iir) -> Nullable[str]:
    """
    Collects the doc-comment lines of a design unit (entity, architecture, package, ...).

    In IIR, a design unit is a ``Design_Unit`` node holding the context clause and the unit's declaration (the
    ``Library_Unit``), as in IEEE Std 1076, 13.1. A design unit's documentation consists of the comments *libghdl*
    attached to the ``Design_Unit`` node (the comments before the unit, except the file header), followed by the
    comments attached to the declaration node itself. A nested package has no ``Design_Unit`` node, so only its own
    comments are read.

    :param unitNode: The IIR node of the unit's declaration (entity declaration, architecture body, ...).
    :returns:        The comment lines joined by newlines, or ``None`` if the unit has no comment.
    """
    documentation = GetDocumentationOfNode(unitNode)

    parent = nodes.Get_Parent(unitNode)
    if GetIirKindOfNode(parent) is nodes.Iir_Kind.Design_Unit:
        unitDocumentation = GetDocumentationOfNode(parent)
        if unitDocumentation is not None:
            documentation = unitDocumentation if documentation is None else f"{unitDocumentation}\n{documentation}"

    return documentation


@export
def GetModeOfNode(node: Iir) -> Mode:
    """
    Return the mode of a ``node``.

    If the source text gives no mode (``generic (G : natural)``), the mode is :attr:`~pyVHDLModel.Base.Mode.Default`.

    :param node:          The IIR node to read the mode of.
    :returns:             The node's mode as a pyVHDLModel enumeration value.
    :raises ValueError:   If parameter ``node`` is :data:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`.
    :raises DOMException: If mode returned by libghdl is not known by :data:`__MODE_TRANSLATION`.
    """
    if node == Null_Iir:
        raise ValueError("GetModeOfNode: Parameter 'node' must not be 'Null_Iir'.")

    if nodes_meta.Has_Has_Mode(GetIirKindOfNode(node)) and not nodes.Get_Has_Mode(node):
        return Mode.Default

    try:
        return __MODE_TRANSLATION[nodes.Get_Mode(node)]
    except KeyError as ex:
        raise DOMException(f"Unknown mode '{ex.args[0]}'.") from ex
