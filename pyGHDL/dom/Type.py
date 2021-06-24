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
from typing import List

from pydecor import export

from pyGHDL.dom.Symbol import SimpleSubTypeSymbol
from pyVHDLModel.VHDLModel import (
    PhysicalType as VHDLModel_PhysicalType,
    IntegerType as VHDLModel_IntegerType,
    EnumeratedType as VHDLModel_EnumeratedType,
    ArrayType as VHDLModel_ArrayType,
    RecordTypeElement as VHDLModel_RecordTypeElement,
    RecordType as VHDLModel_RecordType,
    AccessType as VHDLModel_AccessType,
    FileType as VHDLModel_FileType,
    ProtectedType as VHDLModel_ProtectedType,
    ProtectedTypeBody as VHDLModel_ProtectedTypeBody,
    SubType as VHDLModel_SubType,
)
from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
from pyGHDL.dom.Common import DOMException
from pyGHDL.dom.Literal import EnumerationLiteral
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Subprogram import Function, Procedure


@export
class IntegerType(VHDLModel_IntegerType):
    def __init__(self, typeName: str, range: Range):
        super().__init__(typeName)
        self._leftBound = range.LeftBound
        self._rightBound = range.RightBound


@export
class PhysicalType(VHDLModel_PhysicalType):
    def __init__(self, typeName: str, units: List):
        super().__init__(typeName)


@export
class EnumeratedType(VHDLModel_EnumeratedType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "EnumeratedType":
        literals = []
        enumerationLiterals = nodes.Get_Enumeration_Literal_List(typeDefinitionNode)
        for enumerationLiteral in utils.flist_iter(enumerationLiterals):
            literal = EnumerationLiteral.parse(enumerationLiteral)
            literals.append(literal)

        return cls(typeName, literals)


@export
class ArrayType(VHDLModel_ArrayType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ArrayType":
        from pyGHDL.dom._Translate import (
            GetSimpleTypeFromNode,
            GetSubTypeIndicationFromIndicationNode,
        )

        indices = []
        indexDefinitions = nodes.Get_Index_Subtype_Definition_List(typeDefinitionNode)
        for index in utils.flist_iter(indexDefinitions):
            indexKind = GetIirKindOfNode(index)
            if indexKind == nodes.Iir_Kind.Simple_Name:
                indexSubType = GetSimpleTypeFromNode(index)
                indices.append(indexSubType)
            else:
                raise DOMException(
                    "Unknown kind '{kind}' for an index in the array definition of `{typeName}`.".format(
                        kind=indexKind.name, typeName=typeName
                    )
                )

        elementSubTypeIndication = nodes.Get_Element_Subtype_Indication(
            typeDefinitionNode
        )
        elementSubType = GetSubTypeIndicationFromIndicationNode(
            elementSubTypeIndication, "array declaration", typeName
        )

        return cls(typeName, indices, elementSubType)


@export
class RecordTypeElement(VHDLModel_RecordTypeElement):
    @classmethod
    def parse(cls, elementDeclarationNode: Iir) -> "RecordTypeElement":
        from pyGHDL.dom._Translate import GetSubTypeIndicationFromNode

        elementName = GetNameOfNode(elementDeclarationNode)
        elementType = GetSubTypeIndicationFromNode(
            elementDeclarationNode, "record element", elementName
        )

        return cls(elementName, elementType)


@export
class RecordType(VHDLModel_RecordType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "RecordType":
        elements = []
        elementDeclarations = nodes.Get_Elements_Declaration_List(typeDefinitionNode)
        for elementDeclaration in utils.flist_iter(elementDeclarations):
            element = RecordTypeElement.parse(elementDeclaration)
            elements.append(element)

        return cls(typeName, elements)


@export
class AccessType(VHDLModel_AccessType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "AccessType":
        from pyGHDL.dom._Translate import GetSubTypeIndicationFromIndicationNode

        designatedSubtypeIndication = nodes.Get_Designated_Subtype_Indication(
            typeDefinitionNode
        )
        designatedSubType = GetSubTypeIndicationFromIndicationNode(
            designatedSubtypeIndication, "access type", typeName
        )

        return cls(typeName, designatedSubType)


@export
class FileType(VHDLModel_FileType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "AccessType":

        designatedSubTypeMark = nodes.Get_File_Type_Mark(typeDefinitionNode)
        designatedSubTypeName = GetNameOfNode(designatedSubTypeMark)
        designatedSubType = SimpleSubTypeSymbol(designatedSubTypeName)

        return cls(typeName, designatedSubType)


@export
class ProtectedType(VHDLModel_ProtectedType):
    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ProtectedType":
        from pyGHDL.dom._Translate import GetSubTypeIndicationFromIndicationNode

        # FIXME: change this to a generator
        methods = []
        for item in utils.chain_iter(nodes.Get_Declaration_Chain(typeDefinitionNode)):
            kind = GetIirKindOfNode(item)
            if kind == nodes.Iir_Kind.Function_Declaration:
                methods.append(Function.parse(item))
            elif kind == nodes.Iir_Kind.Procedure_Declaration:
                methods.append(Procedure.parse(item))

        return cls(typeName, methods)


@export
class ProtectedTypeBody(VHDLModel_ProtectedTypeBody):
    @classmethod
    def parse(cls, node: Iir) -> "ProtectedTypeBody":
        from pyGHDL.dom._Translate import GetDeclaredItemsFromChainedNodes

        typeName = GetNameOfNode(node)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(node), "protected type body", typeName
        )

        return cls(typeName, declaredItems)


@export
class SubType(VHDLModel_SubType):
    def __init__(self, subtypeName: str):
        super().__init__(subtypeName)
