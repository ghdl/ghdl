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
from typing import List, Union, Iterator, Tuple

from pydecor import export

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
    SubTypeOrSymbol,
)
from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, DOMException
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
from pyGHDL.dom.Symbol import SimpleSubTypeSymbol
from pyGHDL.dom.Literal import EnumerationLiteral, PhysicalIntegerLiteral
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Subprogram import Function, Procedure


@export
class EnumeratedType(VHDLModel_EnumeratedType, DOMMixin):
    def __init__(self, node: Iir, name: str, literals: List[EnumerationLiteral]):
        super().__init__(name, literals)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "EnumeratedType":
        literals = []
        enumerationLiterals = nodes.Get_Enumeration_Literal_List(typeDefinitionNode)
        for enumerationLiteral in utils.flist_iter(enumerationLiterals):
            literal = EnumerationLiteral.parse(enumerationLiteral)
            literals.append(literal)

        return cls(typeDefinitionNode, typeName, literals)


@export
class IntegerType(VHDLModel_IntegerType, DOMMixin):
    def __init__(self, node: Iir, typeName: str, range: Range):
        super().__init__(typeName)
        DOMMixin.__init__(self, node)

        self._leftBound = range.LeftBound
        self._rightBound = range.RightBound


@export
class PhysicalType(VHDLModel_PhysicalType, DOMMixin):
    def __init__(
        self,
        node: Iir,
        typeName: str,
        primaryUnit: str,
        units: List[Tuple[str, PhysicalIntegerLiteral]],
    ):
        super().__init__(typeName, primaryUnit, units)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "PhysicalType":
        primaryUnit = nodes.Get_Primary_Unit(typeDefinitionNode)
        primaryUnitName = GetNameOfNode(primaryUnit)

        units = []
        for secondaryUnit in utils.chain_iter(nodes.Get_Unit_Chain(typeDefinitionNode)):
            secondaryUnitName = GetNameOfNode(secondaryUnit)
            if secondaryUnit == primaryUnit:
                continue

            physicalLiteral = PhysicalIntegerLiteral.parse(
                nodes.Get_Physical_Literal(secondaryUnit)
            )

            units.append((secondaryUnitName, physicalLiteral))

        return cls(typeDefinitionNode, typeName, primaryUnitName, units)


@export
class ArrayType(VHDLModel_ArrayType, DOMMixin):
    def __init__(
        self, node: Iir, name: str, indices: List, elementSubType: SubTypeOrSymbol
    ):
        super().__init__(name, indices, elementSubType)
        DOMMixin.__init__(self, node)

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

        return cls(typeDefinitionNode, typeName, indices, elementSubType)


@export
class RecordTypeElement(VHDLModel_RecordTypeElement, DOMMixin):
    def __init__(self, node: Iir, name: str, subType: SubTypeOrSymbol):
        super().__init__(name, subType)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementDeclarationNode: Iir) -> "RecordTypeElement":
        from pyGHDL.dom._Translate import GetSubTypeIndicationFromNode

        elementName = GetNameOfNode(elementDeclarationNode)
        elementType = GetSubTypeIndicationFromNode(
            elementDeclarationNode, "record element", elementName
        )

        return cls(elementDeclarationNode, elementName, elementType)


@export
class RecordType(VHDLModel_RecordType, DOMMixin):
    def __init__(self, node: Iir, name: str, elements: List[RecordTypeElement] = None):
        super().__init__(name, elements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "RecordType":
        elements = []
        elementDeclarations = nodes.Get_Elements_Declaration_List(typeDefinitionNode)
        for elementDeclaration in utils.flist_iter(elementDeclarations):
            element = RecordTypeElement.parse(elementDeclaration)
            elements.append(element)

        return cls(typeDefinitionNode, typeName, elements)


@export
class ProtectedType(VHDLModel_ProtectedType, DOMMixin):
    def __init__(self, node: Iir, name: str, methods: Union[List, Iterator] = None):
        super().__init__(name, methods)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ProtectedType":
        # FIXME: change this to a generator
        methods = []
        for item in utils.chain_iter(nodes.Get_Declaration_Chain(typeDefinitionNode)):
            kind = GetIirKindOfNode(item)
            if kind == nodes.Iir_Kind.Function_Declaration:
                methods.append(Function.parse(item))
            elif kind == nodes.Iir_Kind.Procedure_Declaration:
                methods.append(Procedure.parse(item))

        return cls(typeDefinitionNode, typeName, methods)


@export
class ProtectedTypeBody(VHDLModel_ProtectedTypeBody, DOMMixin):
    def __init__(
        self, node: Iir, name: str, declaredItems: Union[List, Iterator] = None
    ):
        super().__init__(name, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, protectedBodyNode: Iir) -> "ProtectedTypeBody":
        from pyGHDL.dom._Translate import GetDeclaredItemsFromChainedNodes

        typeName = GetNameOfNode(protectedBodyNode)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(protectedBodyNode),
            "protected type body",
            typeName,
        )

        return cls(protectedBodyNode, typeName, declaredItems)


@export
class AccessType(VHDLModel_AccessType, DOMMixin):
    def __init__(self, node: Iir, name: str, designatedSubType: SubTypeOrSymbol):
        super().__init__(name, designatedSubType)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "AccessType":
        from pyGHDL.dom._Translate import GetSubTypeIndicationFromIndicationNode

        designatedSubtypeIndication = nodes.Get_Designated_Subtype_Indication(
            typeDefinitionNode
        )
        designatedSubType = GetSubTypeIndicationFromIndicationNode(
            designatedSubtypeIndication, "access type", typeName
        )

        return cls(typeDefinitionNode, typeName, designatedSubType)


@export
class FileType(VHDLModel_FileType, DOMMixin):
    def __init__(self, node: Iir, name: str, designatedSubType: SubTypeOrSymbol):
        super().__init__(name, designatedSubType)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "FileType":

        designatedSubTypeMark = nodes.Get_File_Type_Mark(typeDefinitionNode)
        designatedSubTypeName = GetNameOfNode(designatedSubTypeMark)
        designatedSubType = SimpleSubTypeSymbol(
            typeDefinitionNode, designatedSubTypeName
        )

        return cls(typeDefinitionNode, typeName, designatedSubType)


@export
class SubType(VHDLModel_SubType, DOMMixin):
    def __init__(self, node: Iir, subtypeName: str):
        super().__init__(subtypeName)
        DOMMixin.__init__(self, node)
