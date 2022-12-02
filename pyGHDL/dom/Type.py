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

from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    AnonymousType as VHDLModel_AnonymousType,
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
    Subtype as VHDLModel_Subtype,
    SubtypeOrSymbol,
    Name,
)
from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes, flists
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol
from pyGHDL.dom.Literal import EnumerationLiteral, PhysicalIntegerLiteral
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Subprogram import Function, Procedure


@export
class IncompleteType(VHDLModel_AnonymousType, DOMMixin):
    def __init__(self, node: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "IncompleteType":
        from pyGHDL.dom._Utils import GetNameOfNode

        name = GetNameOfNode(node)

        return cls(node, name)


@export
class EnumeratedType(VHDLModel_EnumeratedType, DOMMixin):
    def __init__(self, node: Iir, identifier: str, literals: List[EnumerationLiteral]):
        super().__init__(identifier, literals)
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
    def __init__(self, node: Iir, typeName: str, rng: Union[Range, "Name"]):
        super().__init__(typeName, rng)
        DOMMixin.__init__(self, node)


@export
class PhysicalType(VHDLModel_PhysicalType, DOMMixin):
    def __init__(
        self,
        node: Iir,
        typeName: str,
        rng: Union[Range, Name],
        primaryUnit: str,
        units: List[Tuple[str, PhysicalIntegerLiteral]],
    ):
        super().__init__(typeName, rng, primaryUnit, units)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "PhysicalType":
        from pyGHDL.dom._Utils import GetIirKindOfNode, GetNameOfNode
        from pyGHDL.dom._Translate import GetRangeFromNode, GetNameFromNode

        rangeConstraint = nodes.Get_Range_Constraint(typeDefinitionNode)
        rangeKind = GetIirKindOfNode(rangeConstraint)
        if rangeKind == nodes.Iir_Kind.Range_Expression:
            rng = GetRangeFromNode(rangeConstraint)
        elif rangeKind in (
            nodes.Iir_Kind.Attribute_Name,
            nodes.Iir_Kind.Parenthesis_Name,
        ):
            rng = GetNameFromNode(rangeConstraint)
        else:
            pos = Position.parse(typeDefinitionNode)
            raise DOMException(f"Unknown range kind '{rangeKind.name}' in physical type definition at line {pos.Line}.")

        primaryUnit = nodes.Get_Primary_Unit(typeDefinitionNode)
        primaryUnitName = GetNameOfNode(primaryUnit)

        units = []
        for secondaryUnit in utils.chain_iter(nodes.Get_Unit_Chain(typeDefinitionNode)):
            secondaryUnitName = GetNameOfNode(secondaryUnit)
            if secondaryUnit == primaryUnit:
                continue

            physicalLiteral = PhysicalIntegerLiteral.parse(nodes.Get_Physical_Literal(secondaryUnit))

            units.append((secondaryUnitName, physicalLiteral))

        return cls(typeDefinitionNode, typeName, rng, primaryUnitName, units)


@export
class ArrayType(VHDLModel_ArrayType, DOMMixin):
    def __init__(self, node: Iir, identifier: str, indices: List, elementSubtype: SubtypeOrSymbol):
        super().__init__(identifier, indices, elementSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ArrayType":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetSimpleTypeFromNode,
            GetSubtypeIndicationFromIndicationNode,
        )

        indices = []
        indexDefinitions = nodes.Get_Index_Subtype_Definition_List(typeDefinitionNode)
        for index in utils.flist_iter(indexDefinitions):
            indexKind = GetIirKindOfNode(index)
            if indexKind == nodes.Iir_Kind.Simple_Name:
                indexSubtype = GetSimpleTypeFromNode(index)
                indices.append(indexSubtype)
            else:
                raise DOMException(
                    f"Unknown kind '{indexKind.name}' for an index in the array definition of `{typeName}`."
                )

        elementSubtypeIndication = nodes.Get_Element_Subtype_Indication(typeDefinitionNode)
        elementSubtype = GetSubtypeIndicationFromIndicationNode(elementSubtypeIndication, "array declaration", typeName)

        return cls(typeDefinitionNode, typeName, indices, elementSubtype)


@export
class RecordTypeElement(VHDLModel_RecordTypeElement, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: SubtypeOrSymbol):
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementDeclarationNode: Iir) -> "RecordTypeElement":
        from pyGHDL.dom._Utils import GetNameOfNode
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        elementName = GetNameOfNode(elementDeclarationNode)
        elementType = GetSubtypeIndicationFromNode(elementDeclarationNode, "record element", elementName)

        return cls(
            elementDeclarationNode,
            [
                elementName,
            ],
            elementType,
        )


@export
class RecordType(VHDLModel_RecordType, DOMMixin):
    def __init__(self, node: Iir, identifier: str, elements: List[RecordTypeElement] = None):
        super().__init__(identifier, elements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "RecordType":
        from pyGHDL.dom._Utils import GetNameOfNode

        elements = []
        elementDeclarations = nodes.Get_Elements_Declaration_List(typeDefinitionNode)

        elementCount = flists.Flast(elementDeclarations) + 1
        index = 0
        while index < elementCount:
            elementDeclaration = flists.Get_Nth_Element(elementDeclarations, index)

            element = RecordTypeElement.parse(elementDeclaration)

            # Lookahead for elements with multiple identifiers at once
            if nodes.Get_Has_Identifier_List(elementDeclaration):
                index += 1
                while index < elementCount:
                    nextNode: Iir = flists.Get_Nth_Element(elementDeclarations, index)
                    # Consecutive identifiers are found, if the subtype indication is Null
                    if nodes.Get_Subtype_Indication(nextNode) == nodes.Null_Iir:
                        element.Identifiers.append(GetNameOfNode(nextNode))
                    else:
                        break
                    index += 1

                    # The last consecutive identifiers has no Identifier_List flag
                    if not nodes.Get_Has_Identifier_List(nextNode):
                        break
            else:
                index += 1

            elements.append(element)

        return cls(typeDefinitionNode, typeName, elements)


@export
class ProtectedType(VHDLModel_ProtectedType, DOMMixin):
    def __init__(self, node: Iir, identifier: str, methods: Union[List, Iterator] = None):
        super().__init__(identifier, methods)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ProtectedType":
        from pyGHDL.dom._Utils import GetIirKindOfNode

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
    def __init__(self, node: Iir, identifier: str, declaredItems: Union[List, Iterator] = None):
        super().__init__(identifier, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, protectedBodyNode: Iir) -> "ProtectedTypeBody":
        from pyGHDL.dom._Utils import GetNameOfNode
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
    def __init__(self, node: Iir, identifier: str, designatedSubtype: SubtypeOrSymbol):
        super().__init__(identifier, designatedSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "AccessType":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromIndicationNode

        designatedSubtypeIndication = nodes.Get_Designated_Subtype_Indication(typeDefinitionNode)
        designatedSubtype = GetSubtypeIndicationFromIndicationNode(designatedSubtypeIndication, "access type", typeName)

        return cls(typeDefinitionNode, typeName, designatedSubtype)


@export
class FileType(VHDLModel_FileType, DOMMixin):
    def __init__(self, node: Iir, identifier: str, designatedSubtype: SubtypeOrSymbol):
        super().__init__(identifier, designatedSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "FileType":
        from pyGHDL.dom._Utils import GetNameOfNode

        designatedSubtypeMark = nodes.Get_File_Type_Mark(typeDefinitionNode)
        designatedSubtypeName = GetNameOfNode(designatedSubtypeMark)
        designatedSubtype = SimpleSubtypeSymbol(typeDefinitionNode, designatedSubtypeName)

        return cls(typeDefinitionNode, typeName, designatedSubtype)


@export
class Subtype(VHDLModel_Subtype, DOMMixin):
    def __init__(self, node: Iir, subtypeName: str):
        super().__init__(subtypeName)
        DOMMixin.__init__(self, node)
