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
This module implements derived type classes from :mod:`pyVHDLModel.Type`.
"""
from typing import List, Union, Iterator, Tuple, Iterable

from pyGHDL.dom.Name import SimpleName
from pyTooling.Decorators import export

from pyVHDLModel.Name import Name
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Type import Subtype as VHDLModel_Subtype
from pyVHDLModel.Type import AnonymousType as VHDLModel_AnonymousType
from pyVHDLModel.Type import EnumeratedType as VHDLModel_EnumeratedType
from pyVHDLModel.Type import IntegerType as VHDLModel_IntegerType
from pyVHDLModel.Type import PhysicalType as VHDLModel_PhysicalType
from pyVHDLModel.Type import ArrayType as VHDLModel_ArrayType
from pyVHDLModel.Type import RecordTypeElement as VHDLModel_RecordTypeElement
from pyVHDLModel.Type import RecordType as VHDLModel_RecordType
from pyVHDLModel.Type import ProtectedType as VHDLModel_ProtectedType
from pyVHDLModel.Type import ProtectedTypeBody as VHDLModel_ProtectedTypeBody
from pyVHDLModel.Type import AccessType as VHDLModel_AccessType
from pyVHDLModel.Type import FileType as VHDLModel_FileType

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
    def __init__(self, node: Iir, identifier: str) -> None:
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "IncompleteType":
        from pyGHDL.dom._Utils import GetNameOfNode

        name = GetNameOfNode(node)

        return cls(node, name)


@export
class EnumeratedType(VHDLModel_EnumeratedType, DOMMixin):
    """
    Represents an *enumerated type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.EnumeratedType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type integer is (lit_1, lit2, ...);
    """

    def __init__(self, node: Iir, identifier: str, literals: List[EnumerationLiteral]) -> None:
        super().__init__(identifier, literals)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "EnumeratedType":
        """
        Parses an *enumerated type* IIR and returns an :class:`~pyVHDLModel.Type.EnumeratedType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The enumerated type instance.
        """
        literals = []
        enumerationLiterals = nodes.Get_Enumeration_Literal_List(typeDefinitionNode)
        for enumerationLiteral in utils.flist_iter(enumerationLiterals):
            literal = EnumerationLiteral.parse(enumerationLiteral)
            literals.append(literal)

        return cls(typeDefinitionNode, typeName, literals)


@export
class IntegerType(VHDLModel_IntegerType, DOMMixin):
    """
    Represents an *integer type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.IntegerType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type integer is range -2147483648 to 2147483647;
    """

    def __init__(self, node: Iir, typeName: str, rng: Union[Range, "Name"]) -> None:
        super().__init__(typeName, rng)
        DOMMixin.__init__(self, node)


@export
class PhysicalType(VHDLModel_PhysicalType, DOMMixin):
    """
    Represents a *physical type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.PhysicalType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type time is range integer'low to integer'high units
            fs;
            ps = 1000 fs;
            -- ...
          end units;
    """

    def __init__(
        self,
        node: Iir,
        typeName: str,
        rng: Union[Range, Name],
        primaryUnit: str,
        units: List[Tuple[str, PhysicalIntegerLiteral]],
    ) -> None:
        super().__init__(typeName, rng, primaryUnit, units)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "PhysicalType":
        """
        Parses an *physical type* IIR and returns an :class:`~pyVHDLModel.Type.PhysicalType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The physical type instance.
        """
        from pyGHDL.dom._Utils import GetIirKindOfNode, GetNameOfNode
        from pyGHDL.dom._Translate import GetRangeFromNode, GetName

        rangeConstraint = nodes.Get_Range_Constraint(typeDefinitionNode)
        rangeKind = GetIirKindOfNode(rangeConstraint)
        if rangeKind == nodes.Iir_Kind.Range_Expression:
            rng = GetRangeFromNode(rangeConstraint)
        elif rangeKind in (
            nodes.Iir_Kind.Attribute_Name,
            nodes.Iir_Kind.Parenthesis_Name,
        ):
            rng = GetName(rangeConstraint)
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
    """
    Represents an *array type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.ArrayType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type bit_vector is array(natural range <>) of bit;
    """

    def __init__(self, node: Iir, identifier: str, indices: List, elementSubtype: Symbol) -> None:
        super().__init__(identifier, indices, elementSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ArrayType":
        """
        Parses an *array type* IIR and returns an :class:`~pyVHDLModel.Type.ArrayType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The array type instance.
        """
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
    """
    Represents a *record element*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.RecordTypeElement`.

    .. admonition:: Example

       .. code-block:: VHDL

          -- type pt is record
            element : std_logic;
            -- ...
          -- end record;
    """

    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol) -> None:
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementDeclarationNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "RecordTypeElement":
        """
        Parses a *record element* IIR and returns an :class:`~pyVHDLModel.Type.RecordTypeElement` instance.

        :param elementDeclarationNode: The IIR node to parse.
        :param furtherIdentifiers:     The list of record element identifiers.
        :return:                       The record element instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        elementName = GetNameOfNode(elementDeclarationNode)
        elementType = GetSubtypeIndicationFromNode(elementDeclarationNode, "record element", elementName)

        identifiers = [elementName]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)

        return cls(elementDeclarationNode, identifiers, elementType)


@export
class RecordType(VHDLModel_RecordType, DOMMixin):
    """
    Represents a *record type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.RecordType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type pt is record
            -- elements
          end record;
    """

    def __init__(self, node: Iir, identifier: str, elements: List[RecordTypeElement] = None) -> None:
        super().__init__(identifier, elements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "RecordType":
        """
        Parses a *record type* IIR and returns an :class:`~pyVHDLModel.Type.RecordType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The record type instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode

        elements = []
        elementDeclarations = nodes.Get_Elements_Declaration_List(typeDefinitionNode)

        furtherIdentifiers = []
        elementCount = flists.Flast(elementDeclarations) + 1
        index = 0
        while index < elementCount:
            elementDeclaration = flists.Get_Nth_Element(elementDeclarations, index)

            # Lookahead for elements with multiple identifiers at once
            if nodes.Get_Has_Identifier_List(elementDeclaration):
                index += 1
                while index < elementCount:
                    nextNode: Iir = flists.Get_Nth_Element(elementDeclarations, index)
                    # Consecutive identifiers are found, if the subtype indication is Null
                    if nodes.Get_Subtype_Indication(nextNode) == nodes.Null_Iir:
                        furtherIdentifiers.append(GetNameOfNode(nextNode))
                    else:
                        break
                    index += 1

                    # The last consecutive identifiers has no Identifier_List flag
                    if not nodes.Get_Has_Identifier_List(nextNode):
                        break
            else:
                index += 1

            element = RecordTypeElement.parse(elementDeclaration, furtherIdentifiers)
            elements.append(element)
            furtherIdentifiers.clear()

        return cls(typeDefinitionNode, typeName, elements)


@export
class ProtectedType(VHDLModel_ProtectedType, DOMMixin):
    """
    Represents a *protected type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.ProtectedType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type pt is protected
            -- public interface
          end protected;
    """

    def __init__(self, node: Iir, identifier: str, methods: Union[List, Iterator] = None) -> None:
        super().__init__(identifier, methods)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "ProtectedType":
        """
        Parses a *protected type* IIR and returns an :class:`~pyVHDLModel.Type.ProtectedType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The protected type instance.
        """
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
    """
    Represents a *protected type body*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.ProtectedTypeBody`.

    .. admonition:: Example

       .. code-block:: VHDL

          type pt is protected body
            -- implementations
          end protected body;
    """

    def __init__(self, node: Iir, identifier: str, declaredItems: Union[List, Iterator] = None) -> None:
        super().__init__(identifier, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, protectedBodyNode: Iir) -> "ProtectedTypeBody":
        """
        Parses a *protected type body* IIR and returns an :class:`~pyVHDLModel.Type.ProtectedTypeBody` instance.

        :param protectedBodyNode: The IIR node to parse.
        :return:                  The protected type body instance.
        """
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
    """
    Represents an *access type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.AccessType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type line is access string;
    """

    def __init__(self, node: Iir, identifier: str, designatedSubtype: Symbol) -> None:
        super().__init__(identifier, designatedSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "AccessType":
        """
        Parses an *access type* IIR and returns an :class:`~pyVHDLModel.Type.AccessType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The access type instance.
        """
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromIndicationNode

        designatedSubtypeIndication = nodes.Get_Designated_Subtype_Indication(typeDefinitionNode)
        designatedSubtype = GetSubtypeIndicationFromIndicationNode(designatedSubtypeIndication, "access type", typeName)

        return cls(typeDefinitionNode, typeName, designatedSubtype)


@export
class FileType(VHDLModel_FileType, DOMMixin):
    """
    Represents a *file type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.FileType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type text is file of string;
    """

    def __init__(self, node: Iir, identifier: str, designatedSubtype: Symbol) -> None:
        super().__init__(identifier, designatedSubtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir) -> "FileType":
        """
        Parses a *file type* IIR and returns an :class:`~pyVHDLModel.Type.FileType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :return:                   The file type instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode

        designatedSubtypeMark = nodes.Get_File_Type_Mark(typeDefinitionNode)
        designatedSubtypeName = GetNameOfNode(designatedSubtypeMark)
        designatedSubtype = SimpleSubtypeSymbol(
            typeDefinitionNode, SimpleName(designatedSubtypeMark, designatedSubtypeName)
        )

        return cls(typeDefinitionNode, typeName, designatedSubtype)


@export
class Subtype(VHDLModel_Subtype, DOMMixin):
    """
    Represents a *subtype*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.Subtype`.
    """

    def __init__(self, node: Iir, subtypeName: str, symbol: Symbol) -> None:
        super().__init__(subtypeName, symbol)
        DOMMixin.__init__(self, node)
