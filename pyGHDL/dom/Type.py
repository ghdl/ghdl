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
from pyTooling.Decorators import export, InheritDocString

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
from pyVHDLModel.Base import Range
from pyGHDL.dom.Subprogram import Function, Procedure


@export
@InheritDocString(VHDLModel_AnonymousType, merge=True)
class IncompleteType(VHDLModel_AnonymousType, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.AnonymousType`.
    """

    def __init__(self, node: Iir, identifier: str, documentation: str = None) -> None:
        """
        Initializes an incomplete type declaration.

        :param node:          The IIR node this object was translated from.
        :param identifier:    Name of the type.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir, documentation: str = None) -> "IncompleteType":
        """
        Translates an IIR node to an :class:`IncompleteType`.

        :param node:          The IIR node this object is translated from.
        :param documentation: The documentation comment associated with this declaration.
        :returns:             The translated object.
        """
        from pyGHDL.dom._Utils import GetNameOfNode

        name = GetNameOfNode(node)

        return cls(node, name, documentation)


@export
class EnumeratedType(VHDLModel_EnumeratedType, DOMMixin):
    """
    Represents an *enumerated type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.EnumeratedType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type integer is (lit_1, lit2, ...);
    """

    def __init__(
        self, node: Iir, identifier: str, literals: List[EnumerationLiteral], documentation: str = None
    ) -> None:
        """
        Initializes an enumerated type definition.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The enumeration type's identifier.
        :param literals:      List of all enumeration literals, in declaration order.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, literals, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "EnumeratedType":
        """
        Parses an *enumerated type* IIR and returns an :class:`~pyVHDLModel.Type.EnumeratedType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The enumerated type instance.
        """
        literals = []
        enumerationLiterals = nodes.Get_Enumeration_Literal_List(typeDefinitionNode)
        for enumerationLiteral in utils.flist_iter(enumerationLiterals):
            literal = EnumerationLiteral.parse(enumerationLiteral)
            literals.append(literal)

        return cls(typeDefinitionNode, typeName, literals, documentation)


@export
class IntegerType(VHDLModel_IntegerType, DOMMixin):
    """
    Represents an *integer type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.IntegerType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type integer is range -2147483648 to 2147483647;
    """

    def __init__(self, node: Iir, typeName: str, rng: Union[Range, "Name"], documentation: str = None) -> None:
        """
        Initializes an integer type definition.

        :param node:          The IIR node this object was translated from.
        :param typeName:      The type's identifier.
        :param rng:           The range constraining this scalar type.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(typeName, rng, documentation)
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
        documentation: str = None,
    ) -> None:
        """
        Initializes a physical type definition.

        :param node:          The IIR node this object was translated from.
        :param typeName:      The type's identifier.
        :param rng:           The range constraining this scalar type.
        :param primaryUnit:   The name of the type's primary unit.
        :param units:         Iterable of the secondary units as (name, value) pairs.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(typeName, rng, primaryUnit, units, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "PhysicalType":
        """
        Parses an *physical type* IIR and returns an :class:`~pyVHDLModel.Type.PhysicalType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The physical type instance.
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

        return cls(typeDefinitionNode, typeName, rng, primaryUnitName, units, documentation)


@export
class ArrayType(VHDLModel_ArrayType, DOMMixin):
    """
    Represents an *array type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.ArrayType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type bit_vector is array(natural range <>) of bit;
    """

    def __init__(
        self, node: Iir, identifier: str, indices: List, elementSubtype: Symbol, documentation: str = None
    ) -> None:
        """
        Initializes an array type definition.

        :param node:           The IIR node this object was translated from.
        :param identifier:     The array type's identifier.
        :param indices:        List of all index ranges, one per dimension.
        :param elementSubtype: Reference to the subtype of the array's elements.
        :param documentation:  The documentation comment associated with this declaration.
        """
        super().__init__(identifier, indices, elementSubtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "ArrayType":
        """
        Parses an *array type* IIR and returns an :class:`~pyVHDLModel.Type.ArrayType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The array type instance.
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

        return cls(typeDefinitionNode, typeName, indices, elementSubtype, documentation)


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

    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        """
        Initializes a record type element.

        :param node:          The IIR node this object was translated from.
        :param identifiers:   A list of identifiers.
        :param subtype:       Reference to the subtype shared by all identifiers of this element declaration.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementDeclarationNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "RecordTypeElement":
        """
        Parses a *record element* IIR and returns an :class:`~pyVHDLModel.Type.RecordTypeElement` instance.

        :param elementDeclarationNode: The IIR node to parse.
        :param furtherIdentifiers:     The list of record element identifiers.
        :returns:                      The record element instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        elementName = GetNameOfNode(elementDeclarationNode)
        elementType = GetSubtypeIndicationFromNode(elementDeclarationNode, "record element", elementName)
        documentation = GetDocumentationOfNode(elementDeclarationNode)

        identifiers = [elementName]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)

        return cls(elementDeclarationNode, identifiers, elementType, documentation)


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

    def __init__(
        self, node: Iir, identifier: str, elements: List[RecordTypeElement] = None, documentation: str = None
    ) -> None:
        """
        Initializes a record type definition.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The record type's identifier.
        :param elements:      List of all element declarations, in declaration order.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, elements, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "RecordType":
        """
        Parses a *record type* IIR and returns an :class:`~pyVHDLModel.Type.RecordType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The record type instance.
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

        return cls(typeDefinitionNode, typeName, elements, documentation)


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

    def __init__(
        self, node: Iir, identifier: str, methods: Union[List, Iterator] = None, documentation: str = None
    ) -> None:
        """
        Initializes a protected type declaration.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The protected type's identifier.
        :param methods:       List of the protected type's methods, in declaration order.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, methods, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "ProtectedType":
        """
        Parses a *protected type* IIR and returns an :class:`~pyVHDLModel.Type.ProtectedType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The protected type instance.
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

        return cls(typeDefinitionNode, typeName, methods, documentation)


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

    def __init__(
        self, node: Iir, identifier: str, declaredItems: Union[List, Iterator] = None, documentation: str = None
    ) -> None:
        """
        Initializes a protected type body.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The protected type body's identifier.
        :param declaredItems: Iterable of all items declared in this body.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, declaredItems, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, protectedBodyNode: Iir) -> "ProtectedTypeBody":
        """
        Parses a *protected type body* IIR and returns an :class:`~pyVHDLModel.Type.ProtectedTypeBody` instance.

        :param protectedBodyNode: The IIR node to parse.
        :returns:                 The protected type body instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode
        from pyGHDL.dom._Translate import GetDeclaredItemsFromChainedNodes

        typeName = GetNameOfNode(protectedBodyNode)
        documentation = GetDocumentationOfNode(protectedBodyNode)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(protectedBodyNode),
            "protected type body",
            typeName,
        )

        return cls(protectedBodyNode, typeName, declaredItems, documentation)


@export
class AccessType(VHDLModel_AccessType, DOMMixin):
    """
    Represents an *access type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.AccessType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type line is access string;
    """

    def __init__(self, node: Iir, identifier: str, designatedSubtype: Symbol, documentation: str = None) -> None:
        """
        Initializes an access type definition.

        :param node:              The IIR node this object was translated from.
        :param identifier:        The access type's identifier.
        :param designatedSubtype: Reference to the subtype the access values designate.
        :param documentation:     The documentation comment associated with this declaration.
        """
        super().__init__(identifier, designatedSubtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "AccessType":
        """
        Parses an *access type* IIR and returns an :class:`~pyVHDLModel.Type.AccessType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The access type instance.
        """
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromIndicationNode

        designatedSubtypeIndication = nodes.Get_Designated_Subtype_Indication(typeDefinitionNode)
        designatedSubtype = GetSubtypeIndicationFromIndicationNode(designatedSubtypeIndication, "access type", typeName)

        return cls(typeDefinitionNode, typeName, designatedSubtype, documentation)


@export
class FileType(VHDLModel_FileType, DOMMixin):
    """
    Represents a *file type*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.FileType`.

    .. admonition:: Example

       .. code-block:: VHDL

          type text is file of string;
    """

    def __init__(self, node: Iir, identifier: str, designatedSubtype: Symbol, documentation: str = None) -> None:
        """
        Initializes a file type definition.

        :param node:              The IIR node this object was translated from.
        :param identifier:        The file type's identifier.
        :param designatedSubtype: Reference to the subtype of the values stored in the file.
        :param documentation:     The documentation comment associated with this declaration.
        """
        super().__init__(identifier, designatedSubtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, typeName: str, typeDefinitionNode: Iir, documentation: str = None) -> "FileType":
        """
        Parses a *file type* IIR and returns an :class:`~pyVHDLModel.Type.FileType` instance.

        :param typeName:           The identifier of the type.
        :param typeDefinitionNode: The IIR node to parse.
        :param documentation:      The documentation comment on the type declaration.
        :returns:                  The file type instance.
        """
        from pyGHDL.dom._Utils import GetNameOfNode

        designatedSubtypeMark = nodes.Get_File_Type_Mark(typeDefinitionNode)
        designatedSubtypeName = GetNameOfNode(designatedSubtypeMark)
        designatedSubtype = SimpleSubtypeSymbol(
            typeDefinitionNode, SimpleName(designatedSubtypeMark, designatedSubtypeName)
        )

        return cls(typeDefinitionNode, typeName, designatedSubtype, documentation)


@export
class Subtype(VHDLModel_Subtype, DOMMixin):
    """
    Represents a *subtype*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Type.Subtype`.
    """

    def __init__(self, node: Iir, subtypeName: str, symbol: Symbol, documentation: str = None) -> None:
        """
        Initializes a subtype declaration.

        :param node:          The IIR node this object was translated from.
        :param subtypeName:   The subtype's identifier.
        :param symbol:        Reference to the type or subtype this subtype is derived from.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(subtypeName, symbol, documentation)
        DOMMixin.__init__(self, node)
