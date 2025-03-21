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
# Package module:   DOM: IIR to *** translations.
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
This module offers helper functions to translate often used IIR substructures to pyGHDL.dom (pyVHDLModel) constructs.
"""

from typing import List, Generator, Type, Dict

from pyTooling.Decorators import export
from pyTooling.Warning import WarningCollector

from pyVHDLModel.Base import ModelEntity, Direction, ExpressionUnion
from pyVHDLModel.Name import Name
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Association import AssociationItem
from pyVHDLModel.Interface import GenericInterfaceItemMixin, PortInterfaceItemMixin, ParameterInterfaceItemMixin
from pyVHDLModel.Type import BaseType
from pyVHDLModel.Sequential import SequentialStatement
from pyVHDLModel.Concurrent import ConcurrentStatement

from pyGHDL.libghdl import utils, name_table
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom.Sequential import (
    IfStatement,
    ForLoopStatement,
    CaseStatement,
    SequentialReportStatement,
    SequentialAssertStatement,
    WaitStatement,
    SequentialSimpleSignalAssignment,
    NullStatement,
    ExitStatement,
    SequentialProcedureCall,
)

from pyGHDL.dom import Position, DOMException
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
from pyGHDL.dom.Name import (
    SimpleName,
    SelectedName,
    AttributeName,
    ParenthesisName,
    AllName,
    OpenName,
)
from pyGHDL.dom.Symbol import (
    SimpleObjectOrFunctionCallSymbol,
    SimpleSubtypeSymbol,
    ConstrainedArraySubtypeSymbol,
    IndexedObjectOrFunctionCallSymbol,
    ConstrainedScalarSubtypeSymbol,
    ConstrainedRecordSubtypeSymbol,
    RecordElementSymbol,
)
from pyGHDL.dom.Type import (
    IntegerType,
    Subtype,
    ArrayType,
    RecordType,
    EnumeratedType,
    AccessType,
    ProtectedType,
    ProtectedTypeBody,
    FileType,
    PhysicalType,
    IncompleteType,
)
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Literal import (
    IntegerLiteral,
    CharacterLiteral,
    FloatingPointLiteral,
    StringLiteral,
    PhysicalIntegerLiteral,
    PhysicalFloatingLiteral,
    NullLiteral,
)
from pyGHDL.dom.Object import Variable
from pyGHDL.dom.Expression import (
    SubtractionExpression,
    AdditionExpression,
    MultiplyExpression,
    DivisionExpression,
    InverseExpression,
    ExponentiationExpression,
    Aggregate,
    NegationExpression,
    ParenthesisExpression,
    ConcatenationExpression,
    QualifiedExpression,
    ModuloExpression,
    RemainderExpression,
    AndExpression,
    NandExpression,
    OrExpression,
    NorExpression,
    XorExpression,
    XnorExpression,
    EqualExpression,
    UnequalExpression,
    LessThanExpression,
    GreaterThanExpression,
    GreaterEqualExpression,
    LessEqualExpression,
    ShiftLeftLogicExpression,
    ShiftRightLogicExpression,
    ShiftLeftArithmeticExpression,
    ShiftRightArithmeticExpression,
    RotateLeftExpression,
    RotateRightExpression,
    RangeExpression,
    QualifiedExpressionAllocation,
    SubtypeAllocation,
    IdentityExpression,
    AbsoluteExpression,
    MatchingGreaterEqualExpression,
    MatchingEqualExpression,
    MatchingUnequalExpression,
    MatchingLessThanExpression,
    MatchingLessEqualExpression,
    MatchingGreaterThanExpression,
)
from pyGHDL.dom.Concurrent import (
    ConcurrentBlockStatement,
    EntityInstantiation,
    ConfigurationInstantiation,
    ComponentInstantiation,
    ProcessStatement,
    IfGenerateStatement,
    ForGenerateStatement,
    CaseGenerateStatement,
    ConcurrentSimpleSignalAssignment,
    ConcurrentProcedureCall,
    GenericAssociationItem,
    PortAssociationItem,
    ParameterAssociationItem,
    ConcurrentAssertStatement,
)
from pyGHDL.dom.Subprogram import Function, Procedure
from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.PSL import DefaultClock


@export
def GetName(node: Iir) -> Name:
    """
    Translates a *name* IIR node to a :class:`~pyVHDLModel.Name.Name`.

    A name can be a *simple name* or complex name consisting of *selected*, *parenthesis*, *attribute*, ... *names*.
    Complex names are composed as a chain of name objects, which can be traversed using the
    :attr:`~pyVHDLModel.Name.Name.Prefix` property.

    :param node: The name IIR node to translate.
    :return:     A name object.
    """
    kind = GetIirKindOfNode(node)
    if kind == nodes.Iir_Kind.Simple_Name:
        name = GetNameOfNode(node)
        return SimpleName(node, name)
    elif kind == nodes.Iir_Kind.Selected_Name:
        name = GetNameOfNode(node)
        prefixName = GetName(nodes.Get_Prefix(node))
        return SelectedName(node, name, prefixName)
    elif kind == nodes.Iir_Kind.Parenthesis_Name:
        prefixName = GetName(nodes.Get_Prefix(node))
        associations = GetAssociations(node)
        return ParenthesisName(node, prefixName, associations)
    elif kind == nodes.Iir_Kind.Attribute_Name:
        name = GetNameOfNode(node)
        prefixName = GetName(nodes.Get_Prefix(node))
        return AttributeName(node, name, prefixName)
    elif kind == nodes.Iir_Kind.Selected_By_All_Name:
        prefixName = GetName(nodes.Get_Prefix(node))
        return AllName(node, prefixName)
    else:
        raise DOMException(f"Unknown name kind '{kind.name}'")


def GetAssociations(node: Iir) -> List:
    associations = []
    for item in utils.chain_iter(nodes.Get_Association_Chain(node)):
        kind = GetIirKindOfNode(item)

        if kind in (nodes.Iir_Kind.Association_Element_By_Expression, nodes.Iir_Kind.Association_Element_By_Name):
            actual = nodes.Get_Actual(item)
            expr = GetExpressionFromNode(actual)

            associations.append(expr)
        else:
            raise DOMException(
                f"Unknown association kind '{kind.name}' in array index/slice or function call '{node}'."
            )

    return associations


@export
def GetArrayConstraintsFromSubtypeIndication(
    subtypeIndication: Iir,
) -> List:
    constraints = []
    for constraint in utils.flist_iter(nodes.Get_Index_Constraint_List(subtypeIndication)):
        constraintKind = GetIirKindOfNode(constraint)
        if constraintKind == nodes.Iir_Kind.Range_Expression:
            constraints.append(RangeExpression.parse(constraint))
        elif constraintKind in (
            nodes.Iir_Kind.Simple_Name,
            nodes.Iir_Kind.Parenthesis_Name,
            nodes.Iir_Kind.Selected_Name,
            nodes.Iir_Kind.Attribute_Name,
        ):
            constraints.append(GetName(constraint))
        else:
            position = Position.parse(constraint)
            raise DOMException(
                f"Unknown constraint kind '{constraintKind.name}' for constraint '{constraint}' in subtype indication '{subtypeIndication}' at {position}."
            )

    return constraints


@export
def GetRecordConstraintsFromSubtypeIndication(
    subtypeIndication: Iir,
) -> Dict:
    constraints = {}
    recordElementConstraint = nodes.Get_Owned_Elements_Chain(subtypeIndication)
    while recordElementConstraint != nodes.Null_Iir:
        recordElementKind = GetIirKindOfNode(recordElementConstraint)
        if recordElementKind == nodes.Iir_Kind.Record_Element_Constraint:
            recordElementName = GetNameOfNode(recordElementConstraint)
            sym = RecordElementSymbol(recordElementConstraint, SimpleName(recordElementConstraint, recordElementName))

            constraints[sym] = None
        else:
            position = Position.parse(recordElementConstraint)
            raise DOMException(
                f"Unknown constraint kind '{recordElementKind.name}' for constraint '{recordElementConstraint}' in subtype indication '{subtypeIndication}' at {position}."
            )

        recordElementConstraint = nodes.Get_Chain(recordElementConstraint)

    return constraints


@export
def GetTypeFromNode(node: Iir) -> BaseType:
    typeName = GetNameOfNode(node)
    typeDefinition = nodes.Get_Type_Definition(node)
    if typeDefinition is nodes.Null_Iir:
        return IncompleteType(node, typeName)

    kind = GetIirKindOfNode(typeDefinition)
    if kind == nodes.Iir_Kind.Enumeration_Type_Definition:
        return EnumeratedType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Array_Type_Definition:
        return ArrayType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Record_Type_Definition:
        return RecordType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Access_Type_Definition:
        return AccessType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.File_Type_Definition:
        return FileType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Protected_Type_Declaration:
        return ProtectedType.parse(typeName, typeDefinition)
    else:
        position = Position.parse(typeDefinition)
        raise DOMException(
            f"GetTypeFromNode: Unknown type definition kind '{kind.name}' for type '{typeName}' at {position}."
        )


@export
def GetAnonymousTypeFromNode(node: Iir) -> BaseType:
    typeName = GetNameOfNode(node)
    typeDefinition = nodes.Get_Type_Definition(node)
    if typeDefinition is nodes.Null_Iir:
        return IncompleteType(node, typeName)

    kind = GetIirKindOfNode(typeDefinition)
    if kind == nodes.Iir_Kind.Range_Expression:
        r = GetRangeFromNode(typeDefinition)
        return IntegerType(node, typeName, r)

    elif kind in (nodes.Iir_Kind.Attribute_Name, nodes.Iir_Kind.Parenthesis_Name):
        n = GetName(typeDefinition)

        return IntegerType(node, typeName, n)
    elif kind == nodes.Iir_Kind.Physical_Type_Definition:
        return PhysicalType.parse(typeName, typeDefinition)

    elif kind == nodes.Iir_Kind.Array_Subtype_Definition:
        WarningCollector.Raise("[NOT IMPLEMENTED] Array_Subtype_Definition")

        return ArrayType(typeDefinition, "????", [], None)
    else:
        position = Position.parse(typeDefinition)
        raise DOMException(
            f"GetAnonymousTypeFromNode: Unknown type definition kind '{kind.name}' for type '{typeName}' at {position}."
        )


@export
def GetSubtypeIndicationFromNode(node: Iir, entity: str, name: str) -> Symbol:
    """
    Extracts the subtype indication from an IIR node and translates it to a :class:`~pyVHDLModel.Symbol.Symbol`.

    .. note::

       This function calls :func:`GetSubtypeIndicationFromIndicationNode`.

       This function should be used directly if :pycode:`nodes.Get_Subtype_Indication(...)` was already called or a
       subtype indication is return by another getter.

    :param node:   The IIR node to get the subtype indication from.
    :param entity: The entity kind the subtype indication is extracted from (e.g. ``constant``). Used in exception messages.
    :param name:   The entity's name the subtype indication is extracted from (e.g. ``BITS``). Used in exception messages.
    :returns:      A symbol representing a reference to a type/subtype and possible array or record constraints.
    """
    subtypeIndicationNode = nodes.Get_Subtype_Indication(node)
    return GetSubtypeIndicationFromIndicationNode(subtypeIndicationNode, entity, name)


@export
def GetSubtypeIndicationFromIndicationNode(subtypeIndicationNode: Iir, entity: str, name: str) -> Symbol:
    """
    Translate a subtype indication to a :class:`~pyVHDLModel.Symbol.Symbol`.

    :param subtypeIndicationNode: The subtype indication IIR node.
    :param entity: The entity kind the subtype indication is extracted from (e.g. ``constant``). Used in exception messages.
    :param name:   The entity's name the subtype indication is extracted from (e.g. ``BITS``). Used in exception messages.
    :returns:      A symbol representing a reference to a type/subtype and possible array or record constraints.
    """
    if subtypeIndicationNode is nodes.Null_Iir:
        raise ValueError("Parameter 'subtypeIndicationNode' is 'Null_Iir'.")

    kind = GetIirKindOfNode(subtypeIndicationNode)
    if kind in (
        nodes.Iir_Kind.Simple_Name,
        nodes.Iir_Kind.Selected_Name,
        nodes.Iir_Kind.Attribute_Name,
    ):
        return GetSimpleTypeFromNode(subtypeIndicationNode)
    elif kind == nodes.Iir_Kind.Subtype_Definition:
        return GetScalarConstrainedSubtypeFromNode(subtypeIndicationNode)
    elif kind == nodes.Iir_Kind.Array_Subtype_Definition:
        return GetArrayConstrainedSubtypeFromNode(subtypeIndicationNode)
    elif kind == nodes.Iir_Kind.Record_Subtype_Definition:
        return GetRecordConstrainedSubtypeFromNode(subtypeIndicationNode)
    else:
        raise DOMException(f"Unknown kind '{kind.name}' for an subtype indication in a {entity} of `{name}`.")


@export
def GetSimpleTypeFromNode(subtypeIndicationNode: Iir) -> SimpleSubtypeSymbol:
    subtypeName = GetName(subtypeIndicationNode)
    return SimpleSubtypeSymbol(subtypeIndicationNode, subtypeName)


@export
def GetScalarConstrainedSubtypeFromNode(
    subtypeIndicationNode: Iir,
) -> ConstrainedScalarSubtypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subtypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    simpleTypeMark = SimpleName(typeMark, typeMarkName)
    rangeConstraint = nodes.Get_Range_Constraint(subtypeIndicationNode)

    r = None
    # Check if RangeExpression. Might also be an AttributeName (see §3.1)
    if rangeConstraint != nodes.Null_Iir:
        if GetIirKindOfNode(rangeConstraint) == nodes.Iir_Kind.Range_Expression:
            r = GetRangeFromNode(rangeConstraint)
    # TODO: Get actual range from AttributeName node?

    return ConstrainedScalarSubtypeSymbol(subtypeIndicationNode, simpleTypeMark, r)


@export
def GetArrayConstrainedSubtypeFromNode(
    subtypeIndicationNode: Iir,
) -> ConstrainedArraySubtypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subtypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    simpleTypeMark = SimpleName(typeMark, typeMarkName)

    constraints = GetArrayConstraintsFromSubtypeIndication(subtypeIndicationNode)
    return ConstrainedArraySubtypeSymbol(subtypeIndicationNode, simpleTypeMark, constraints)


@export
def GetRecordConstrainedSubtypeFromNode(
    subtypeIndicationNode: Iir,
) -> ConstrainedRecordSubtypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subtypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    simpleTypeMark = SimpleName(typeMark, typeMarkName)

    constraints = GetRecordConstraintsFromSubtypeIndication(subtypeIndicationNode)
    return ConstrainedRecordSubtypeSymbol(subtypeIndicationNode, simpleTypeMark, constraints)


@export
def GetSubtypeFromNode(subtypeNode: Iir) -> Symbol:
    subtypeName = GetNameOfNode(subtypeNode)
    symbol = GetSubtypeIndicationFromNode(subtypeNode, "subtype", subtypeName)

    return Subtype(subtypeNode, subtypeName, symbol)


@export
def GetRangeFromNode(node: Iir) -> Range:
    """
    Translate a range IIR node to a :class:`~pyVHDLModel.Base.Range`.

    :param node: The IIR node representing a range.
    :return:     The translated range object.
    """
    direction = nodes.Get_Direction(node)
    leftBound = nodes.Get_Left_Limit_Expr(node)
    rightBound = nodes.Get_Right_Limit_Expr(node)

    return Range(
        GetExpressionFromNode(leftBound),
        GetExpressionFromNode(rightBound),
        Direction.DownTo if direction else Direction.To,
    )


__EXPRESSION_TRANSLATION = {
    nodes.Iir_Kind.Simple_Name: SimpleObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Selected_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Attribute_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Parenthesis_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Null_Literal: NullLiteral,
    nodes.Iir_Kind.Integer_Literal: IntegerLiteral,
    nodes.Iir_Kind.Floating_Point_Literal: FloatingPointLiteral,
    nodes.Iir_Kind.Physical_Int_Literal: PhysicalIntegerLiteral,
    nodes.Iir_Kind.Physical_Fp_Literal: PhysicalFloatingLiteral,
    nodes.Iir_Kind.Character_Literal: CharacterLiteral,
    nodes.Iir_Kind.String_Literal8: StringLiteral,
    nodes.Iir_Kind.Identity_Operator: IdentityExpression,
    nodes.Iir_Kind.Negation_Operator: NegationExpression,
    nodes.Iir_Kind.Absolute_Operator: AbsoluteExpression,
    nodes.Iir_Kind.Range_Expression: RangeExpression,
    nodes.Iir_Kind.Addition_Operator: AdditionExpression,
    nodes.Iir_Kind.Concatenation_Operator: ConcatenationExpression,
    nodes.Iir_Kind.Not_Operator: InverseExpression,
    nodes.Iir_Kind.Parenthesis_Expression: ParenthesisExpression,
    nodes.Iir_Kind.Substraction_Operator: SubtractionExpression,
    nodes.Iir_Kind.Multiplication_Operator: MultiplyExpression,
    nodes.Iir_Kind.Division_Operator: DivisionExpression,
    nodes.Iir_Kind.Modulus_Operator: ModuloExpression,
    nodes.Iir_Kind.Remainder_Operator: RemainderExpression,
    nodes.Iir_Kind.Exponentiation_Operator: ExponentiationExpression,
    nodes.Iir_Kind.And_Operator: AndExpression,
    nodes.Iir_Kind.Nand_Operator: NandExpression,
    nodes.Iir_Kind.Or_Operator: OrExpression,
    nodes.Iir_Kind.Nor_Operator: NorExpression,
    nodes.Iir_Kind.Xor_Operator: XorExpression,
    nodes.Iir_Kind.Xnor_Operator: XnorExpression,
    nodes.Iir_Kind.Equality_Operator: EqualExpression,
    nodes.Iir_Kind.Inequality_Operator: UnequalExpression,
    nodes.Iir_Kind.Less_Than_Operator: LessThanExpression,
    nodes.Iir_Kind.Less_Than_Or_Equal_Operator: LessEqualExpression,
    nodes.Iir_Kind.Greater_Than_Operator: GreaterThanExpression,
    nodes.Iir_Kind.Greater_Than_Or_Equal_Operator: GreaterEqualExpression,
    nodes.Iir_Kind.Match_Equality_Operator: MatchingEqualExpression,
    nodes.Iir_Kind.Match_Inequality_Operator: MatchingUnequalExpression,
    nodes.Iir_Kind.Match_Less_Than_Operator: MatchingLessThanExpression,
    nodes.Iir_Kind.Match_Less_Than_Or_Equal_Operator: MatchingLessEqualExpression,
    nodes.Iir_Kind.Match_Greater_Than_Operator: MatchingGreaterThanExpression,
    nodes.Iir_Kind.Match_Greater_Than_Or_Equal_Operator: MatchingGreaterEqualExpression,
    nodes.Iir_Kind.Sll_Operator: ShiftLeftLogicExpression,
    nodes.Iir_Kind.Srl_Operator: ShiftRightLogicExpression,
    nodes.Iir_Kind.Sla_Operator: ShiftLeftArithmeticExpression,
    nodes.Iir_Kind.Sra_Operator: ShiftRightArithmeticExpression,
    nodes.Iir_Kind.Rol_Operator: RotateLeftExpression,
    nodes.Iir_Kind.Ror_Operator: RotateRightExpression,
    nodes.Iir_Kind.Qualified_Expression: QualifiedExpression,
    nodes.Iir_Kind.Aggregate: Aggregate,
    nodes.Iir_Kind.Allocator_By_Subtype: SubtypeAllocation,
    nodes.Iir_Kind.Allocator_By_Expression: QualifiedExpressionAllocation,
}


@export
def GetExpressionFromNode(node: Iir) -> ExpressionUnion:
    """
    Translates an expression IIR node to an :class:`~pyVHDLModel.Expression.BaseExpression`.

    :param node: The IIR node representing an expression.
    :return:     The translated expression.
    """
    kind = GetIirKindOfNode(node)

    try:
        cls = __EXPRESSION_TRANSLATION[kind]
    except KeyError:
        position = Position.parse(node)
        raise DOMException(f"Unknown expression kind '{kind.name}' in expression '{node}' at {position}.")

    return cls.parse(node)


@export
def GetGenericsFromChainedNodes(nodeChain: Iir) -> Generator[GenericInterfaceItemMixin, None, None]:
    """
    Translates a chain of generics (IIR nodes) to a sequence of :class:`pyVHDLModel.Interface.GenericInterfaceItem`.

    :param nodeChain: The IIR node representing the first generic in the chain.
    :return:          A generator returning generic interface items.
    """
    from pyGHDL.dom.InterfaceItem import (
        GenericTypeInterfaceItem,
        GenericPackageInterfaceItem,
        GenericProcedureInterfaceItem,
        GenericFunctionInterfaceItem,
    )

    furtherIdentifiers = []
    generic = nodeChain
    while generic != nodes.Null_Iir:
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            parseNode = generic

            # Lookahead for generics with multiple identifiers at once
            if nodes.Get_Has_Identifier_List(generic):
                nextNode = nodes.Get_Chain(generic)
                for nextGeneric in utils.chain_iter(nextNode):
                    # Consecutive identifiers are found, if the subtype indication is Null
                    if nodes.Get_Subtype_Indication(nextGeneric) == nodes.Null_Iir:
                        furtherIdentifiers.append(GetNameOfNode(nextGeneric))
                    else:
                        generic = nextGeneric
                        break

                    # The last consecutive identifiers has no Identifier_List flag
                    if not nodes.Get_Has_Identifier_List(nextGeneric):
                        generic = nodes.Get_Chain(nextGeneric)
                        break
                else:
                    generic = nodes.Null_Iir
            else:
                generic = nodes.Get_Chain(generic)

            yield GenericConstantInterfaceItem.parse(parseNode, furtherIdentifiers)
            furtherIdentifiers.clear()
            continue
        else:
            if kind == nodes.Iir_Kind.Interface_Type_Declaration:
                yield GenericTypeInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Package_Declaration:
                yield GenericPackageInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Procedure_Declaration:
                yield GenericProcedureInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Function_Declaration:
                yield GenericFunctionInterfaceItem.parse(generic)
            else:
                position = Position.parse(generic)
                raise DOMException(f"Unknown generic kind '{kind.name}' in generic '{generic}' at {position}.")

        generic = nodes.Get_Chain(generic)


@export
def GetPortsFromChainedNodes(nodeChain: Iir) -> Generator[PortInterfaceItemMixin, None, None]:
    """
    Translates a chain of ports (IIR nodes) to a sequence of :class:`pyVHDLModel.Interface.PortInterfaceItem`.

    :param nodeChain: The IIR node representing the first port in the chain.
    :return:          A generator returning port interface items.
    """
    furtherIdentifiers = []
    port = nodeChain
    while port != nodes.Null_Iir:
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portToParse = port

            # Lookahead for ports with multiple identifiers at once
            if nodes.Get_Has_Identifier_List(port):
                nextNode = nodes.Get_Chain(port)
                for nextPort in utils.chain_iter(nextNode):
                    # Consecutive identifiers are found, if the subtype indication is Null
                    if nodes.Get_Subtype_Indication(nextPort) == nodes.Null_Iir:
                        furtherIdentifiers.append(GetNameOfNode(nextPort))
                    else:
                        port = nextPort
                        break

                    # The last consecutive identifiers has no Identifier_List flag
                    if not nodes.Get_Has_Identifier_List(nextPort):
                        port = nodes.Get_Chain(nextPort)
                        break
                else:
                    port = nodes.Null_Iir
            else:
                port = nodes.Get_Chain(port)

            yield PortSignalInterfaceItem.parse(portToParse, furtherIdentifiers)
            furtherIdentifiers.clear()
            continue
        else:
            position = Position.parse(port)
            raise DOMException(f"Unknown port kind '{kind.name}' in port '{port}' at {position}.")


@export
def GetParameterFromChainedNodes(nodeChain: Iir) -> Generator[ParameterInterfaceItemMixin, None, None]:
    """
    Translates a chain of parameters (IIR nodes) to a sequence of :class:`pyVHDLModel.Interface.ParameterInterfaceItem`.

    :param nodeChain: The IIR node representing the first parameter in the chain.
    :return:          A generator returning parameter interface items.
    """
    identifiers = []
    parameter = nodeChain
    while parameter != nodes.Null_Iir:
        kind = GetIirKindOfNode(parameter)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterConstantInterfaceItem

            parseMethod = ParameterConstantInterfaceItem.parse
            parseNode = parameter
        elif kind == nodes.Iir_Kind.Interface_Variable_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterVariableInterfaceItem

            parseMethod = ParameterVariableInterfaceItem.parse
            parseNode = parameter
        elif kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterSignalInterfaceItem

            parseMethod = ParameterSignalInterfaceItem.parse
            parseNode = parameter
        elif kind == nodes.Iir_Kind.Interface_File_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterFileInterfaceItem

            parseMethod = ParameterFileInterfaceItem.parse
            parseNode = parameter
        else:
            position = Position.parse(parameter)
            raise DOMException(f"Unknown parameter kind '{kind.name}' in parameter '{parameter}' at {position}.")

        # Lookahead for parameters with multiple identifiers at once
        if nodes.Get_Has_Identifier_List(parameter):
            nextNode = nodes.Get_Chain(parameter)
            for nextParameter in utils.chain_iter(nextNode):
                # Consecutive identifiers are found, if the subtype indication is Null
                if nodes.Get_Subtype_Indication(nextParameter) == nodes.Null_Iir:
                    identifiers.append(GetNameOfNode(nextParameter))
                else:
                    parameter = nextParameter
                    break

                # The last consecutive identifiers has no Identifier_List flag
                if not nodes.Get_Has_Identifier_List(nextParameter):
                    parameter = nodes.Get_Chain(nextParameter)
                    break
            else:
                parameter = nodes.Null_Iir
        else:
            parameter = nodes.Get_Chain(parameter)

        yield parseMethod(parseNode, identifiers)


def GetMapAspect(mapAspect: Iir, cls: Type, entity: str) -> Generator[AssociationItem, None, None]:
    for item in utils.chain_iter(mapAspect):
        kind = GetIirKindOfNode(item)
        if kind is nodes.Iir_Kind.Association_Element_By_Expression:
            formalNode = nodes.Get_Formal(item)
            if formalNode is nodes.Null_Iir:
                formal = None
            else:
                formal = GetName(formalNode)

            actual = GetExpressionFromNode(nodes.Get_Actual(item))

            yield cls(item, actual, formal)
        elif kind is nodes.Iir_Kind.Association_Element_Open:
            formalNode = nodes.Get_Formal(item)
            if formalNode is nodes.Null_Iir:
                formal = None
            else:
                formal = GetName(formalNode)

            yield cls(item, OpenName(item), formal)
        else:
            pos = Position.parse(item)
            raise DOMException(f"Unknown association kind '{kind.name}' in {entity} map at line {pos.Line}.")


def GetGenericMapAspect(genericMapAspect: Iir) -> Generator[GenericAssociationItem, None, None]:
    return GetMapAspect(genericMapAspect, GenericAssociationItem, "generic")


def GetPortMapAspect(portMapAspect: Iir) -> Generator[PortAssociationItem, None, None]:
    return GetMapAspect(portMapAspect, PortAssociationItem, "port")


def GetParameterMapAspect(parameterMapAspect: Iir) -> Generator[ParameterAssociationItem, None, None]:
    return GetMapAspect(parameterMapAspect, ParameterAssociationItem, "parameter")


def GetDeclaredItemsFromChainedNodes(nodeChain: Iir, entity: str, name: str) -> Generator[ModelEntity, None, None]:
    furtherIdentifiers = []
    item = nodeChain
    lastKind = None
    while item != nodes.Null_Iir:
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            objectParseMethod = Constant.parse
            objectItem = item
        elif kind == nodes.Iir_Kind.Variable_Declaration:
            from pyGHDL.dom.Object import SharedVariable

            if nodes.Get_Shared_Flag(item):
                objectParseMethod = SharedVariable.parse
                objectItem = item
            else:
                objectParseMethod = Variable.parse
                objectItem = item
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            objectParseMethod = Signal.parse
            objectItem = item
        elif kind == nodes.Iir_Kind.File_Declaration:
            from pyGHDL.dom.Object import File

            objectParseMethod = File.parse
            objectItem = item
        else:
            if kind == nodes.Iir_Kind.Type_Declaration:
                yield GetTypeFromNode(item)

            elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
                yield GetAnonymousTypeFromNode(item)

            elif kind == nodes.Iir_Kind.Subtype_Declaration:
                yield GetSubtypeFromNode(item)

            elif kind == nodes.Iir_Kind.Function_Declaration:
                if nodes.Get_Has_Body(item):
                    yield Function.parse(item)
                else:
                    WarningCollector.Raise("[NOT IMPLEMENTED] function declaration without body")

                lastKind = kind
                item = nodes.Get_Chain(item)
                continue
            elif kind == nodes.Iir_Kind.Function_Body:
                if lastKind is nodes.Iir_Kind.Function_Declaration:
                    pass
                else:
                    position = Position.parse(item)
                    raise DOMException(
                        f"Found unexpected function body '{GetNameOfNode(item)}' in {entity} '{name}' at {position}."
                    )
            elif kind == nodes.Iir_Kind.Procedure_Declaration:
                if nodes.Get_Has_Body(item):
                    yield Procedure.parse(item)
                else:
                    WarningCollector.Raise("[NOT IMPLEMENTED] procedure declaration without body")

                lastKind = kind
                item = nodes.Get_Chain(item)
                continue
            elif kind == nodes.Iir_Kind.Procedure_Body:
                if lastKind is nodes.Iir_Kind.Procedure_Declaration:
                    pass
                else:
                    position = Position.parse(item)
                    raise DOMException(
                        f"Found unexpected procedure body '{GetNameOfNode(item)}' in {entity} '{name}' at {position}."
                    )
            elif kind == nodes.Iir_Kind.Protected_Type_Body:
                yield ProtectedTypeBody.parse(item)
            elif kind == nodes.Iir_Kind.Object_Alias_Declaration:
                yield GetAliasFromNode(item)
            elif kind == nodes.Iir_Kind.Component_Declaration:
                from pyGHDL.dom.DesignUnit import Component

                yield Component.parse(item)
            elif kind == nodes.Iir_Kind.Attribute_Declaration:
                from pyGHDL.dom.Attribute import Attribute

                yield Attribute.parse(item)
            elif kind == nodes.Iir_Kind.Attribute_Specification:
                from pyGHDL.dom.Attribute import AttributeSpecification

                yield AttributeSpecification.parse(item)
            elif kind == nodes.Iir_Kind.Use_Clause:
                from pyGHDL.dom.DesignUnit import UseClause

                yield UseClause.parse(item)
            elif kind == nodes.Iir_Kind.Package_Declaration:
                from pyGHDL.dom.DesignUnit import Package

                yield Package.parse(item, None)  # TODO: Can it have a context?
            elif kind == nodes.Iir_Kind.Package_Instantiation_Declaration:
                from pyGHDL.dom.DesignUnit import PackageInstantiation

                yield PackageInstantiation.parse(item)
            elif kind == nodes.Iir_Kind.Configuration_Specification:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Configuration specification in {name}")
            elif kind == nodes.Iir_Kind.Psl_Default_Clock:
                yield DefaultClock.parse(item)
            elif kind == nodes.Iir_Kind.Group_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Group declaration in {name}")
            elif kind == nodes.Iir_Kind.Group_Template_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Group template declaration in {name}")
            elif kind == nodes.Iir_Kind.Disconnection_Specification:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Disconnect specification in {name}")
            elif kind == nodes.Iir_Kind.Nature_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Nature declaration in {name}")
            elif kind == nodes.Iir_Kind.Free_Quantity_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Free quantity declaration in {name}")
            elif kind == nodes.Iir_Kind.Across_Quantity_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Across quantity declaration in {name}")
            elif kind == nodes.Iir_Kind.Through_Quantity_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Through quantity declaration in {name}")
            elif kind == nodes.Iir_Kind.Terminal_Declaration:
                WarningCollector.Raise(f"[NOT IMPLEMENTED] Terminal declaration in {name}")
            else:
                position = Position.parse(item)
                raise DOMException(f"Unknown declared item kind '{kind.name}' in {entity} '{name}' at {position}.")

            lastKind = None
            item = nodes.Get_Chain(item)
            continue

        # Lookahead for objects with multiple identifiers at once
        if nodes.Get_Has_Identifier_List(item):
            nextNode = nodes.Get_Chain(item)
            for nextItem in utils.chain_iter(nextNode):
                # Consecutive identifiers are found, if the subtype indication is Null
                if nodes.Get_Subtype_Indication(nextItem) == nodes.Null_Iir:
                    furtherIdentifiers.append(GetNameOfNode(nextItem))
                else:
                    item = nextItem
                    break

                # The last consecutive identifiers has no Identifier_List flag
                if not nodes.Get_Has_Identifier_List(nextItem):
                    item = nodes.Get_Chain(nextItem)
                    break
            else:
                item = nodes.Null_Iir
        else:
            item = nodes.Get_Chain(item)

        yield objectParseMethod(objectItem, furtherIdentifiers)
        furtherIdentifiers.clear()


def GetConcurrentStatementsFromChainedNodes(
    nodeChain: Iir, entity: str, name: str
) -> Generator[ConcurrentStatement, None, None]:
    for statement in utils.chain_iter(nodeChain):
        label = nodes.Get_Label(statement)
        label = name_table.Get_Name_Ptr(label) if label != nodes.Null_Iir else None

        position = Position.parse(statement)

        kind = GetIirKindOfNode(statement)
        if kind == nodes.Iir_Kind.Sensitized_Process_Statement:
            yield ProcessStatement.parse(statement, label, True)

        elif kind == nodes.Iir_Kind.Process_Statement:
            yield ProcessStatement.parse(statement, label, False)

        elif kind == nodes.Iir_Kind.Concurrent_Simple_Signal_Assignment:
            yield ConcurrentSimpleSignalAssignment.parse(statement, label)
        elif kind == nodes.Iir_Kind.Concurrent_Conditional_Signal_Assignment:
            WarningCollector.Raise(
                f"[NOT IMPLEMENTED] Concurrent (conditional) signal assignment (label: '{label}') at line {position.Line}"
            )
        elif kind == nodes.Iir_Kind.Concurrent_Selected_Signal_Assignment:
            WarningCollector.Raise(
                f"[NOT IMPLEMENTED] Concurrent (selected) signal assignment (label: '{label}') at line {position.Line}"
            )
        elif kind == nodes.Iir_Kind.Concurrent_Procedure_Call_Statement:
            yield ConcurrentProcedureCall.parse(statement, label)
        elif kind == nodes.Iir_Kind.Component_Instantiation_Statement:
            instantiatedUnit = nodes.Get_Instantiated_Unit(statement)
            instantiatedUnitKind = GetIirKindOfNode(instantiatedUnit)

            if instantiatedUnitKind == nodes.Iir_Kind.Entity_Aspect_Entity:
                yield EntityInstantiation.parse(statement, instantiatedUnit, label)
            elif instantiatedUnitKind == nodes.Iir_Kind.Entity_Aspect_Configuration:
                yield ConfigurationInstantiation.parse(statement, instantiatedUnit, label)
            elif (
                instantiatedUnitKind == nodes.Iir_Kind.Simple_Name
                or instantiatedUnitKind == nodes.Iir_Kind.Selected_Name
            ):
                yield ComponentInstantiation.parse(statement, instantiatedUnit, label)
            else:
                raise DOMException(
                    f"Unknown instantiation kind '{instantiatedUnitKind.name}' in instantiation of label {label} at {position}."
                )
        elif kind == nodes.Iir_Kind.Block_Statement:
            yield ConcurrentBlockStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.If_Generate_Statement:
            yield IfGenerateStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Case_Generate_Statement:
            yield CaseGenerateStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.For_Generate_Statement:
            yield ForGenerateStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Psl_Assert_Directive:
            yield ConcurrentAssertStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Simple_Simultaneous_Statement:
            WarningCollector.Raise(
                f"[NOT IMPLEMENTED] Simple simultaneous statement (label: '{label}') at line {position.Line}"
            )
        else:
            raise DOMException(f"Unknown statement of kind '{kind.name}' in {entity} '{name}' at {position}.")


def GetSequentialStatementsFromChainedNodes(
    nodeChain: Iir, entity: str, name: str
) -> Generator[SequentialStatement, None, None]:
    for statement in utils.chain_iter(nodeChain):
        label = nodes.Get_Label(statement)
        label = name_table.Get_Name_Ptr(label) if label != nodes.Null_Iir else None

        position = Position.parse(statement)
        kind = GetIirKindOfNode(statement)
        if kind == nodes.Iir_Kind.If_Statement:
            yield IfStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.For_Loop_Statement:
            yield ForLoopStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Case_Statement:
            yield CaseStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Simple_Signal_Assignment_Statement:
            yield SequentialSimpleSignalAssignment.parse(statement, label)
        elif kind in (
            nodes.Iir_Kind.Variable_Assignment_Statement,
            nodes.Iir_Kind.Conditional_Variable_Assignment_Statement,
        ):
            WarningCollector.Raise(f"[NOT IMPLEMENTED] Variable assignment (label: '{label}') at line {position.Line}")
        elif kind == nodes.Iir_Kind.Wait_Statement:
            yield WaitStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Procedure_Call_Statement:
            yield SequentialProcedureCall.parse(statement, label)
        elif kind == nodes.Iir_Kind.Report_Statement:
            yield SequentialReportStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Assertion_Statement:
            yield SequentialAssertStatement.parse(statement, label)
        elif kind == nodes.Iir_Kind.Null_Statement:
            yield NullStatement(statement, label)
        elif kind == nodes.Iir_Kind.Exit_Statement:
            yield ExitStatement(statement, label)
        else:
            raise DOMException(f"Unknown statement of kind '{kind.name}' in {entity} '{name}' at {position}.")


def GetAliasFromNode(aliasNode: Iir):
    aliasName = GetNameOfNode(aliasNode)

    return Alias(aliasNode, aliasName)
