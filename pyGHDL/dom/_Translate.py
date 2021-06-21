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

from pyVHDLModel.VHDLModel import (
    Constraint,
    Direction,
    Expression,
    SubTypeOrSymbol,
    BaseType,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode, GetPositionOfNode
from pyGHDL.dom.Common import DOMException
from pyGHDL.dom.Symbol import (
    SimpleObjectOrFunctionCallSymbol,
    SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol,
    IndexedObjectOrFunctionCallSymbol,
)
from pyGHDL.dom.Type import IntegerType, SubType
from pyGHDL.dom.Range import Range, RangeExpression
from pyGHDL.dom.Literal import (
    IntegerLiteral,
    CharacterLiteral,
    FloatingPointLiteral,
    StringLiteral,
    PhysicalIntegerLiteral,
    PhysicalFloatingLiteral,
)
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
)
from pyGHDL.dom.Subprogram import Function, Procedure
from pyGHDL.dom.Misc import Alias


__all__ = []


@export
def GetSubtypeIndicationFromNode(node: Iir, entity: str, name: str) -> SubTypeOrSymbol:
    subTypeIndication = nodes.Get_Subtype_Indication(node)
    if subTypeIndication is nodes.Null_Iir:
        return None
    subTypeKind = GetIirKindOfNode(subTypeIndication)

    if subTypeKind == nodes.Iir_Kind.Simple_Name:
        subTypeName = GetNameOfNode(subTypeIndication)

        subType = SimpleSubTypeSymbol(subTypeName)
    elif subTypeKind == nodes.Iir_Kind.Array_Subtype_Definition:
        typeMark = nodes.Get_Subtype_Type_Mark(subTypeIndication)
        typeMarkName = GetNameOfNode(typeMark)

        constraints = GetArrayConstraintsFromSubtypeIndication(subTypeIndication)
        subType = ConstrainedSubTypeSymbol(typeMarkName, constraints)
    elif subTypeKind == nodes.Iir_Kind.Subtype_Definition:
        raise DOMException(
            "Unknown handling of subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}'.".format(
                kind=subTypeKind, indication=subTypeIndication, entity=entity, name=name
            )
        )
    else:
        position = GetPositionOfNode(node)
        raise DOMException(
            "Unknown subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}' at {file}:{line}:{column}.".format(
                kind=subTypeKind,
                indication=subTypeIndication,
                entity=entity,
                name=name,
                file=position.Filename,
                line=position.Line,
                column=position.Column,
            )
        )

    return subType


@export
def GetArrayConstraintsFromSubtypeIndication(
    subTypeIndication: Iir,
) -> List[Constraint]:
    constraints = []
    for constraint in utils.flist_iter(
        nodes.Get_Index_Constraint_List(subTypeIndication)
    ):
        constraintKind = GetIirKindOfNode(constraint)
        if constraintKind == nodes.Iir_Kind.Range_Expression:
            constraints.append(RangeExpression(GetRangeFromNode(constraint)))
        elif constraintKind == nodes.Iir_Kind.Attribute_Name:
            raise DOMException("[NOT IMPLEMENTED] Attribute name as range.")
        elif constraintKind == nodes.Iir_Kind.Simple_Name:
            raise DOMException("[NOT IMPLEMENTED] Subtype as range.")
        else:
            position = GetPositionOfNode(constraint)
            raise DOMException(
                "Unknown constraint kind '{kind}' for constraint '{constraint}' in subtype indication '{indication}' at {file}:{line}:{column}.".format(
                    kind=constraintKind,
                    constraint=constraint,
                    indication=subTypeIndication,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return constraints


@export
def GetTypeFromNode(node: Iir) -> BaseType:
    typeName = GetNameOfNode(node)
    leftBound = IntegerLiteral(0)
    rightBound = IntegerLiteral(15)

    return IntegerType(typeName, leftBound, rightBound)


@export
def GetSubTypeFromNode(node: Iir) -> BaseType:
    subTypeName = GetNameOfNode(node)

    return SubType(subTypeName)


@export
def GetRangeFromNode(node: Iir) -> Range:
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
    nodes.Iir_Kind.Parenthesis_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Integer_Literal: IntegerLiteral,
    nodes.Iir_Kind.Floating_Point_Literal: FloatingPointLiteral,
    nodes.Iir_Kind.Physical_Int_Literal: PhysicalIntegerLiteral,
    nodes.Iir_Kind.Physical_Fp_Literal: PhysicalFloatingLiteral,
    nodes.Iir_Kind.Character_Literal: CharacterLiteral,
    nodes.Iir_Kind.String_Literal8: StringLiteral,
    nodes.Iir_Kind.Negation_Operator: NegationExpression,
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
    nodes.Iir_Kind.Sll_Operator: ShiftLeftLogicExpression,
    nodes.Iir_Kind.Srl_Operator: ShiftRightLogicExpression,
    nodes.Iir_Kind.Sla_Operator: ShiftLeftArithmeticExpression,
    nodes.Iir_Kind.Sra_Operator: ShiftRightArithmeticExpression,
    nodes.Iir_Kind.Rol_Operator: RotateLeftExpression,
    nodes.Iir_Kind.Ror_Operator: RotateRightExpression,
    nodes.Iir_Kind.Qualified_Expression: QualifiedExpression,
    nodes.Iir_Kind.Aggregate: Aggregate,
}


@export
def GetExpressionFromNode(node: Iir) -> Expression:
    kind = GetIirKindOfNode(node)

    try:
        cls = __EXPRESSION_TRANSLATION[kind]
    except KeyError:
        position = GetPositionOfNode(node)
        raise DOMException(
            "Unknown expression kind '{kindName}'({kind}) in expression '{expr}' at {file}:{line}:{column}.".format(
                kind=kind,
                kindName=kind.name,
                expr=node,
                file=position.Filename,
                line=position.Line,
                column=position.Column,
            )
        )

    return cls.parse(node)


# FIXME: rewrite to generator
@export
def GetGenericsFromChainedNodes(nodeChain: Iir):
    result = []
    for generic in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            genericConstant = GenericConstantInterfaceItem.parse(generic)

            result.append(genericConstant)
        else:
            position = GetPositionOfNode(generic)
            raise DOMException(
                "Unknown generic kind '{kindName}'({kind}) in generic '{generic}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    generic=generic,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return result


# FIXME: rewrite to generator
@export
def GetPortsFromChainedNodes(nodeChain: Iir):
    result = []
    for port in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = PortSignalInterfaceItem.parse(port)

            result.append(portSignal)
        else:
            position = GetPositionOfNode(port)
            raise DOMException(
                "Unknown port kind '{kindName}'({kind}) in port '{port}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    port=port,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return result


# FIXME: rewrite to generator
@export
def GetParameterFromChainedNodes(nodeChain: Iir):
    result = []
    for parameter in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(parameter)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            pass
        elif kind == nodes.Iir_Kind.Interface_Variable_Declaration:
            pass
        elif kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = ParameterSignalInterfaceItem.parse(parameter)

            result.append(portSignal)
        else:
            position = GetPositionOfNode(parameter)
            raise DOMException(
                "Unknown parameter kind '{kindName}'({kind}) in parameter '{param}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    param=parameter,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return result


def GetDeclaredItemsFromChainedNodes(nodeChain: Iir, entity: str, name: str):
    result = []
    for item in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            result.append(Constant.parse(item))
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            result.append(Signal.parse(item))
        elif kind == nodes.Iir_Kind.Type_Declaration:
            result.append(GetTypeFromNode(item))
        elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
            result.append(GetTypeFromNode(item))
        elif kind == nodes.Iir_Kind.Subtype_Declaration:
            result.append(GetSubTypeFromNode(item))
        elif kind == nodes.Iir_Kind.Function_Declaration:
            result.append(Function.parse(item))
        elif kind == nodes.Iir_Kind.Function_Body:
            #                procedureName = NodeToName(item)
            print("found function body '{name}'".format(name="????"))
        elif kind == nodes.Iir_Kind.Procedure_Declaration:
            result.append(Procedure.parse(item))
        elif kind == nodes.Iir_Kind.Procedure_Body:
            #                procedureName = NodeToName(item)
            print("found procedure body '{name}'".format(name="????"))
        elif kind == nodes.Iir_Kind.Object_Alias_Declaration:
            result.append(GetAliasFromNode(item))
        elif kind == nodes.Iir_Kind.Component_Declaration:
            from pyGHDL.dom.DesignUnit import Component

            result.append(Component.parse(item))
        else:
            position = GetPositionOfNode(item)
            raise DOMException(
                "Unknown declared item kind '{kindName}'({kind}) in {entity} '{name}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    entity=entity,
                    name=name,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return result


def GetAliasFromNode(node: Iir):
    aliasName = GetNameOfNode(node)

    return Alias(aliasName)
