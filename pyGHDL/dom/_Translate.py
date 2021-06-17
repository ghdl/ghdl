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
from pyVHDLModel.VHDLModel import Constraint, Direction, Expression, SubTypeOrSymbol

from pyGHDL.libghdl import utils
from pyGHDL.libghdl.utils import flist_iter
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import NodeToName, GetIirKindOfNode
from pyGHDL.dom.Common import DOMException
from pyGHDL.dom.Range import Range, RangeExpression
from pyGHDL.dom.Symbol import (
    SimpleObjectSymbol,
    SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol,
)
from pyGHDL.dom.Literal import IntegerLiteral, CharacterLiteral, FloatingPointLiteral
from pyGHDL.dom.Expression import (
    SubtractionExpression,
    AdditionExpression,
    MultiplyExpression,
    DivisionExpression,
    InverseExpression,
    ExponentiationExpression,
)

__all__ = []


@export
def GetSubtypeIndicationFromNode(node, entity: str, name: str) -> SubTypeOrSymbol:
    subTypeIndication = nodes.Get_Subtype_Indication(node)
    subTypeKind = GetIirKindOfNode(subTypeIndication)

    if subTypeKind == nodes.Iir_Kind.Simple_Name:
        subTypeName = NodeToName(subTypeIndication)

        subType = SimpleSubTypeSymbol(subTypeName)
    elif subTypeKind == nodes.Iir_Kind.Array_Subtype_Definition:
        typeMark = nodes.Get_Subtype_Type_Mark(subTypeIndication)
        typeMarkName = NodeToName(typeMark)

        constraints = GetArrayConstraintsFromSubtypeIndication(subTypeIndication)
        subType = ConstrainedSubTypeSymbol(typeMarkName, constraints)
    elif subTypeKind == nodes.Iir_Kind.Subtype_Definition:
        raise DOMException(
            "Unknown handling of subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}'.".format(
                kind=subTypeKind, indication=subTypeIndication, entity=entity, name=name
            )
        )
    else:
        raise DOMException(
            "Unknown subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}'.".format(
                kind=subTypeKind, indication=subTypeIndication, entity=entity, name=name
            )
        )

    return subType


@export
def GetArrayConstraintsFromSubtypeIndication(subTypeIndication) -> List[Constraint]:
    constraints = []
    for constraint in flist_iter(nodes.Get_Index_Constraint_List(subTypeIndication)):
        constraintKind = GetIirKindOfNode(constraint)
        if constraintKind == nodes.Iir_Kind.Range_Expression:
            direction = nodes.Get_Direction(constraint)
            leftBound = nodes.Get_Left_Limit_Expr(constraint)
            rightBound = nodes.Get_Right_Limit_Expr(constraint)

            r = Range(
                GetExpressionFromNode(leftBound),
                GetExpressionFromNode(rightBound),
                Direction.DownTo if direction else Direction.To,
            )
            constraints.append(RangeExpression(r))
        elif constraintKind == nodes.Iir_Kind.Attribute_Name:
            raise DOMException("[NOT IMPLEMENTED] Attribute name as range.")
        elif constraintKind == nodes.Iir_Kind.Simple_Name:
            raise DOMException("[NOT IMPLEMENTED] Subtype as range.")
        else:
            raise DOMException(
                "Unknown constraint kind '{kind}' for constraint '{constraint}' in subtype indication '{indication}'.".format(
                    kind=constraintKind,
                    constraint=constraint,
                    indication=subTypeIndication,
                )
            )

    return constraints


__EXPRESSION_TRANSLATION = {
    nodes.Iir_Kind.Simple_Name: SimpleObjectSymbol,
    nodes.Iir_Kind.Integer_Literal: IntegerLiteral,
    nodes.Iir_Kind.Floating_Point_Literal: FloatingPointLiteral,
    nodes.Iir_Kind.Character_Literal: CharacterLiteral,
    nodes.Iir_Kind.Negation_Operator: InverseExpression,
    nodes.Iir_Kind.Addition_Operator: AdditionExpression,
    nodes.Iir_Kind.Substraction_Operator: SubtractionExpression,
    nodes.Iir_Kind.Multiplication_Operator: MultiplyExpression,
    nodes.Iir_Kind.Division_Operator: DivisionExpression,
    nodes.Iir_Kind.Exponentiation_Operator: ExponentiationExpression,
    #    nodes.Iir_Kind.Aggregate: Aggregate
}


@export
def GetExpressionFromNode(node) -> Expression:
    kind = GetIirKindOfNode(node)

    try:
        cls = __EXPRESSION_TRANSLATION[kind]
    except KeyError:
        raise DOMException(
            "Unknown expression kind '{kindName}'({kind}) in expression '{expr}'.".format(
                kind=kind, kindName=kind.name, expr=node
            )
        )

    return cls.parse(node)


# FIXME: rewrite to generator
@export
def GetGenericsFromChainedNodes(nodeChain):
    result = []
    for generic in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            genericConstant = GenericConstantInterfaceItem.parse(generic)

            result.append(genericConstant)
        else:
            raise DOMException(
                "Unknown generic kind '{kindName}'({kind}) in generic '{generic}'.".format(
                    kind=kind, kindName=kind.name, generic=generic
                )
            )

    return result


# FIXME: rewrite to generator
@export
def GetPortsFromChainedNodes(nodeChain):
    result = []
    for port in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = PortSignalInterfaceItem.parse(port)

            result.append(portSignal)
        else:
            raise DOMException(
                "Unknown port kind '{kindName}'({kind}) in port '{port}'.".format(
                    kind=kind, kindName=kind.name, port=port
                )
            )

    return result


def GetDeclaredItemsFromChainedNodes(nodeChain, entity: str, name: str):
    result = []
    for item in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            result.append(Constant.parse(item))
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            result.append(Signal.parse(item))
        elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
            typeName = NodeToName(item)
            print("found type '{name}'".format(name=typeName))
        elif kind == nodes.Iir_Kind.Subtype_Declaration:
            subTypeName = NodeToName(item)
            print("found subtype '{name}'".format(name=subTypeName))
        elif kind == nodes.Iir_Kind.Function_Declaration:
            functionName = NodeToName(item)
            print("found function '{name}'".format(name=functionName))
        elif kind == nodes.Iir_Kind.Function_Body:
            #                functionName = NodeToName(item)
            print("found function body '{name}'".format(name="????"))
        elif kind == nodes.Iir_Kind.Object_Alias_Declaration:
            aliasName = NodeToName(item)
            print("found alias '{name}'".format(name=aliasName))
        else:
            raise DOMException(
                "Unknown declared item kind '{kindName}'({kind}) in {entity} '{name}'.".format(
                    kind=kind, kindName=kind.name, entity=entity, name=name
                )
            )

    return result
