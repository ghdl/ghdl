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

from pyGHDL.dom.Aggregates import OthersAggregateElement, SimpleAggregateElement, RangedAggregateElement, IndexedAggregateElement, NamedAggregateElement
from pyGHDL.dom.Symbol import EnumerationLiteralSymbol
from pyGHDL.libghdl import utils

from pyGHDL.dom.Common import DOMException
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyGHDL.libghdl.vhdl import nodes
from pydecor import export

from pyVHDLModel.VHDLModel import (
    InverseExpression as VHDLModel_InverseExpression,
    IdentityExpression as VHDLModel_IdentityExpression,
    NegationExpression as VHDLModel_NegationExpression,
    AbsoluteExpression as VHDLModel_AbsoluteExpression,
    TypeConversion as VHDLModel_TypeConversion,
    FunctionCall as VHDLModel_FunctionCall,
    QualifiedExpression as VHDLModel_QualifiedExpression,
    AdditionExpression as VHDLModel_AdditionExpression,
    SubtractionExpression as VHDLModel_SubtractionExpression,
    ConcatenationExpression as VHDLModel_ConcatenationExpression,
    MultiplyExpression as VHDLModel_MultiplyExpression,
    DivisionExpression as VHDLModel_DivisionExpression,
    RemainderExpression as VHDLModel_RemainderExpression,
    ModuloExpression as VHDLModel_ModuloExpression,
    ExponentiationExpression as VHDLModel_ExponentiationExpression,
    AndExpression as VHDLModel_AndExpression,
    NandExpression as VHDLModel_NandExpression,
    OrExpression as VHDLModel_OrExpression,
    NorExpression as VHDLModel_NorExpression,
    XorExpression as VHDLModel_XorExpression,
    XnorExpression as VHDLModel_XnorExpression,
    EqualExpression as VHDLModel_EqualExpression,
    UnequalExpression as VHDLModel_UnequalExpression,
    GreaterThanExpression as VHDLModel_GreaterThanExpression,
    GreaterEqualExpression as VHDLModel_GreaterEqualExpression,
    LessThanExpression as VHDLModel_LessThanExpression,
    ShiftRightLogicExpression as VHDLModel_ShiftRightLogicExpression,
    ShiftLeftLogicExpression as VHDLModel_ShiftLeftLogicExpression,
    ShiftRightArithmeticExpression as VHDLModel_ShiftRightArithmeticExpression,
    ShiftLeftArithmeticExpression as VHDLModel_ShiftLeftArithmeticExpression,
    RotateRightExpression as VHDLModel_RotateRightExpression,
    RotateLeftExpression as VHDLModel_RotateLeftExpression,
    Aggregate as VHDLModel_Aggregate,
    Expression, AggregateElement,
)

__all__ = []


class _ParseUnaryExpression:
    @classmethod
    def parse(cls, node):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Operand(node))
        return cls(operand)


class _ParseBinaryExpression:
    @classmethod
    def parse(cls, node):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return cls(left, right)


@export
class InverseExpression(VHDLModel_InverseExpression, _ParseUnaryExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class IdentityExpression(VHDLModel_IdentityExpression, _ParseUnaryExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class NegationExpression(VHDLModel_NegationExpression, _ParseUnaryExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class AbsoluteExpression(VHDLModel_AbsoluteExpression, _ParseUnaryExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class TypeConversion(VHDLModel_TypeConversion):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class FunctionCall(VHDLModel_FunctionCall):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class QualifiedExpression(VHDLModel_QualifiedExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class AdditionExpression(VHDLModel_AdditionExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class SubtractionExpression(VHDLModel_SubtractionExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ConcatenationExpression(
    VHDLModel_ConcatenationExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class MultiplyExpression(VHDLModel_MultiplyExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class DivisionExpression(VHDLModel_DivisionExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RemainderExpression(VHDLModel_RemainderExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ModuloExpression(VHDLModel_ModuloExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ExponentiationExpression(
    VHDLModel_ExponentiationExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class AndExpression(VHDLModel_AndExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class NandExpression(VHDLModel_NandExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class OrExpression(VHDLModel_OrExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class NorExpression(VHDLModel_NorExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class XorExpression(VHDLModel_XorExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class XnorExpression(VHDLModel_XnorExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class EqualExpression(VHDLModel_EqualExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class UnequalExpression(VHDLModel_UnequalExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterThanExpression(VHDLModel_GreaterThanExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterEqualExpression(VHDLModel_GreaterEqualExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class LessThanExpression(VHDLModel_LessThanExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightLogicExpression(
    VHDLModel_ShiftRightLogicExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftLogicExpression(
    VHDLModel_ShiftLeftLogicExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightArithmeticExpression(
    VHDLModel_ShiftRightArithmeticExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftArithmeticExpression(
    VHDLModel_ShiftLeftArithmeticExpression, _ParseBinaryExpression
):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RotateRightExpression(VHDLModel_RotateRightExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RotateLeftExpression(VHDLModel_RotateLeftExpression, _ParseBinaryExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class Aggregate(VHDLModel_Aggregate):
    def __init__(self, elements: List[AggregateElement]):
        super().__init__()
        self._elements = elements

    @classmethod
    def parse(cls, node):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        choices = []

        choicesChain = nodes.Get_Association_Choices_Chain(node)
        for item in utils.chain_iter(choicesChain):
            kind = GetIirKindOfNode(item)
            if kind == nodes.Iir_Kind.Choice_By_None:
                value = GetExpressionFromNode(nodes.Get_Associated_Expr(item))
                choices.append(SimpleAggregateElement(value))
            elif kind == nodes.Iir_Kind.Choice_By_Expression:
                index = GetExpressionFromNode(nodes.Get_Choice_Expression(item))
                value = GetExpressionFromNode(nodes.Get_Associated_Expr(item))
                choices.append(IndexedAggregateElement(index, value))
            elif kind == nodes.Iir_Kind.Choice_By_Range:
                r = GetExpressionFromNode(nodes.Get_Choice_Range(item))
                value = GetExpressionFromNode(nodes.Get_Associated_Expr(item))
                choices.append(RangedAggregateElement(r, value))
            elif kind == nodes.Iir_Kind.Choice_By_Name:
                name = EnumerationLiteralSymbol(nodes.Get_Choice_Name(item))
                value = GetExpressionFromNode(nodes.Get_Associated_Expr(item))
                choices.append(NamedAggregateElement(name, value))
            elif kind == nodes.Iir_Kind.Choice_By_Others:
                expression = None
                choices.append(OthersAggregateElement(expression))
            else:
                raise DOMException(
                    "Unknown choice kind '{kindName}'({kind}) in aggregate '{aggr}'.".format(
                        kind=kind, kindName=kind.name, aggr=node
                    )
                )

        return choices

