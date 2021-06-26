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

from pyGHDL.dom import DOMMixin, DOMException
from pyVHDLModel.VHDLModel import (
    InverseExpression as VHDLModel_InverseExpression,
    IdentityExpression as VHDLModel_IdentityExpression,
    NegationExpression as VHDLModel_NegationExpression,
    AbsoluteExpression as VHDLModel_AbsoluteExpression,
    SubExpression as VHDLModel_ParenthesisExpression,
    TypeConversion as VHDLModel_TypeConversion,
    FunctionCall as VHDLModel_FunctionCall,
    QualifiedExpression as VHDLModel_QualifiedExpression,
    RangeExpression as VHDLModel_RangeExpression,
    AscendingRangeExpression as VHDLModel_AscendingRangeExpression,
    DescendingRangeExpression as VHDLModel_DescendingRangeExpression,
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
    LessThanExpression as VHDLModel_LessThanExpression,
    LessEqualExpression as VHDLModel_LessEqualExpression,
    GreaterThanExpression as VHDLModel_GreaterThanExpression,
    GreaterEqualExpression as VHDLModel_GreaterEqualExpression,
    ShiftRightLogicExpression as VHDLModel_ShiftRightLogicExpression,
    ShiftLeftLogicExpression as VHDLModel_ShiftLeftLogicExpression,
    ShiftRightArithmeticExpression as VHDLModel_ShiftRightArithmeticExpression,
    ShiftLeftArithmeticExpression as VHDLModel_ShiftLeftArithmeticExpression,
    RotateRightExpression as VHDLModel_RotateRightExpression,
    RotateLeftExpression as VHDLModel_RotateLeftExpression,
    Aggregate as VHDLModel_Aggregate,
    Expression,
    AggregateElement,
    SubTypeOrSymbol,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyGHDL.dom.Symbol import EnumerationLiteralSymbol, SimpleSubTypeSymbol
from pyGHDL.dom.Aggregates import (
    OthersAggregateElement,
    SimpleAggregateElement,
    RangedAggregateElement,
    IndexedAggregateElement,
    NamedAggregateElement,
)


__all__ = []


class _ParseUnaryExpression:
    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Operand(node))
        return cls(node, operand)


class _ParseBinaryExpression:
    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return cls(node, left, right)


@export
class InverseExpression(VHDLModel_InverseExpression, DOMMixin, _ParseUnaryExpression):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


@export
class IdentityExpression(VHDLModel_IdentityExpression, DOMMixin, _ParseUnaryExpression):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


@export
class NegationExpression(VHDLModel_NegationExpression, DOMMixin, _ParseUnaryExpression):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


@export
class AbsoluteExpression(VHDLModel_AbsoluteExpression, DOMMixin, _ParseUnaryExpression):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


@export
class ParenthesisExpression(
    VHDLModel_ParenthesisExpression, DOMMixin, _ParseUnaryExpression
):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(node, operand)


@export
class TypeConversion(VHDLModel_TypeConversion, DOMMixin):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


@export
class FunctionCall(VHDLModel_FunctionCall, DOMMixin):
    def __init__(self, node: Iir, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._operand = operand


class RangeExpression(VHDLModel_RangeExpression, DOMMixin):
    @classmethod
    def parse(cls, node: Iir) -> "VHDLModel_RangeExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        direction = nodes.Get_Direction(node)
        leftBound = GetExpressionFromNode(nodes.Get_Left_Limit_Expr(node))
        rightBound = GetExpressionFromNode(nodes.Get_Right_Limit_Expr(node))

        if not direction:  # ascending
            return AscendingRangeExpression(node, leftBound, rightBound)
        else:
            return DescendingRangeExpression(node, leftBound, rightBound)


@export
class AscendingRangeExpression(VHDLModel_AscendingRangeExpression, DOMMixin):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class DescendingRangeExpression(VHDLModel_DescendingRangeExpression, DOMMixin):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class AdditionExpression(
    VHDLModel_AdditionExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class SubtractionExpression(
    VHDLModel_SubtractionExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ConcatenationExpression(
    VHDLModel_ConcatenationExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class MultiplyExpression(
    VHDLModel_MultiplyExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class DivisionExpression(
    VHDLModel_DivisionExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class RemainderExpression(
    VHDLModel_RemainderExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ModuloExpression(VHDLModel_ModuloExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ExponentiationExpression(
    VHDLModel_ExponentiationExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class AndExpression(VHDLModel_AndExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class NandExpression(VHDLModel_NandExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class OrExpression(VHDLModel_OrExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class NorExpression(VHDLModel_NorExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class XorExpression(VHDLModel_XorExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class XnorExpression(VHDLModel_XnorExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class EqualExpression(VHDLModel_EqualExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class UnequalExpression(VHDLModel_UnequalExpression, DOMMixin, _ParseBinaryExpression):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class LessThanExpression(
    VHDLModel_LessThanExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class LessEqualExpression(
    VHDLModel_LessEqualExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterThanExpression(
    VHDLModel_GreaterThanExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterEqualExpression(
    VHDLModel_GreaterEqualExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightLogicExpression(
    VHDLModel_ShiftRightLogicExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftLogicExpression(
    VHDLModel_ShiftLeftLogicExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightArithmeticExpression(
    VHDLModel_ShiftRightArithmeticExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftArithmeticExpression(
    VHDLModel_ShiftLeftArithmeticExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class RotateRightExpression(
    VHDLModel_RotateRightExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class RotateLeftExpression(
    VHDLModel_RotateLeftExpression, DOMMixin, _ParseBinaryExpression
):
    def __init__(self, node: Iir, left: Expression, right: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._leftOperand = left
        self._rightOperand = right


@export
class QualifiedExpression(VHDLModel_QualifiedExpression, DOMMixin):
    def __init__(self, node: Iir, subType: SubTypeOrSymbol, operand: Expression):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._subtype = subType
        self._operand = operand

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetNameOfNode

        typeMarkName = GetNameOfNode(nodes.Get_Type_Mark(node))
        subType = SimpleSubTypeSymbol(node, typeMarkName)
        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(subType, operand)


@export
class Aggregate(VHDLModel_Aggregate, DOMMixin):
    def __init__(self, node: Iir, elements: List[AggregateElement]):
        super().__init__()
        DOMMixin.__init__(self, node)

        self._elements = elements

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetRangeFromNode

        choices = []

        choicesChain = nodes.Get_Association_Choices_Chain(node)
        for item in utils.chain_iter(choicesChain):
            kind = GetIirKindOfNode(item)
            value = GetExpressionFromNode(nodes.Get_Associated_Expr(item))

            if kind == nodes.Iir_Kind.Choice_By_None:
                choices.append(SimpleAggregateElement(item, value))
            elif kind == nodes.Iir_Kind.Choice_By_Expression:
                index = GetExpressionFromNode(nodes.Get_Choice_Expression(item))
                choices.append(IndexedAggregateElement(item, index, value))
            elif kind == nodes.Iir_Kind.Choice_By_Range:
                r = GetRangeFromNode(nodes.Get_Choice_Range(item))
                choices.append(RangedAggregateElement(item, r, value))
            elif kind == nodes.Iir_Kind.Choice_By_Name:
                name = EnumerationLiteralSymbol(nodes.Get_Choice_Name(item))
                choices.append(NamedAggregateElement(item, name, value))
            elif kind == nodes.Iir_Kind.Choice_By_Others:
                choices.append(OthersAggregateElement(item, value))
            else:
                raise DOMException(
                    "Unknown choice kind '{kindName}'({kind}) in aggregate '{aggr}'.".format(
                        kind=kind, kindName=kind.name, aggr=node
                    )
                )

        return cls(node, choices)
