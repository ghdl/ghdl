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
# Package module:   DOM: Expressions.
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
from typing import List, Union

from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    UnaryExpression as VHDLModel_UnaryExpression,
    BinaryExpression as VHDLModel_BinaryExpression,
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
    MatchingEqualExpression as VHDLModel_MatchingEqualExpression,
    MatchingUnequalExpression as VHDLModel_MatchingUnequalExpression,
    MatchingLessThanExpression as VHDLModel_MatchingLessThanExpression,
    MatchingLessEqualExpression as VHDLModel_MatchingLessEqualExpression,
    MatchingGreaterThanExpression as VHDLModel_MatchingGreaterThanExpression,
    MatchingGreaterEqualExpression as VHDLModel_MatchingGreaterEqualExpression,
    ShiftRightLogicExpression as VHDLModel_ShiftRightLogicExpression,
    ShiftLeftLogicExpression as VHDLModel_ShiftLeftLogicExpression,
    ShiftRightArithmeticExpression as VHDLModel_ShiftRightArithmeticExpression,
    ShiftLeftArithmeticExpression as VHDLModel_ShiftLeftArithmeticExpression,
    RotateRightExpression as VHDLModel_RotateRightExpression,
    RotateLeftExpression as VHDLModel_RotateLeftExpression,
    SubtypeAllocation as VHDLModel_SubtypeAllocation,
    QualifiedExpressionAllocation as VHDLModel_QualifiedExpressionAllocation,
    Aggregate as VHDLModel_Aggregate,
    ExpressionUnion,
    AggregateElement,
    SubtypeOrSymbol,
    Symbol,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol
from pyGHDL.dom.Aggregates import (
    OthersAggregateElement,
    SimpleAggregateElement,
    RangedAggregateElement,
    IndexedAggregateElement,
    NamedAggregateElement,
)


__all__ = []


class _ParseUnaryExpressionMixin:
    @classmethod
    def parse(cls, node: Iir) -> VHDLModel_UnaryExpression:
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Operand(node))
        return cls(node, operand)


class _ParseBinaryExpressionMixin:
    @classmethod
    def parse(cls, node: Iir) -> VHDLModel_BinaryExpression:
        from pyGHDL.dom._Translate import GetExpressionFromNode

        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return cls(node, left, right)


@export
class InverseExpression(VHDLModel_InverseExpression, DOMMixin, _ParseUnaryExpressionMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
class IdentityExpression(VHDLModel_IdentityExpression, DOMMixin, _ParseUnaryExpressionMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
class NegationExpression(VHDLModel_NegationExpression, DOMMixin, _ParseUnaryExpressionMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
class AbsoluteExpression(VHDLModel_AbsoluteExpression, DOMMixin, _ParseUnaryExpressionMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
class ParenthesisExpression(VHDLModel_ParenthesisExpression, DOMMixin, _ParseUnaryExpressionMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "ParenthesisExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(node, operand)


@export
class TypeConversion(VHDLModel_TypeConversion, DOMMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
class FunctionCall(VHDLModel_FunctionCall, DOMMixin):
    def __init__(self, node: Iir, operand: ExpressionUnion):
        super().__init__()
        DOMMixin.__init__(self, node)


class RangeExpression(VHDLModel_RangeExpression, DOMMixin):
    @classmethod
    def parse(cls, node: Iir) -> Union["AscendingRangeExpression", "DescendingRangeExpression"]:
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
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class DescendingRangeExpression(VHDLModel_DescendingRangeExpression, DOMMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class AdditionExpression(VHDLModel_AdditionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class SubtractionExpression(VHDLModel_SubtractionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ConcatenationExpression(VHDLModel_ConcatenationExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MultiplyExpression(VHDLModel_MultiplyExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class DivisionExpression(VHDLModel_DivisionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class RemainderExpression(VHDLModel_RemainderExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ModuloExpression(VHDLModel_ModuloExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ExponentiationExpression(VHDLModel_ExponentiationExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class AndExpression(VHDLModel_AndExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class NandExpression(VHDLModel_NandExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class OrExpression(VHDLModel_OrExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class NorExpression(VHDLModel_NorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class XorExpression(VHDLModel_XorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class XnorExpression(VHDLModel_XnorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class EqualExpression(VHDLModel_EqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class UnequalExpression(VHDLModel_UnequalExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class LessThanExpression(VHDLModel_LessThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class LessEqualExpression(VHDLModel_LessEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class GreaterThanExpression(VHDLModel_GreaterThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class GreaterEqualExpression(VHDLModel_GreaterEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingEqualExpression(VHDLModel_MatchingEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingUnequalExpression(VHDLModel_MatchingUnequalExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingLessThanExpression(VHDLModel_MatchingLessThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingLessEqualExpression(VHDLModel_MatchingLessEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingGreaterThanExpression(VHDLModel_MatchingGreaterThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class MatchingGreaterEqualExpression(VHDLModel_MatchingGreaterEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ShiftRightLogicExpression(VHDLModel_ShiftRightLogicExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ShiftLeftLogicExpression(VHDLModel_ShiftLeftLogicExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ShiftRightArithmeticExpression(VHDLModel_ShiftRightArithmeticExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class ShiftLeftArithmeticExpression(VHDLModel_ShiftLeftArithmeticExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class RotateRightExpression(VHDLModel_RotateRightExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class RotateLeftExpression(VHDLModel_RotateLeftExpression, DOMMixin, _ParseBinaryExpressionMixin):
    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion):
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
class QualifiedExpression(VHDLModel_QualifiedExpression, DOMMixin):
    def __init__(self, node: Iir, subtype: SubtypeOrSymbol, operand: ExpressionUnion):
        super().__init__(subtype, operand)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetNameOfNode

        typeMarkName = GetNameOfNode(nodes.Get_Type_Mark(node))
        subtype = SimpleSubtypeSymbol(node, typeMarkName)
        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(node, subtype, operand)


@export
class SubtypeAllocation(VHDLModel_SubtypeAllocation, DOMMixin):
    def __init__(self, node: Iir, subtype: Symbol):
        super().__init__(subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpressionAllocation":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        subtype = GetSubtypeIndicationFromNode(node, "allocation", "?")

        return cls(node, subtype)


@export
class QualifiedExpressionAllocation(VHDLModel_QualifiedExpressionAllocation, DOMMixin):
    def __init__(self, node: Iir, qualifiedExpression: QualifiedExpression):
        super().__init__(qualifiedExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpressionAllocation":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        expression = GetExpressionFromNode(nodes.Get_Expression(node))

        return cls(node, expression)


@export
class Aggregate(VHDLModel_Aggregate, DOMMixin):
    def __init__(self, node: Iir, elements: List[AggregateElement]):
        super().__init__(elements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "Aggregate":
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetNameFromNode,
        )

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
                choiceRange = nodes.Get_Choice_Range(item)
                rangeKind = GetIirKindOfNode(choiceRange)
                if rangeKind == nodes.Iir_Kind.Range_Expression:
                    rng = GetRangeFromNode(choiceRange)
                elif rangeKind in (
                    nodes.Iir_Kind.Attribute_Name,
                    nodes.Iir_Kind.Parenthesis_Name,
                ):
                    rng = GetNameFromNode(choiceRange)
                else:
                    pos = Position.parse(item)
                    raise DOMException(
                        f"Unknown discete range kind '{rangeKind.name}' in for...generate statement at line {pos.Line}."
                    )

                choices.append(RangedAggregateElement(item, rng, value))
            elif kind == nodes.Iir_Kind.Choice_By_Name:
                name = GetNameFromNode(nodes.Get_Choice_Name(item))
                symbol = Symbol(item, name)
                choices.append(NamedAggregateElement(item, symbol, value))
            elif kind == nodes.Iir_Kind.Choice_By_Others:
                choices.append(OthersAggregateElement(item, value))
            else:
                raise DOMException(f"Unknown choice kind '{kind.name}' in aggregate '{node}'.")

        return cls(node, choices)
