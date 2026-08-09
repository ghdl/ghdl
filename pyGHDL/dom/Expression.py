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
"""
This module implements derived expression classes from :mod:`pyVHDLModel.Expression`.
"""

from typing import List, Union

from pyTooling.Decorators import export, InheritDocString
from pyTooling.MetaClasses import ExtendedType

from pyVHDLModel.Base import ExpressionUnion
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Expression import (
    UnaryExpression as VHDLModel_UnaryExpression,
    NegationExpression as VHDLModel_NegationExpression,
    IdentityExpression as VHDLModel_IdentityExpression,
    InverseExpression as VHDLModel_InverseExpression,
    AbsoluteExpression as VHDLModel_AbsoluteExpression,
    TypeConversion as VHDLModel_TypeConversion,
    SubExpression as VHDLModel_ParenthesisExpression,
    BinaryExpression as VHDLModel_BinaryExpression,
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
    UnaryAndExpression as VHDLModel_UnaryAndExpression,
    UnaryNandExpression as VHDLModel_UnaryNandExpression,
    UnaryOrExpression as VHDLModel_UnaryOrExpression,
    UnaryNorExpression as VHDLModel_UnaryNorExpression,
    UnaryXorExpression as VHDLModel_UnaryXorExpression,
    UnaryXnorExpression as VHDLModel_UnaryXnorExpression,
    EqualExpression as VHDLModel_EqualExpression,
    UnequalExpression as VHDLModel_UnequalExpression,
    GreaterThanExpression as VHDLModel_GreaterThanExpression,
    GreaterEqualExpression as VHDLModel_GreaterEqualExpression,
    LessThanExpression as VHDLModel_LessThanExpression,
    LessEqualExpression as VHDLModel_LessEqualExpression,
    MatchingEqualExpression as VHDLModel_MatchingEqualExpression,
    MatchingUnequalExpression as VHDLModel_MatchingUnequalExpression,
    MatchingGreaterThanExpression as VHDLModel_MatchingGreaterThanExpression,
    MatchingGreaterEqualExpression as VHDLModel_MatchingGreaterEqualExpression,
    MatchingLessThanExpression as VHDLModel_MatchingLessThanExpression,
    MatchingLessEqualExpression as VHDLModel_MatchingLessEqualExpression,
    ShiftRightLogicExpression as VHDLModel_ShiftRightLogicExpression,
    ShiftLeftLogicExpression as VHDLModel_ShiftLeftLogicExpression,
    ShiftRightArithmeticExpression as VHDLModel_ShiftRightArithmeticExpression,
    ShiftLeftArithmeticExpression as VHDLModel_ShiftLeftArithmeticExpression,
    RotateRightExpression as VHDLModel_RotateRightExpression,
    RotateLeftExpression as VHDLModel_RotateLeftExpression,
    QualifiedExpression as VHDLModel_QualifiedExpression,
    FunctionCall as VHDLModel_FunctionCall,
    SubtypeAllocation as VHDLModel_SubtypeAllocation,
    QualifiedExpressionAllocation as VHDLModel_QualifiedExpressionAllocation,
    AggregateElement,
    Aggregate as VHDLModel_Aggregate,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyVHDLModel.Symbol import SubtypeSymbol
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol, RecordElementSymbol
from pyGHDL.dom.Aggregates import (
    OthersAggregateElement,
    SimpleAggregateElement,
    RangedAggregateElement,
    IndexedAggregateElement,
    NamedAggregateElement,
)


class _ParseUnaryExpressionMixin(metaclass=ExtendedType, mixin=True):
    """
    Mixin providing a :meth:`parse` classmethod for all unary expressions.

    The IIR node of a unary expression carries its single operand in the ``Operand`` field, so the same
    translation applies to every operator and only the concrete class differs.
    """

    @classmethod
    def parse(cls, node: Iir) -> VHDLModel_UnaryExpression:
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Operand(node))
        return cls(node, operand)


class _ParseBinaryExpressionMixin(metaclass=ExtendedType, mixin=True):
    """
    Mixin providing a :meth:`parse` classmethod for all binary expressions.

    The IIR node of a binary expression carries its operands in the ``Left`` and ``Right`` fields, so the
    same translation applies to every operator and only the concrete class differs.
    """

    @classmethod
    def parse(cls, node: Iir) -> VHDLModel_BinaryExpression:
        from pyGHDL.dom._Translate import GetExpressionFromNode

        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return cls(node, left, right)


@export
@InheritDocString(VHDLModel_InverseExpression, merge=True)
class InverseExpression(VHDLModel_InverseExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.InverseExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes an inverse expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_IdentityExpression, merge=True)
class IdentityExpression(VHDLModel_IdentityExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.IdentityExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes an identity expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_NegationExpression, merge=True)
class NegationExpression(VHDLModel_NegationExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.NegationExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a negation expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_AbsoluteExpression, merge=True)
class AbsoluteExpression(VHDLModel_AbsoluteExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.AbsoluteExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes an absolute expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ParenthesisExpression, merge=True)
class ParenthesisExpression(VHDLModel_ParenthesisExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.SubExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a parenthesis expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "ParenthesisExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(node, operand)


@export
class TypeConversion(VHDLModel_TypeConversion, DOMMixin):
    """
    Represents a *type conversion*, e.g. ``integer(x)``.

    .. note::

       Not reachable from this translator today. GHDL's parser cannot tell a type conversion from an
       indexed name or a function call - ``integer(x)``, ``arr(x)`` and ``f(x)`` are all parsed as
       ``Parenthesis_Name`` - and only semantic analysis rewrites one into ``Type_Conversion``.
       :mod:`pyGHDL.dom` parses without that pass, so a ``Type_Conversion`` node never arrives and
       ``Parenthesis_Name`` is translated to
       :class:`~pyVHDLModel.Symbol.IndexedObjectOrFunctionCallSymbol`, which carries the ambiguity.

       The constructor is kept correct so this class works if semantic analysis is ever run.
    """

    def __init__(self, node: Iir, targetSubtype: SubtypeSymbol, operand: ExpressionUnion) -> None:
        """
        Initializes a type conversion.

        :param node:          The IIR node this object was translated from.
        :param targetSubtype: Reference to the subtype the expression is converted to.
        :param operand:       The expression the operator is applied to.
        """
        super().__init__(targetSubtype, operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_FunctionCall, merge=True)
class FunctionCall(VHDLModel_FunctionCall, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.FunctionCall`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a function call.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the function is applied to.

        .. todo::

           The operand is not forwarded to the base-class, because
           :class:`pyVHDLModel.Expression.FunctionCall` does not model a call's operands yet.
        """
        super().__init__()
        DOMMixin.__init__(self, node)


@InheritDocString(VHDLModel_RangeExpression, merge=True)
class RangeExpression(VHDLModel_RangeExpression, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.RangeExpression`.
    """

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
@InheritDocString(VHDLModel_AscendingRangeExpression, merge=True)
class AscendingRangeExpression(VHDLModel_AscendingRangeExpression, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.AscendingRangeExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an ascending range expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_DescendingRangeExpression, merge=True)
class DescendingRangeExpression(VHDLModel_DescendingRangeExpression, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.DescendingRangeExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a descending range expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_AdditionExpression, merge=True)
class AdditionExpression(VHDLModel_AdditionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.AdditionExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an addition expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_SubtractionExpression, merge=True)
class SubtractionExpression(VHDLModel_SubtractionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.SubtractionExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a subtraction expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ConcatenationExpression, merge=True)
class ConcatenationExpression(VHDLModel_ConcatenationExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ConcatenationExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a concatenation expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MultiplyExpression, merge=True)
class MultiplyExpression(VHDLModel_MultiplyExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MultiplyExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a multiply expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_DivisionExpression, merge=True)
class DivisionExpression(VHDLModel_DivisionExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.DivisionExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a division expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_RemainderExpression, merge=True)
class RemainderExpression(VHDLModel_RemainderExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.RemainderExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a remainder expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ModuloExpression, merge=True)
class ModuloExpression(VHDLModel_ModuloExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ModuloExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a modulo expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ExponentiationExpression, merge=True)
class ExponentiationExpression(VHDLModel_ExponentiationExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ExponentiationExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an exponentiation expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_AndExpression, merge=True)
class AndExpression(VHDLModel_AndExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.AndExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an and expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_NandExpression, merge=True)
class NandExpression(VHDLModel_NandExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.NandExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a nand expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_OrExpression, merge=True)
class OrExpression(VHDLModel_OrExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.OrExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an or expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_NorExpression, merge=True)
class NorExpression(VHDLModel_NorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.NorExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a nor expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_XorExpression, merge=True)
class XorExpression(VHDLModel_XorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.XorExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an :vhdlkw:`xor` expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_XnorExpression, merge=True)
class XnorExpression(VHDLModel_XnorExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.XnorExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an :vhdlkw:`xnor` expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryAndExpression, merge=True)
class UnaryAndExpression(VHDLModel_UnaryAndExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryAndExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary and expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryNandExpression, merge=True)
class UnaryNandExpression(VHDLModel_UnaryNandExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryNandExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary nand expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryOrExpression, merge=True)
class UnaryOrExpression(VHDLModel_UnaryOrExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryOrExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary or expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryNorExpression, merge=True)
class UnaryNorExpression(VHDLModel_UnaryNorExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryNorExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary nor expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryXorExpression, merge=True)
class UnaryXorExpression(VHDLModel_UnaryXorExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryXorExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary xor expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnaryXnorExpression, merge=True)
class UnaryXnorExpression(VHDLModel_UnaryXnorExpression, DOMMixin, _ParseUnaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnaryXnorExpression`.
    """

    def __init__(self, node: Iir, operand: ExpressionUnion) -> None:
        """
        Initializes a unary xnor expression.

        :param node:    The IIR node this object was translated from.
        :param operand: The expression the operator is applied to.
        """
        super().__init__(operand)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_EqualExpression, merge=True)
class EqualExpression(VHDLModel_EqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.EqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_UnequalExpression, merge=True)
class UnequalExpression(VHDLModel_UnequalExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.UnequalExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes an unequal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_LessThanExpression, merge=True)
class LessThanExpression(VHDLModel_LessThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.LessThanExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a less than expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_LessEqualExpression, merge=True)
class LessEqualExpression(VHDLModel_LessEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.LessEqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a less equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_GreaterThanExpression, merge=True)
class GreaterThanExpression(VHDLModel_GreaterThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.GreaterThanExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a greater than expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_GreaterEqualExpression, merge=True)
class GreaterEqualExpression(VHDLModel_GreaterEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.GreaterEqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a greater equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingEqualExpression, merge=True)
class MatchingEqualExpression(VHDLModel_MatchingEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingEqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingUnequalExpression, merge=True)
class MatchingUnequalExpression(VHDLModel_MatchingUnequalExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingUnequalExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching unequal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingLessThanExpression, merge=True)
class MatchingLessThanExpression(VHDLModel_MatchingLessThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingLessThanExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching less than expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingLessEqualExpression, merge=True)
class MatchingLessEqualExpression(VHDLModel_MatchingLessEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingLessEqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching less equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingGreaterThanExpression, merge=True)
class MatchingGreaterThanExpression(VHDLModel_MatchingGreaterThanExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingGreaterThanExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching greater than expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_MatchingGreaterEqualExpression, merge=True)
class MatchingGreaterEqualExpression(VHDLModel_MatchingGreaterEqualExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.MatchingGreaterEqualExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a matching greater equal expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ShiftRightLogicExpression, merge=True)
class ShiftRightLogicExpression(VHDLModel_ShiftRightLogicExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ShiftRightLogicExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a shift right logic expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ShiftLeftLogicExpression, merge=True)
class ShiftLeftLogicExpression(VHDLModel_ShiftLeftLogicExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ShiftLeftLogicExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a shift left logic expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ShiftRightArithmeticExpression, merge=True)
class ShiftRightArithmeticExpression(VHDLModel_ShiftRightArithmeticExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ShiftRightArithmeticExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a shift right arithmetic expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ShiftLeftArithmeticExpression, merge=True)
class ShiftLeftArithmeticExpression(VHDLModel_ShiftLeftArithmeticExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.ShiftLeftArithmeticExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a shift left arithmetic expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_RotateRightExpression, merge=True)
class RotateRightExpression(VHDLModel_RotateRightExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.RotateRightExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a rotate right expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_RotateLeftExpression, merge=True)
class RotateLeftExpression(VHDLModel_RotateLeftExpression, DOMMixin, _ParseBinaryExpressionMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.RotateLeftExpression`.
    """

    def __init__(self, node: Iir, left: ExpressionUnion, right: ExpressionUnion) -> None:
        """
        Initializes a rotate left expression.

        :param node:  The IIR node this object was translated from.
        :param left:  The expression left of the operator.
        :param right: The expression right of the operator.
        """
        super().__init__(left, right)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_QualifiedExpression, merge=True)
class QualifiedExpression(VHDLModel_QualifiedExpression, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.QualifiedExpression`.
    """

    def __init__(self, node: Iir, subtype: Symbol, operand: ExpressionUnion) -> None:
        """
        Initializes a qualified expression.

        :param node:    The IIR node this object was translated from.
        :param subtype: Reference to the subtype qualifying the expression.
        :param operand: The expression being qualified.
        """
        super().__init__(subtype, operand)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetName

        typeMarkName = GetName(nodes.Get_Type_Mark(node))
        subtype = SimpleSubtypeSymbol(node, typeMarkName)
        operand = GetExpressionFromNode(nodes.Get_Expression(node))
        return cls(node, subtype, operand)


@export
@InheritDocString(VHDLModel_SubtypeAllocation, merge=True)
class SubtypeAllocation(VHDLModel_SubtypeAllocation, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.SubtypeAllocation`.
    """

    def __init__(self, node: Iir, subtype: Symbol) -> None:
        """
        Initializes an allocation of a subtype via :vhdlkw:`new`.

        :param node:    The IIR node this object was translated from.
        :param subtype: Reference to the subtype being allocated.
        """
        super().__init__(subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpressionAllocation":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        subtype = GetSubtypeIndicationFromNode(node, "allocation", "?")

        return cls(node, subtype)


@export
@InheritDocString(VHDLModel_QualifiedExpressionAllocation, merge=True)
class QualifiedExpressionAllocation(VHDLModel_QualifiedExpressionAllocation, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.QualifiedExpressionAllocation`.
    """

    def __init__(self, node: Iir, qualifiedExpression: QualifiedExpression) -> None:
        """
        Initializes an allocation initialized by a qualified expression.

        :param node:                The IIR node this object was translated from.
        :param qualifiedExpression: The qualified expression the allocated object is initialized with.
        """
        super().__init__(qualifiedExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "QualifiedExpressionAllocation":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        expression = GetExpressionFromNode(nodes.Get_Expression(node))

        return cls(node, expression)


@export
@InheritDocString(VHDLModel_Aggregate, merge=True)
class Aggregate(VHDLModel_Aggregate, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.Aggregate`.
    """

    def __init__(self, node: Iir, elements: List[AggregateElement]) -> None:
        """
        Initializes an aggregate.

        :param node:     The IIR node this object was translated from.
        :param elements: List of all elements of this aggregate, in the order they were written.
        """
        super().__init__(elements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "Aggregate":
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetDiscreteRangeFromNode,
            GetName,
        )

        choices = []
        ownerNode = nodes.Null_Iir

        choicesChain = nodes.Get_Association_Choices_Chain(node)
        for item in utils.chain_iter(choicesChain):
            kind = GetIirKindOfNode(item)

            # A choice list (`b | c => '0'`) groups multiple choices onto one associated expression:
            # the *first* choice in the group owns it (``Same_Alternative_Flag=False``), while the later
            # ones have a null ``Associated_Expr``. Same grouping algorithm as case statement
            # alternatives, see :func:`pyGHDL.dom.Concurrent.GetSelectedWaveformsFromChainedNodes`.
            if not nodes.Get_Same_Alternative_Flag(item):
                ownerNode = item
            elif ownerNode is nodes.Null_Iir:
                position = Position.parse(item)
                raise DOMException(
                    f"Aggregate choice at line {position.Line} continues a choice list that never started."
                )

            # Translated per element rather than once per group: each AggregateElement takes ownership of
            # its expression by setting ``Parent``, so sharing one object across elements would misparent it.
            value = GetExpressionFromNode(nodes.Get_Associated_Expr(ownerNode))

            if kind == nodes.Iir_Kind.Choice_By_None:
                choices.append(SimpleAggregateElement(item, value))
            elif kind == nodes.Iir_Kind.Choice_By_Expression:
                index = GetExpressionFromNode(nodes.Get_Choice_Expression(item))
                choices.append(IndexedAggregateElement(item, index, value))
            elif kind == nodes.Iir_Kind.Choice_By_Range:
                rng = GetDiscreteRangeFromNode(nodes.Get_Choice_Range(item), "aggregate choice")
                choices.append(RangedAggregateElement(item, rng, value))
            elif kind == nodes.Iir_Kind.Choice_By_Name:
                # An aggregate choice name always denotes a record element, never an object.
                choiceName = nodes.Get_Choice_Name(item)
                symbol = RecordElementSymbol(choiceName, GetName(choiceName))
                choices.append(NamedAggregateElement(item, symbol, value))
            elif kind == nodes.Iir_Kind.Choice_By_Others:
                choices.append(OthersAggregateElement(item, value))
            else:
                raise DOMException(f"Unknown choice kind '{kind.name}' in aggregate '{node}'.")

        return cls(node, choices)
