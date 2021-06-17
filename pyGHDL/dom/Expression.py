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
    Expression,
)

__all__ = []


@export
class InverseExpression(VHDLModel_InverseExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class IdentityExpression(VHDLModel_IdentityExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class NegationExpression(VHDLModel_NegationExpression):
    def __init__(self, operand: Expression):
        super().__init__()
        self._operand = operand


@export
class AbsoluteExpression(VHDLModel_AbsoluteExpression):
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
class AdditionExpression(VHDLModel_AdditionExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class SubtractionExpression(VHDLModel_SubtractionExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ConcatenationExpression(VHDLModel_ConcatenationExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class MultiplyExpression(VHDLModel_MultiplyExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class DivisionExpression(VHDLModel_DivisionExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RemainderExpression(VHDLModel_RemainderExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ModuloExpression(VHDLModel_ModuloExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ExponentiationExpression(VHDLModel_ExponentiationExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class AndExpression(VHDLModel_AndExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class NandExpression(VHDLModel_NandExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class OrExpression(VHDLModel_OrExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class NorExpression(VHDLModel_NorExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class XorExpression(VHDLModel_XorExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class XnorExpression(VHDLModel_XnorExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class EqualExpression(VHDLModel_EqualExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class UnequalExpression(VHDLModel_UnequalExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterThanExpression(VHDLModel_GreaterThanExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class GreaterEqualExpression(VHDLModel_GreaterEqualExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class LessThanExpression(VHDLModel_LessThanExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightLogicExpression(VHDLModel_ShiftRightLogicExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftLogicExpression(VHDLModel_ShiftLeftLogicExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftRightArithmeticExpression(VHDLModel_ShiftRightArithmeticExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class ShiftLeftArithmeticExpression(VHDLModel_ShiftLeftArithmeticExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RotateRightExpression(VHDLModel_RotateRightExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right


@export
class RotateLeftExpression(VHDLModel_RotateLeftExpression):
    def __init__(self, left: Expression, right: Expression):
        super().__init__()
        self._leftOperand = left
        self._rightOperand = right
