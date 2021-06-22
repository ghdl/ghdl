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
from pyGHDL.libghdl.vhdl import nodes
from pydecor import export

from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode, GetExpressionFromNode
from pyGHDL.dom._Utils import GetNameOfNode
from pyVHDLModel.VHDLModel import (
    Constant as VHDLModel_Constant,
    Variable as VHDLModel_Variable,
    Signal as VHDLModel_Signal,
    Expression,
    SubTypeOrSymbol,
)

__all__ = []


@export
class Constant(VHDLModel_Constant):
    def __init__(
        self, name: str, subType: SubTypeOrSymbol, defaultExpression: Expression
    ):
        super().__init__(name)

        self._name = name
        self._subType = subType
        self._defaultExpression = defaultExpression

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubtypeIndicationFromNode(node, "constant", name)
        defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(node))

        constant = cls(name, subTypeIndication, defaultExpression)

        return constant


@export
class Variable(VHDLModel_Variable):
    def __init__(
        self, name: str, subType: SubTypeOrSymbol, defaultExpression: Expression
    ):
        super().__init__(name)

        self._name = name
        self._subType = subType
        self._defaultExpression = defaultExpression

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubtypeIndicationFromNode(node, "variable", name)
        defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(node))

        variable = cls(name, subTypeIndication, defaultExpression)

        return variable


@export
class Signal(VHDLModel_Signal):
    def __init__(
        self, name: str, subType: SubTypeOrSymbol, defaultExpression: Expression
    ):
        super().__init__(name)

        self._name = name
        self._subType = subType
        self._defaultExpression = defaultExpression

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubtypeIndicationFromNode(node, "signal", name)
        default = nodes.Get_Default_Value(node)
        defaultExpression = GetExpressionFromNode(default) if default else None

        signal = cls(name, subTypeIndication, defaultExpression)

        return signal
