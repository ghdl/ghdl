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

from pyGHDL.dom._Translate import GetSubTypeIndicationFromNode, GetExpressionFromNode
from pyGHDL.dom._Utils import GetNameOfNode
from pyVHDLModel.VHDLModel import (
    Constant as VHDLModel_Constant,
    DeferredConstant as VHDLModel_DeferredConstant,
    Variable as VHDLModel_Variable,
    SharedVariable as VHDLModel_SharedVariable,
    Signal as VHDLModel_Signal,
    File as VHDLModel_File,
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
        subTypeIndication = GetSubTypeIndicationFromNode(node, "constant", name)
        defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(node))

        return cls(name, subTypeIndication, defaultExpression)


@export
class DeferredConstant(VHDLModel_DeferredConstant):
    def __init__(self, name: str, subType: SubTypeOrSymbol):
        super().__init__(name)

        self._name = name
        self._subType = subType

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubTypeIndicationFromNode(
            node, "deferred constant", name
        )

        return cls(name, subTypeIndication)


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
        subTypeIndication = GetSubTypeIndicationFromNode(node, "variable", name)
        defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(node))

        return cls(name, subTypeIndication, defaultExpression)


@export
class SharedVariable(VHDLModel_SharedVariable):
    def __init__(self, name: str, subType: SubTypeOrSymbol):
        super().__init__(name)

        self._name = name
        self._subType = subType

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubTypeIndicationFromNode(node, "variable", name)

        return cls(name, subTypeIndication)


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
        subTypeIndication = GetSubTypeIndicationFromNode(node, "signal", name)
        default = nodes.Get_Default_Value(node)
        defaultExpression = GetExpressionFromNode(default) if default else None

        return cls(name, subTypeIndication, defaultExpression)


@export
class File(VHDLModel_File):
    def __init__(self, name: str, subType: SubTypeOrSymbol):
        super().__init__(name)

        self._name = name
        self._subType = subType

    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        subTypeIndication = GetSubTypeIndicationFromNode(node, "file", name)

        return cls(name, subTypeIndication)
