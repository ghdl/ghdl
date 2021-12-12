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
from typing import Union, List

from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    Constant as VHDLModel_Constant,
    DeferredConstant as VHDLModel_DeferredConstant,
    Variable as VHDLModel_Variable,
    SharedVariable as VHDLModel_SharedVariable,
    Signal as VHDLModel_Signal,
    File as VHDLModel_File,
    ExpressionUnion,
    SubtypeOrSymbol,
)

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode

__all__ = []


@export
class Constant(VHDLModel_Constant, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion,
    ):
        super().__init__(identifiers, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, constantNode: Iir) -> Union["Constant", "DeferredConstant"]:
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(constantNode)
        subtypeIndication = GetSubtypeIndicationFromNode(constantNode, "constant", name)
        defaultValue = nodes.Get_Default_Value(constantNode)
        if defaultValue != nodes.Null_Iir:
            defaultExpression = GetExpressionFromNode(defaultValue)

            return cls(
                constantNode,
                [
                    name,
                ],
                subtypeIndication,
                defaultExpression,
            )
        else:
            return DeferredConstant(
                constantNode,
                [
                    name,
                ],
                subtypeIndication,
            )


@export
class DeferredConstant(VHDLModel_DeferredConstant, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: SubtypeOrSymbol):
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, constantNode: Iir) -> "DeferredConstant":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(constantNode)
        subtypeIndication = GetSubtypeIndicationFromNode(constantNode, "deferred constant", name)

        return cls(
            constantNode,
            [
                name,
            ],
            subtypeIndication,
        )


@export
class Variable(VHDLModel_Variable, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion,
    ):
        super().__init__(identifiers, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, variableNode: Iir) -> "Variable":
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(variableNode)
        subtypeIndication = GetSubtypeIndicationFromNode(variableNode, "variable", name)
        defaultValue = nodes.Get_Default_Value(variableNode)
        defaultExpression = None
        if defaultValue != nodes.Null_Iir:
            defaultExpression = GetExpressionFromNode(defaultValue)

        return cls(
            variableNode,
            [
                name,
            ],
            subtypeIndication,
            defaultExpression,
        )


@export
class SharedVariable(VHDLModel_SharedVariable, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: SubtypeOrSymbol):
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, variableNode: Iir) -> "SharedVariable":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(variableNode)
        subtypeIndication = GetSubtypeIndicationFromNode(variableNode, "variable", name)

        return cls(
            variableNode,
            [
                name,
            ],
            subtypeIndication,
        )


@export
class Signal(VHDLModel_Signal, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion,
    ):
        super().__init__(identifiers, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, signalNode: Iir) -> "Signal":
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(signalNode)
        subtypeIndication = GetSubtypeIndicationFromNode(signalNode, "signal", name)
        default = nodes.Get_Default_Value(signalNode)
        defaultExpression = GetExpressionFromNode(default) if default else None

        return cls(
            signalNode,
            [
                name,
            ],
            subtypeIndication,
            defaultExpression,
        )


@export
class File(VHDLModel_File, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: SubtypeOrSymbol):
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, fileNode: Iir) -> "File":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(fileNode)
        subtypeIndication = GetSubtypeIndicationFromNode(fileNode, "file", name)

        # FIXME: handle file open stuff

        return cls(
            fileNode,
            [
                name,
            ],
            subtypeIndication,
        )
