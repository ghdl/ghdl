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
from pyGHDL.libghdl.vhdl.nodes import Null_Iir

from pyGHDL.libghdl.vhdl import nodes
from pydecor import export

from pyVHDLModel.VHDLModel import (
    GenericConstantInterfaceItem as VHDLModel_GenericConstantInterfaceItem,
    PortSignalInterfaceItem as VHDLModel_PortSignalInterfaceItem,
    Mode,
    SubTypeOrSymbol,
    Expression,
)

from pyGHDL.dom._Utils import NodeToName, GetModeOfNode
from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode, GetExpressionFromNode
from pyGHDL.dom.Common import GHDLMixin

__all__ = []


@export
class GenericConstantInterfaceItem(VHDLModel_GenericConstantInterfaceItem, GHDLMixin):
    @classmethod
    def parse(cls, generic):
        name = NodeToName(generic)
        mode = GetModeOfNode(generic)
        subTypeIndication = GetSubtypeIndicationFromNode(generic, "generic", name)
        value = GetExpressionFromNode(nodes.Get_Default_Value(generic))

        generic = cls(name, mode, subTypeIndication, value)

        return generic

    def __init__(self, name: str, mode: Mode, subType: SubTypeOrSymbol, defaultExpression: Expression):
        super().__init__(name=name, mode=mode)
        self._subType = subType
        self._defaultExpression = defaultExpression


@export
class PortSignalInterfaceItem(VHDLModel_PortSignalInterfaceItem, GHDLMixin):
    @classmethod
    def parse(cls, port):
        name = NodeToName(port)
        mode = GetModeOfNode(port)
        subTypeIndication = GetSubtypeIndicationFromNode(port, "port", name)

        defaultValue = nodes.Get_Default_Value(port)
        value = GetExpressionFromNode(defaultValue) if defaultValue != Null_Iir else None

        port = cls(name, mode, subTypeIndication, value)

        return port

    def __init__(
        self,
        name: str,
        mode: Mode,
        subType: SubTypeOrSymbol,
        defaultExpression: Expression = None,
    ):
        super().__init__(name=name, mode=mode)
        self._subType = subType
        self._defaultExpression = defaultExpression
