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
from pyGHDL.dom.Symbol import SimpleSubTypeSymbol
from pyGHDL.libghdl.vhdl import nodes
from pydecor import export

from pyGHDL.dom._Utils import GetNameOfNode
from pyVHDLModel.VHDLModel import (
    Function as VHDLModel_Function,
    Procedure as VHDLModel_Procedure,
    SubTypeOrSymbol,
)
from pyGHDL.libghdl._types import Iir


@export
class Function(VHDLModel_Function):
    def __init__(self, functionName: str, returnType: SubTypeOrSymbol):
        super().__init__(functionName)
        self._returnType = returnType

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import (
            GetGenericsFromChainedNodes,
            GetParameterFromChainedNodes,
        )

        functionName = GetNameOfNode(node)
        returnType = nodes.Get_Return_Type_Mark(node)
        returnTypeName = GetNameOfNode(returnType)

        returnTypeSymbol = SimpleSubTypeSymbol(returnTypeName)
        function = cls(functionName, returnTypeSymbol)

        for generic in GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(node)):
            function.GenericItems.append(generic)
        for port in GetParameterFromChainedNodes(
            nodes.Get_Interface_Declaration_Chain(node)
        ):
            function.ParameterItems.append(port)

        return function


@export
class Procedure(VHDLModel_Procedure):
    def __init__(self, procedureName: str):
        super().__init__(procedureName)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import (
            GetGenericsFromChainedNodes,
            GetParameterFromChainedNodes,
        )

        procedureName = GetNameOfNode(node)

        procedure = cls(procedureName)

        for generic in GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(node)):
            procedure.GenericItems.append(generic)
        for port in GetParameterFromChainedNodes(
            nodes.Get_Interface_Declaration_Chain(node)
        ):
            procedure.ParameterItems.append(port)

        return procedure
