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
#  Copyright (C) 2019-2022 Tristan Gingold
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

from pyTooling.Decorators import export

from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Interface import GenericInterfaceItemMixin, ParameterInterfaceItemMixin
from pyVHDLModel.Subprogram import Procedure as VHDLModel_Procedure, Function as VHDLModel_Function

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol


@export
class Function(VHDLModel_Function, DOMMixin):
    def __init__(
        self,
        node: Iir,
        functionName: str,
        returnType: Symbol,
        genericItems: List[GenericInterfaceItemMixin] = None,
        parameterItems: List[ParameterInterfaceItemMixin] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(functionName, documentation)
        DOMMixin.__init__(self, node)

        # TODO: move to model
        self._genericItems = [] if genericItems is None else [g for g in genericItems]
        self._parameterItems = [] if parameterItems is None else [p for p in parameterItems]
        self._returnType = returnType

    @classmethod
    def parse(cls, functionNode: Iir) -> "Function":
        from pyGHDL.dom._Translate import (
            GetName,
            GetGenericsFromChainedNodes,
            GetParameterFromChainedNodes,
        )

        functionName = GetNameOfNode(functionNode)
        documentation = GetDocumentationOfNode(functionNode)

        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(functionNode))
        parameters = GetParameterFromChainedNodes(nodes.Get_Interface_Declaration_Chain(functionNode))

        returnType = nodes.Get_Return_Type_Mark(functionNode)
        returnTypeName = GetName(returnType)
        returnTypeSymbol = SimpleSubtypeSymbol(returnType, returnTypeName)

        return cls(functionNode, functionName, returnTypeSymbol, generics, parameters, documentation)


@export
class Procedure(VHDLModel_Procedure, DOMMixin):
    def __init__(
        self,
        node: Iir,
        procedureName: str,
        genericItems: List[GenericInterfaceItemMixin] = None,
        parameterItems: List[ParameterInterfaceItemMixin] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(procedureName, documentation)
        DOMMixin.__init__(self, node)

        # TODO: move to model
        self._genericItems = [] if genericItems is None else [g for g in genericItems]
        self._parameterItems = [] if parameterItems is None else [p for p in parameterItems]

    @classmethod
    def parse(cls, procedureNode: Iir) -> "Procedure":
        from pyGHDL.dom._Translate import (
            GetGenericsFromChainedNodes,
            GetParameterFromChainedNodes,
        )

        procedureName = GetNameOfNode(procedureNode)
        documentation = GetDocumentationOfNode(procedureNode)

        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(procedureNode))
        parameters = GetParameterFromChainedNodes(nodes.Get_Interface_Declaration_Chain(procedureNode))

        return cls(procedureNode, procedureName, generics, parameters, documentation)
