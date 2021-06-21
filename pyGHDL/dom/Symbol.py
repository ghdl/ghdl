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

from pyVHDLModel.VHDLModel import (
    EntitySymbol as VHDLModel_EntitySymbol,
    SimpleSubTypeSymbol as VHDLModel_SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol as VHDLModel_ConstrainedSubTypeSymbol,
    EnumerationLiteralSymbol as VHDLModel_EnumerationLiteralSymbol,
    SimpleObjectOrFunctionCallSymbol as VHDLModel_SimpleObjectOrFunctionCallSymbol,
    IndexedObjectOrFunctionCallSymbol as VHDLModel_IndexedObjectOrFunctionCallSymbol,
    Constraint,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import GetIirKindOfNode, GetNameOfNode
from pyGHDL.dom.Common import DOMException

__all__ = []


@export
class EntitySymbol(VHDLModel_EntitySymbol):
    def __init__(self, entityName: str):
        super().__init__(entityName)


@export
class EnumerationLiteralSymbol(VHDLModel_EnumerationLiteralSymbol):
    def __init__(self, literalName: str):
        super().__init__(symbolName=literalName)


@export
class SimpleSubTypeSymbol(VHDLModel_SimpleSubTypeSymbol):
    def __init__(self, subTypeName: str):
        super().__init__(subTypeName=subTypeName)

    @classmethod
    def parse(cls, node):
        pass


@export
class ConstrainedSubTypeSymbol(VHDLModel_ConstrainedSubTypeSymbol):
    def __init__(self, subTypeName: str, constraints: List[Constraint] = None):
        super().__init__(subTypeName=subTypeName, constraints=constraints)

    @classmethod
    def parse(cls, node):
        pass


@export
class SimpleObjectOrFunctionCallSymbol(VHDLModel_SimpleObjectOrFunctionCallSymbol):
    @classmethod
    def parse(cls, node):
        name = GetNameOfNode(node)
        return cls(name)


@export
class IndexedObjectOrFunctionCallSymbol(VHDLModel_IndexedObjectOrFunctionCallSymbol):
    def __init__(self, name: str, associations: List):
        super().__init__(objectName=name)

    @classmethod
    def parse(cls, node):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        prefix = nodes.Get_Prefix(node)
        name = GetNameOfNode(prefix)

        associations = []
        for item in utils.chain_iter(nodes.Get_Association_Chain(node)):
            kind = GetIirKindOfNode(item)

            if kind == nodes.Iir_Kind.Association_Element_By_Expression:
                expr = None  # GetExpressionFromNode(nodes.Get_Associated_Expr(item))

                associations.append(expr)
            else:
                raise DOMException(
                    "Unknown association kind '{kindName}'({kind}) in array index/slice or function call '{node}'.".format(
                        kind=kind, kindName=kind.name, node=node
                    )
                )

        return cls(name, associations)
