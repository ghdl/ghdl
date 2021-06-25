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
from pyGHDL.libghdl import utils

from pyGHDL.dom.Symbol import SimpleSubTypeSymbol
from pyGHDL.dom._Translate import GetNameFromNode
from pyGHDL.libghdl.vhdl import nodes

from pyGHDL.libghdl._types import Iir
from pydecor import export

from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
from pyVHDLModel.VHDLModel import (
    Attribute as VHDLModel_Attribute,
    AttributeSpecification as VHDLModel_AttributeSpecification,
)


@export
class Attribute(VHDLModel_Attribute):
    @classmethod
    def parse(cls, node: Iir) -> "Attribute":
        name = GetNameOfNode(node)
        subTypeMark = nodes.Get_Type_Mark(node)
        subTypeName = GetNameOfNode(subTypeMark)

        subType = SimpleSubTypeSymbol(subTypeName)
        return cls(name, subType)


@export
class AttributeSpecification(VHDLModel_AttributeSpecification):
    @classmethod
    def parse(cls, node: Iir) -> "AttributeSpecification":
        attributeDesignator = nodes.Get_Attribute_Designator(node)
        attributeName = GetNameFromNode(attributeDesignator)

        # FIXME: needs an implementation
        entityNameList = nodes.Get_Entity_Name_List(node)
        enlk = GetIirKindOfNode(entityNameList)

        entityClass = nodes.Get_Entity_Class(node)
        eck = GetIirKindOfNode(entityClass)

        return cls(attributeName)
