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
    Attribute as VHDLModel_Attribute,
    AttributeSpecification as VHDLModel_AttributeSpecification,
    Name,
    SubTypeOrSymbol,
)
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
from pyGHDL.dom._Translate import GetNameFromNode
from pyGHDL.dom.Symbol import SimpleSubTypeSymbol


@export
class Attribute(VHDLModel_Attribute, DOMMixin):
    def __init__(self, node: Iir, identifier: str, subType: SubTypeOrSymbol):
        super().__init__(identifier, subType)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, attributeNode: Iir) -> "Attribute":
        name = GetNameOfNode(attributeNode)
        subTypeMark = nodes.Get_Type_Mark(attributeNode)
        subTypeName = GetNameOfNode(subTypeMark)

        subType = SimpleSubTypeSymbol(subTypeMark, subTypeName)
        return cls(attributeNode, name, subType)


@export
class AttributeSpecification(VHDLModel_AttributeSpecification, DOMMixin):
    def __init__(self, node: Iir, attribute: Name):
        super().__init__(attribute)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, attributeNode: Iir) -> "AttributeSpecification":
        attributeDesignator = nodes.Get_Attribute_Designator(attributeNode)
        attributeName = GetNameFromNode(attributeDesignator)

        # FIXME: needs an implementation
        entityNameList = nodes.Get_Entity_Name_List(attributeNode)
        enlk = GetIirKindOfNode(entityNameList)

        entityClass = nodes.Get_Entity_Class(attributeNode)
        eck = GetIirKindOfNode(entityClass)

        return cls(attributeNode, attributeName)
