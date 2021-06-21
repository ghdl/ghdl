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
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyGHDL.libghdl import name_table

from pyGHDL.libghdl.vhdl import nodes
from pydecor import export

from pyVHDLModel.VHDLModel import (
    IntegerLiteral as VHDLModel_IntegerLiteral,
    FloatingPointLiteral as VHDLModel_FloatingPointLiteral,
    CharacterLiteral as VHDLModel_CharacterLiteral,
    StringLiteral as VHDLModel_StringLiteral,
)

__all__ = []


@export
class IntegerLiteral(VHDLModel_IntegerLiteral):
    @classmethod
    def parse(cls, node):
        value = nodes.Get_Value(node)
        return cls(value)


@export
class FloatingPointLiteral(VHDLModel_FloatingPointLiteral):
    @classmethod
    def parse(cls, node):
        value = nodes.Get_Fp_Value(node)
        return cls(value)


@export
class CharacterLiteral(VHDLModel_CharacterLiteral):
    @classmethod
    def parse(cls, node):
        identifier = nodes.Get_Identifier(node)
        value = name_table.Get_Character(identifier)
        return cls(value)


@export
class StringLiteral(VHDLModel_StringLiteral):
    @classmethod
    def parse(cls, node):
        stringID = nodes.Get_String8_Id(node)
        value = name_table.Get_Name_Ptr(stringID)
        return cls(value)
