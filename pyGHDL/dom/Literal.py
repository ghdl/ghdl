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
# Package module:   DOM: Literals.
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
from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    NullLiteral as VHDLModel_NullLiteral,
    EnumerationLiteral as VHDLModel_EnumerationLiteral,
    IntegerLiteral as VHDLModel_IntegerLiteral,
    FloatingPointLiteral as VHDLModel_FloatingPointLiteral,
    PhysicalIntegerLiteral as VHDLModel_PhysicalIntegerLiteral,
    PhysicalFloatingLiteral as VHDLModel_PhysicalFloatingLiteral,
    CharacterLiteral as VHDLModel_CharacterLiteral,
    StringLiteral as VHDLModel_StringLiteral,
)
from pyGHDL.libghdl import name_table, str_table
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode

__all__ = []


@export
class NullLiteral(VHDLModel_NullLiteral, DOMMixin):
    def __init__(self, node: Iir):
        super().__init__()
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "NullLiteral":
        return cls(node)


@export
class EnumerationLiteral(VHDLModel_EnumerationLiteral, DOMMixin):
    def __init__(self, node: Iir, value: str):
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "EnumerationLiteral":
        literalName = GetNameOfNode(literalNode)
        return cls(literalNode, literalName)


@export
class IntegerLiteral(VHDLModel_IntegerLiteral, DOMMixin):
    def __init__(self, node: Iir, value: int):
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "IntegerLiteral":
        value = nodes.Get_Value(literalNode)
        return cls(literalNode, value)


@export
class FloatingPointLiteral(VHDLModel_FloatingPointLiteral, DOMMixin):
    def __init__(self, node: Iir, value: float):
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "FloatingPointLiteral":
        value = nodes.Get_Fp_Value(literalNode)
        return cls(literalNode, value)


@export
class PhysicalIntegerLiteral(VHDLModel_PhysicalIntegerLiteral, DOMMixin):
    def __init__(self, node: Iir, value: int, unitName: str):
        super().__init__(value, unitName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "PhysicalIntegerLiteral":
        value = nodes.Get_Value(literalNode)
        unit = nodes.Get_Unit_Name(literalNode)
        unitName = GetNameOfNode(unit)

        return cls(literalNode, value, unitName)


@export
class PhysicalFloatingLiteral(VHDLModel_PhysicalFloatingLiteral, DOMMixin):
    def __init__(self, node: Iir, value: int, unitName: float):
        super().__init__(value, unitName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "PhysicalFloatingLiteral":
        value = nodes.Get_Fp_Value(literalNode)
        unit = nodes.Get_Unit_Name(literalNode)
        unitName = GetNameOfNode(unit)

        return cls(literalNode, value, unitName)


@export
class CharacterLiteral(VHDLModel_CharacterLiteral, DOMMixin):
    def __init__(self, node: Iir, value: str):
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "CharacterLiteral":
        identifier = nodes.Get_Identifier(literalNode)
        value = name_table.Get_Character(identifier)
        return cls(literalNode, value)


@export
class StringLiteral(VHDLModel_StringLiteral, DOMMixin):
    def __init__(self, node: Iir, value: str):
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "StringLiteral":
        if nodes.Get_Bit_String_Base(literalNode) is nodes.NumberBaseType.Base_None:
            value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
            return cls(literalNode, value)
        else:
            print("[NOT IMPLEMENTED] Bit String Literal not supported yet")
