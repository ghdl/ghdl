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
"""
This module implements derived literal classes from :mod:`pyVHDLModel.Expression`.
"""

from typing import Union, Optional as Nullable

from pyTooling.Decorators import export, InheritDocString, DocStringMergeStrategy
from pyTooling.Warning import WarningCollector

from pyVHDLModel.Expression import NullLiteral as VHDLModel_NullLiteral
from pyVHDLModel.Expression import EnumerationLiteral as VHDLModel_EnumerationLiteral
from pyVHDLModel.Expression import IntegerLiteral as VHDLModel_IntegerLiteral
from pyVHDLModel.Expression import FloatingPointLiteral as VHDLModel_FloatingPointLiteral
from pyVHDLModel.Expression import PhysicalIntegerLiteral as VHDLModel_PhysicalIntegerLiteral
from pyVHDLModel.Expression import PhysicalFloatingLiteral as VHDLModel_PhysicalFloatingLiteral
from pyVHDLModel.Expression import CharacterLiteral as VHDLModel_CharacterLiteral
from pyVHDLModel.Expression import StringLiteral as VHDLModel_StringLiteral
from pyVHDLModel.Expression import BinaryBitStringLiteral as VHDLModel_BinaryBitStringLiteral
from pyVHDLModel.Expression import OctalBitStringLiteral as VHDLModel_OctalBitStringLiteral
from pyVHDLModel.Expression import DecimalBitStringLiteral as VHDLModel_DecimalBitStringLiteral
from pyVHDLModel.Expression import HexadecimalBitStringLiteral as VHDLModel_HexadecimalBitStringLiteral

from pyGHDL.libghdl import ENCODING, name_table, str_table, files_map
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes, utils
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom._Utils import GetNameOfNode


@export
@InheritDocString(VHDLModel_NullLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class NullLiteral(VHDLModel_NullLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.NullLiteral`.
    """

    def __init__(self, node: Iir) -> None:
        """
        Initializes a :vhdlkw:`null` literal.

        :param node: The IIR node this object was translated from.
        """
        super().__init__()
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "NullLiteral":
        """
        Translates an IIR node to a :class:`NullLiteral`.

        :param node: The IIR node this object is translated from.
        :returns:    The translated object.
        """
        return cls(node)


@export
@InheritDocString(VHDLModel_EnumerationLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class EnumerationLiteral(VHDLModel_EnumerationLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.EnumerationLiteral`.
    """

    def __init__(self, node: Iir, value: str) -> None:
        """
        Initializes an enumeration literal.

        :param node:  The IIR node this object was translated from.
        :param value: The enumeration literal's name.
        """
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "EnumerationLiteral":
        """
        Translates the IIR node of the literal to an :class:`EnumerationLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        literalName = GetNameOfNode(literalNode)
        return cls(literalNode, literalName)


@export
@InheritDocString(VHDLModel_IntegerLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class IntegerLiteral(VHDLModel_IntegerLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.IntegerLiteral`.
    """

    def __init__(self, node: Iir, value: int) -> None:
        """
        Initializes an integer literal.

        :param node:  The IIR node this object was translated from.
        :param value: The literal's integer value.
        """
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "IntegerLiteral":
        """
        Translates the IIR node of the literal to an :class:`IntegerLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        value = nodes.Get_Value(literalNode)
        return cls(literalNode, value)


@export
@InheritDocString(VHDLModel_FloatingPointLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class FloatingPointLiteral(VHDLModel_FloatingPointLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.FloatingPointLiteral`.
    """

    def __init__(self, node: Iir, value: float) -> None:
        """
        Initializes a floating-point literal.

        :param node:  The IIR node this object was translated from.
        :param value: The literal's floating-point value.
        """
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "FloatingPointLiteral":
        """
        Translates the IIR node of the literal to a :class:`FloatingPointLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        value = nodes.Get_Fp_Value(literalNode)
        return cls(literalNode, value)


@export
@InheritDocString(VHDLModel_PhysicalIntegerLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class PhysicalIntegerLiteral(VHDLModel_PhysicalIntegerLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.PhysicalIntegerLiteral`.
    """

    def __init__(self, node: Iir, value: int, unitName: str) -> None:
        """
        Initializes a physical literal with an integer value.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal's integer value, in units of :attr:`_unitName`.
        :param unitName: The name of the physical unit the value is given in.
        """
        super().__init__(value, unitName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "PhysicalIntegerLiteral":
        """
        Translates the IIR node of the literal to a :class:`PhysicalIntegerLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        value = nodes.Get_Value(literalNode)
        unit = nodes.Get_Unit_Name(literalNode)
        unitName = GetNameOfNode(unit)

        return cls(literalNode, value, unitName)


@export
@InheritDocString(VHDLModel_PhysicalFloatingLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class PhysicalFloatingLiteral(VHDLModel_PhysicalFloatingLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.PhysicalFloatingLiteral`.
    """

    def __init__(self, node: Iir, value: int, unitName: float) -> None:
        """
        Initializes a physical literal with a floating-point value.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal's floating-point value, in units of :attr:`_unitName`.
        :param unitName: The name of the physical unit the value is given in.
        """
        super().__init__(value, unitName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "PhysicalFloatingLiteral":
        """
        Translates the IIR node of the literal to a :class:`PhysicalFloatingLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        value = nodes.Get_Fp_Value(literalNode)
        unit = nodes.Get_Unit_Name(literalNode)
        unitName = GetNameOfNode(unit)

        return cls(literalNode, value, unitName)


@export
@InheritDocString(VHDLModel_CharacterLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class CharacterLiteral(VHDLModel_CharacterLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.CharacterLiteral`.
    """

    def __init__(self, node: Iir, value: str) -> None:
        """
        Initializes a character literal.

        :param node:  The IIR node this object was translated from.
        :param value: The literal's character value.
        """
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> "CharacterLiteral":
        """
        Translates the IIR node of the literal to a :class:`CharacterLiteral`.

        :param literalNode: The IIR node of the literal.
        :returns:           The translated literal.
        """
        identifier = nodes.Get_Identifier(literalNode)
        value = name_table.Get_Character(identifier)
        return cls(literalNode, value)


@export
@InheritDocString(VHDLModel_BinaryBitStringLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class BinaryBitStringLiteral(VHDLModel_BinaryBitStringLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.BinaryBitStringLiteral`.
    """

    def __init__(self, node: Iir, value: str, length: Nullable[int] = None, isSigned: Nullable[bool] = None) -> None:
        """
        Initializes a binary bit string literal.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal as written in the source, without the enclosing double quotes.
        :param length:   The explicitly given length, or ``None`` if the literal has no length specification.
        :param isSigned: ``True`` if signed, ``False`` if unsigned, ``None`` if unspecified.
        """
        super().__init__(value, length, isSigned)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_OctalBitStringLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class OctalBitStringLiteral(VHDLModel_OctalBitStringLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.OctalBitStringLiteral`.
    """

    def __init__(self, node: Iir, value: str, length: Nullable[int] = None, isSigned: Nullable[bool] = None) -> None:
        """
        Initializes an octal bit string literal.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal as written in the source, without the enclosing double quotes.
        :param length:   The explicitly given length, or ``None`` if the literal has no length specification.
        :param isSigned: ``True`` if signed, ``False`` if unsigned, ``None`` if unspecified.
        """
        super().__init__(value, length, isSigned)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_DecimalBitStringLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class DecimalBitStringLiteral(VHDLModel_DecimalBitStringLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.DecimalBitStringLiteral`.
    """

    def __init__(self, node: Iir, value: str, length: Nullable[int] = None, isSigned: Nullable[bool] = None) -> None:
        """
        Initializes a decimal bit string literal.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal as written in the source, without the enclosing double quotes.
        :param length:   The explicitly given length, or ``None`` if the literal has no length specification.
        :param isSigned: ``True`` if signed, ``False`` if unsigned, ``None`` if unspecified.
        """
        super().__init__(value, length, isSigned)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_HexadecimalBitStringLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class HexadecimalBitStringLiteral(VHDLModel_HexadecimalBitStringLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.HexadecimalBitStringLiteral`.
    """

    def __init__(self, node: Iir, value: str, length: Nullable[int] = None, isSigned: Nullable[bool] = None) -> None:
        """
        Initializes a hexadecimal bit string literal.

        :param node:     The IIR node this object was translated from.
        :param value:    The literal as written in the source, without the enclosing double quotes.
        :param length:   The explicitly given length, or ``None`` if the literal has no length specification.
        :param isSigned: ``True`` if signed, ``False`` if unsigned, ``None`` if unspecified.
        """
        super().__init__(value, length, isSigned)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_StringLiteral, strategy=DocStringMergeStrategy.BaseFirst)
class StringLiteral(VHDLModel_StringLiteral, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Expression.StringLiteral`.
    """

    def __init__(self, node: Iir, value: str) -> None:
        """
        Initializes a string literal.

        :param node:  The IIR node this object was translated from.
        :param value: The literal's string value, without the enclosing double quotes.
        """
        super().__init__(value)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, literalNode: Iir) -> Union[
        "StringLiteral",
        BinaryBitStringLiteral,
        OctalBitStringLiteral,
        DecimalBitStringLiteral,
        HexadecimalBitStringLiteral,
    ]:
        """
        Translates the IIR node of the literal to a :class:`StringLiteral`.

        A bit string literal's value is the literal as written in the source, without length, signedness, base
        specifier and delimiters (``x"A_5"`` has the value ``A_5``).

        :param literalNode:                  The IIR node of the literal.
        :returns:                            The translated literal.
        :raises DOMException:                If a bit string literal has no source text.
        :raises UnhandledExceptionException: If the literal's base is not handled and no
                                             :class:`~pyTooling.Warning.WarningCollector` is active.
        """
        base = nodes.Get_Bit_String_Base(literalNode)
        isSigned = nodes.Get_Has_Signed(literalNode) if nodes.Get_Has_Sign(literalNode) else None
        length = nodes.Get_String_Length(literalNode) if nodes.Get_Has_Length(literalNode) else None

        # sourceIdentifier = utils.Get_Source_Identifier(literalNode)
        # test = name_table.Get_Name_Ptr(sourceIdentifier)

        if base is nodes.NumberBaseType.Base_None:
            value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
            return cls(literalNode, value)

        # GHDL's String8 of a bit string literal holds the *expanded* bit value: one character per bit, including the
        # padding or sign extension to an explicit length (x"A5" is stored as "10100101"). pyVHDLModel's
        # BitStringLiteral.Value is the literal *as written* (digits, letters and underscores between the quotes), and
        # the class chosen by the base tells how to read it. Passing the expansion as the value of an octal, decimal or
        # hexadecimal literal changed the literal: x"A5" became x"10100101" when written back.
        # Therefore, the value is cut from the source text: the node's location and the literal length (the number of
        # characters the scanner read, Get_Literal_Length) select the literal, the value is what stands between its
        # quotes. Length and signedness are taken from the node as before.
        #
        # Original code, for comparison (each branch passed the expansion as the value):
        #
        # elif base is nodes.NumberBaseType.Base_2:
        #     value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
        #     return BinaryBitStringLiteral(literalNode, value, length, isSigned)
        # elif base is nodes.NumberBaseType.Base_8:
        #     value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
        #     return OctalBitStringLiteral(literalNode, value, length, isSigned)
        # elif base is nodes.NumberBaseType.Base_10:
        #     value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
        #     return DecimalBitStringLiteral(literalNode, value, length, isSigned)
        # elif base is nodes.NumberBaseType.Base_16:
        #     value = str_table.Get_String8_Ptr(nodes.Get_String8_Id(literalNode), nodes.Get_String_Length(literalNode))
        #     return HexadecimalBitStringLiteral(literalNode, value, length, isSigned)
        literalLength = nodes.Get_Literal_Length(literalNode)
        if literalLength == 0:
            position = Position.parse(literalNode)
            raise DOMException(f"Bit string literal at {position} has no source text.")

        location = nodes.Get_Location(literalNode)
        file = files_map.Location_To_File(location)
        start = files_map.Location_File_To_Pos(location, file)
        sourceText = files_map.Get_File_Buffer(file)[start : start + literalLength].decode(ENCODING)
        value = sourceText[sourceText.index('"') + 1 : -1]

        if base is nodes.NumberBaseType.Base_2:
            return BinaryBitStringLiteral(literalNode, value, length, isSigned)
        elif base is nodes.NumberBaseType.Base_8:
            return OctalBitStringLiteral(literalNode, value, length, isSigned)
        elif base is nodes.NumberBaseType.Base_10:
            return DecimalBitStringLiteral(literalNode, value, length, isSigned)
        elif base is nodes.NumberBaseType.Base_16:
            return HexadecimalBitStringLiteral(literalNode, value, length, isSigned)
        else:
            WarningCollector.Raise(NotImplementedError("Bit String Literal not supported yet"))
