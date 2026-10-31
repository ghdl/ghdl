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
# Testsuite:        Check libghdl IIR translation to DOM for literals.
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
from pathlib import Path
from textwrap import dedent
from types import SimpleNamespace
from typing import TypeVar, Dict
from unittest import TestCase
from unittest.mock import patch

from pyTooling.Common import firstValue

from pyVHDLModel.Base import ExpressionUnion

from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMException
from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Object import Constant
from pyGHDL.dom.Literal import IntegerLiteral
from pyGHDL.dom.DesignUnit import Package


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Literals(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _design = Design()
    _packageTemplate = dedent(
            """\
            package package_1 is
              {code}
            end package;
            """
        )

    def parse(self, filename: Path, code: str) -> ExpressionUnion:
        sourceCode = self._packageTemplate.format(code=code)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        # Traverse already to default value expression
        package: Package = firstValue(document.Packages)
        item: Constant = package.DeclaredItems[0]
        default: ExpressionUnion = item.DefaultExpression

        return default

    def test_IntegerLiteral(self):
        _filename: Path = self._root / "{className}.vhdl".format(
            className=self.__class__.__name__
        )

        constantDeclartion = "constant c0 : integer := 0;"
        expected = (0, 1, 1024, 1048576)

        # Parse in-memory
        default: ExpressionUnion = self.parse(_filename, constantDeclartion)

        self.assertIsInstance(default, IntegerLiteral)
        self.assertEqual(expected[0], default.Value)

    def test_BitStringLiteral_PlainHex(self):
        """``x"F"`` - no sign, no explicit length. Regression test: Signed/Length were computed from
        the IIR node but never forwarded to the constructed literal object - always silently None."""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_PlainHex.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector(3 downto 0) := x"F";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertIsNone(default.IsSigned)
        self.assertIsNone(default.Length)

    def test_BitStringLiteral_Signed(self):
        """``sx"F"``"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_Signed.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector(7 downto 0) := sx"F";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertTrue(default.IsSigned)

    def test_BitStringLiteral_Unsigned(self):
        """``ux"F"``"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_Unsigned.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector(7 downto 0) := ux"F";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertFalse(default.IsSigned)

    def test_BitStringLiteral_ExplicitLength(self):
        """``4x"F"``"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_ExplicitLength.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector(3 downto 0) := 4x"F";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertEqual(4, default.Length)

    def test_BitStringLiteral_ValueAsWritten_Binary(self):
        """``b"1010"`` - the value is the literal as written, not GHDL's expansion to base 2."""
        from pyGHDL.dom.Literal import BinaryBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenBinary.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := b"1010";')

        self.assertIsInstance(default, BinaryBitStringLiteral)
        self.assertEqual("1010", default.Value)
        self.assertIsNone(default.Length)
        self.assertIsNone(default.IsSigned)
        self.assertEqual('b"1010"', str(default))

    def test_BitStringLiteral_ValueAsWritten_Octal(self):
        """``o"17"`` (expanded: ``001111``)"""
        from pyGHDL.dom.Literal import OctalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenOctal.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := o"17";')

        self.assertIsInstance(default, OctalBitStringLiteral)
        self.assertEqual("17", default.Value)
        self.assertEqual('o"17"', str(default))

    def test_BitStringLiteral_ValueAsWritten_Hexadecimal(self):
        """``x"A5"`` (expanded: ``10100101``)"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenHex.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := x"A5";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertEqual("A5", default.Value)
        self.assertEqual('x"A5"', str(default))

    def test_BitStringLiteral_ValueAsWritten_Length(self):
        """``8x"A5"``"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenLength.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := 8x"A5";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertEqual("A5", default.Value)
        self.assertEqual(8, default.Length)
        self.assertIsNone(default.IsSigned)
        self.assertEqual('8x"A5"', str(default))

    def test_BitStringLiteral_ValueAsWritten_SignedLength(self):
        """``12sx"F5"`` (expanded with sign extension: ``111111110101``)"""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenSigned.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := 12sx"F5";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertEqual("F5", default.Value)
        self.assertEqual(12, default.Length)
        self.assertTrue(default.IsSigned)
        self.assertEqual('12sx"F5"', str(default))

    def test_BitStringLiteral_ValueAsWritten_Decimal(self):
        """``d"160"`` (expanded: ``10100000``)"""
        from pyGHDL.dom.Literal import DecimalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenDecimal.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := d"160";')

        self.assertIsInstance(default, DecimalBitStringLiteral)
        self.assertEqual("160", default.Value)
        self.assertEqual('d"160"', str(default))

    def test_BitStringLiteral_ValueAsWritten_Underscore(self):
        """``x"A_5"`` - underscores are part of the literal as written."""
        from pyGHDL.dom.Literal import HexadecimalBitStringLiteral

        _filename: Path = self._root / "{className}_AsWrittenUnderscore.vhdl".format(className=self.__class__.__name__)
        default: ExpressionUnion = self.parse(_filename, 'constant c0 : bit_vector := x"A_5";')

        self.assertIsInstance(default, HexadecimalBitStringLiteral)
        self.assertEqual("A_5", default.Value)
        self.assertEqual('x"A_5"', str(default))

    def test_StringLiteral_UnknownBase(self):
        """A literal with a base not handled by ``StringLiteral.parse`` raises :exc:`DOMException`."""
        _filename: Path = self._root / "{className}_UnknownBase.vhdl".format(className=self.__class__.__name__)
        sourceCode = self._packageTemplate.format(code='constant c0 : string := "F";')

        document = Document(_filename, sourceCode, dontTranslate=True)
        self._design.Documents.append(document)

        unknownBase = SimpleNamespace(name="Base_Unknown")
        with patch.object(nodes, "Get_Bit_String_Base", return_value=unknownBase):
            with self.assertRaises(DOMException):
                document.translate()
