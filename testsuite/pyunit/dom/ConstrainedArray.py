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
# Testsuite:        Check libghdl IIR translation of constrained array types.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2026 Tristan Gingold
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
from unittest import TestCase

from pyVHDLModel.Formatting.VHDL import VHDLWriter

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Expression import AscendingRangeExpression, DescendingRangeExpression
from pyGHDL.dom.Name import SimpleName
from pyGHDL.dom.Symbol import ConstrainedArraySubtypeSymbol, ConstrainedScalarSubtypeSymbol, SimpleSubtypeSymbol
from pyGHDL.dom.Type import ArrayType


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ConstrainedArrayTypes(TestCase):
    """
    Translation of constrained array type declarations, which GHDL reports as an anonymous type declaration
    holding an ``Array_Subtype_Definition``. An index constraint is a range, a type mark, or a type mark with a
    range constraint.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/ConstrainedArray.vhdl"

    @staticmethod
    def _package():
        design = Design()
        document = Document(ConstrainedArrayTypes._filename)
        design.Documents.append(document)
        return document.Packages["constrainedarray"]

    def test_Range(self) -> None:
        """``type ranged_t is array (0 to 3) of bit;``"""
        arrayType = self._package().DeclaredItems[1]

        self.assertIsInstance(arrayType, ArrayType)
        self.assertEqual("ranged_t", arrayType.Identifier)
        self.assertEqual(1, len(arrayType.Dimensions))
        self.assertIsInstance(arrayType.Dimensions[0], AscendingRangeExpression)
        self.assertIsInstance(arrayType.ElementType, SimpleSubtypeSymbol)
        self.assertEqual("type ranged_t is array (0 to 3) of bit;\n", VHDLWriter().Format(arrayType))

    def test_TypeMark(self) -> None:
        """``type enumerated_t is array (state_t) of integer;`` - the index is constrained by a type mark, no box."""
        arrayType = self._package().DeclaredItems[2]

        self.assertIsInstance(arrayType, ArrayType)
        self.assertEqual(1, len(arrayType.Dimensions))
        self.assertIsInstance(arrayType.Dimensions[0], SimpleName)
        self.assertEqual("state_t", arrayType.Dimensions[0].Identifier)
        self.assertIsInstance(arrayType.ElementType, SimpleSubtypeSymbol)
        self.assertEqual("type enumerated_t is array (state_t) of integer;\n", VHDLWriter().Format(arrayType))

    def test_TwoDimensions(self) -> None:
        """``type matrix_t is array (7 downto 0, state_t) of bit_vector(1 downto 0);`` - two dimensions and a
        constrained element subtype."""
        arrayType = self._package().DeclaredItems[3]

        self.assertIsInstance(arrayType, ArrayType)
        self.assertEqual(2, len(arrayType.Dimensions))
        self.assertIsInstance(arrayType.Dimensions[0], DescendingRangeExpression)
        self.assertIsInstance(arrayType.Dimensions[1], SimpleName)
        self.assertIsInstance(arrayType.ElementType, ConstrainedArraySubtypeSymbol)
        self.assertEqual(
            "type matrix_t is array (7 downto 0, state_t) of bit_vector(1 downto 0);\n",
            VHDLWriter().Format(arrayType),
        )

    def test_SubtypeIndication(self) -> None:
        """``type subtyped_t is array (natural range 0 to 7) of bit;`` - the index is a subtype indication with a
        range constraint, which GHDL reports as a ``Subtype_Definition``."""
        arrayType = self._package().DeclaredItems[4]

        self.assertIsInstance(arrayType, ArrayType)
        self.assertEqual(1, len(arrayType.Dimensions))
        self.assertIsInstance(arrayType.Dimensions[0], ConstrainedScalarSubtypeSymbol)
        self.assertIsInstance(arrayType.ElementType, SimpleSubtypeSymbol)
        self.assertEqual("type subtyped_t is array (natural range 0 to 7) of bit;\n", VHDLWriter().Format(arrayType))
