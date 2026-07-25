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
# Testsuite:        Check libghdl IIR translation of discrete ranges.
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

from pyVHDLModel.Base import Range
from pyVHDLModel.Name import AttributeName, SimpleName

from pyGHDL.dom.Concurrent import ForGenerateStatement
from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Sequential import ForLoopStatement


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class DiscreteRanges(TestCase):
    """
    Regression tests for the shared discrete range translation.

    A ``discrete_range`` is either a range or a discrete subtype indication. Only the range forms were
    translated before, so ``for i in integer range 0 to 3 loop`` (``Subtype_Definition``) and
    ``for i in bit loop`` (``Simple_Name``) raised a ``DOMException`` instead - crashing on real sources
    such as ``libraries/synopsys/std_logic_arith.vhdl``.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/DiscreteRanges.vhdl"

    def setUp(self) -> None:
        design = Design()
        self._document = Document(self._filename)
        design.Documents.append(self._document)

        architecture = self._document.Architectures["discreteranges"]["rtl"]
        self._generate = architecture.Statements[0]
        self._statements = architecture.Statements[1].Statements

    def test_ForGenerateWithSubtypeIndication(self) -> None:
        self.assertIsInstance(self._generate, ForGenerateStatement)
        self.assertIsInstance(self._generate.Range, Range)

    def test_ForLoopWithConstrainedSubtypeIndication(self) -> None:
        loop = self._statements[0]

        self.assertIsInstance(loop, ForLoopStatement)
        # The range constraint of `integer range 0 to 3` carries the iteration bounds.
        self.assertIsInstance(loop.Range, Range)
        self.assertIs(loop.Range.Parent, loop)

    def test_ForLoopWithTypeMark(self) -> None:
        loop = self._statements[1]

        self.assertIsInstance(loop, ForLoopStatement)
        # `bit` has no range constraint of its own, so the type mark's name is kept.
        self.assertIsInstance(loop.Range, SimpleName)

    def test_ForLoopWithRangeAttribute(self) -> None:
        loop = self._statements[2]

        self.assertIsInstance(loop, ForLoopStatement)
        self.assertIsInstance(loop.Range, AttributeName)

    def test_ForLoopWithRangeExpression(self) -> None:
        loop = self._statements[3]

        self.assertIsInstance(loop, ForLoopStatement)
        self.assertIsInstance(loop.Range, Range)
