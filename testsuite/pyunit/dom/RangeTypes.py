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
# Testsuite:        Check libghdl IIR translation of integer and real type declarations.
#
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
from inspect import currentframe
from pathlib import Path
from textwrap import dedent
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Type import IntegerType, RealType

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class RangeTypes(TestCase):
    """
    ``type t is range 0.0 to 1.0;`` was translated to an ``IntegerType``. Without semantic analysis, a floating-point
    literal in a bound makes it a ``RealType``; bounds without any literal (``range LOW to HIGH``) stay an
    ``IntegerType``.
    """

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()

    def parse(self, declarations: str) -> list:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"
        sourceCode = dedent("""\
            package p is
            {declarations}
            end package;
            """).format(declarations=declarations)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        return document.Packages["p"].DeclaredItems

    def test_IntegerType(self) -> None:
        integerType, expressionBounds = self.parse(
            "\n".join(
                [
                    "  type t is range 0 to 7;",
                    "  type u is range -2**31 to 2**31 - 1;",
                ]
            )
        )

        self.assertIsInstance(integerType, IntegerType)
        self.assertEqual("0 to 7", str(integerType.Range))
        self.assertIsInstance(expressionBounds, IntegerType)

    def test_RealType(self) -> None:
        (realType,) = self.parse("  type t is range 0.0 to 1.0;")

        self.assertIsInstance(realType, RealType)
        self.assertEqual("t", realType.Identifier)
        self.assertEqual("0.0 to 1.0", str(realType.Range))

    def test_RealTypeExpressionBounds(self) -> None:
        negated, downto = self.parse(
            "\n".join(
                [
                    "  type t is range -1.0E38 to 1.0E38;",
                    "  type u is range 1.0 downto -(2.5 * 2.0);",
                ]
            )
        )

        self.assertIsInstance(negated, RealType)
        self.assertIsInstance(downto, RealType)

    def test_RealTypeNamedBound(self) -> None:
        _, realType, namesOnly = self.parse(
            "\n".join(
                [
                    "  constant LOW : real := 0.0;",
                    "  type t is range LOW to 1.0;",
                    "  type u is range LOW to LOW;",
                ]
            )
        )

        self.assertIsInstance(realType, RealType)
        self.assertIsInstance(namesOnly, IntegerType)
