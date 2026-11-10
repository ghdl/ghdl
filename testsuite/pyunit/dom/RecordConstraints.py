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
# Testsuite:        Check libghdl IIR translation of record element constraints.
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

from pyVHDLModel.Formatting.VHDL import VHDLWriter

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Expression import AscendingRangeExpression, DescendingRangeExpression
from pyGHDL.dom.Name import AttributeName
from pyGHDL.dom.Object import Signal
from pyGHDL.dom.Symbol import (
    ArrayElementConstraint,
    ConstrainedRecordSubtypeSymbol,
    RecordElementConstraint,
    RecordElementSymbol,
)
from pyGHDL.dom.Type import Subtype

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class RecordConstraints(TestCase):
    """
    A record constraint maps each constrained record element to its element constraint: an ``ArrayElementConstraint``
    with one range per dimension, or a nested ``RecordElementConstraint``.
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

    def test_ArrayElement(self) -> None:
        """Example of ghdl/ghdl#2902: the array element of a record constrained by a range with named bounds."""
        _, _, _, signal = self.parse(
            "\n".join(
                [
                    "  type reg_state_t is record",
                    "    data  : bit_vector;",
                    "    valid : bit;",
                    "  end record;",
                    "  constant lefty  : natural := 7;",
                    "  constant righty : natural := 0;",
                    "  signal reg_state : reg_state_t(data(lefty downto righty));",
                ]
            )
        )

        self.assertIsInstance(signal, Signal)
        symbol = signal.Subtype
        self.assertIsInstance(symbol, ConstrainedRecordSubtypeSymbol)
        self.assertEqual("reg_state_t", symbol.Name.Identifier)
        self.assertEqual(1, len(symbol.Constraints))

        ((element, constraint),) = symbol.Constraints.items()
        self.assertIsInstance(element, RecordElementSymbol)
        self.assertEqual("data", element.Name.Identifier)
        self.assertIs(symbol, element.Parent)
        self.assertIsInstance(constraint, ArrayElementConstraint)
        self.assertIs(symbol, constraint.Parent)
        self.assertEqual(1, len(constraint.Constraints))
        self.assertIsInstance(constraint.Constraints[0], DescendingRangeExpression)
        self.assertIs(constraint, constraint.Constraints[0].Parent)
        self.assertEqual(
            "signal reg_state : reg_state_t(data(lefty downto righty));\n",
            VHDLWriter().Format(signal),
        )

    def test_RangeAttribute(self) -> None:
        """The array element of a record constrained by a range attribute: ``data(vec'range)``."""
        _, vector, signal = self.parse(
            "\n".join(
                [
                    "  type reg_state_t is record",
                    "    data  : bit_vector;",
                    "    valid : bit;",
                    "  end record;",
                    "  signal vec : bit_vector(15 downto 0);",
                    "  signal reg_state : reg_state_t(data(vec'range));",
                ]
            )
        )

        self.assertIsInstance(vector, Signal)
        symbol = signal.Subtype
        self.assertIsInstance(symbol, ConstrainedRecordSubtypeSymbol)

        ((element, constraint),) = symbol.Constraints.items()
        self.assertEqual("data", element.Name.Identifier)
        self.assertIsInstance(constraint, ArrayElementConstraint)
        self.assertEqual(1, len(constraint.Constraints))
        self.assertIsInstance(constraint.Constraints[0], AttributeName)
        self.assertEqual("signal reg_state : reg_state_t(data(vec'range));\n", VHDLWriter().Format(signal))

    def test_NestedRecord(self) -> None:
        """The record element of a record is constrained by a nested record constraint: ``inner(field(3 downto 0))``."""
        _, _, signal = self.parse(
            "\n".join(
                [
                    "  type inner_t is record",
                    "    field : bit_vector;",
                    "  end record;",
                    "  type outer_t is record",
                    "    inner : inner_t;",
                    "  end record;",
                    "  signal nested : outer_t(inner(field(3 downto 0)));",
                ]
            )
        )

        symbol = signal.Subtype
        self.assertIsInstance(symbol, ConstrainedRecordSubtypeSymbol)
        self.assertEqual("outer_t", symbol.Name.Identifier)

        ((element, constraint),) = symbol.Constraints.items()
        self.assertEqual("inner", element.Name.Identifier)
        self.assertIsInstance(constraint, RecordElementConstraint)
        self.assertIs(symbol, constraint.Parent)
        self.assertEqual("(field(3 downto 0))", str(constraint))

        ((nestedElement, nestedConstraint),) = constraint.Constraints.items()
        self.assertIsInstance(nestedElement, RecordElementSymbol)
        self.assertEqual("field", nestedElement.Name.Identifier)
        self.assertIs(constraint, nestedElement.Parent)
        self.assertIsInstance(nestedConstraint, ArrayElementConstraint)
        self.assertIs(constraint, nestedConstraint.Parent)
        self.assertEqual("(3 downto 0)", str(nestedConstraint))
        self.assertEqual("signal nested : outer_t(inner(field(3 downto 0)));\n", VHDLWriter().Format(signal))

    def test_TwoDimensions(self) -> None:
        """The two-dimensional array element of a record is constrained by one range per dimension."""
        _, _, signal = self.parse(
            "\n".join(
                [
                    "  type matrix_t is array (natural range <>, natural range <>) of bit;",
                    "  type matrix_record_t is record",
                    "    m : matrix_t;",
                    "  end record;",
                    "  signal two : matrix_record_t(m(0 to 1, 0 to 7));",
                ]
            )
        )

        symbol = signal.Subtype
        self.assertIsInstance(symbol, ConstrainedRecordSubtypeSymbol)

        ((element, constraint),) = symbol.Constraints.items()
        self.assertEqual("m", element.Name.Identifier)
        self.assertIsInstance(constraint, ArrayElementConstraint)
        self.assertEqual(2, len(constraint.Constraints))
        for dimension in constraint.Constraints:
            self.assertIsInstance(dimension, AscendingRangeExpression)
            self.assertIs(constraint, dimension.Parent)
        self.assertEqual("0 to 1", str(constraint.Constraints[0]))
        self.assertEqual("0 to 7", str(constraint.Constraints[1]))
        self.assertEqual("(0 to 1, 0 to 7)", str(constraint))

    def test_SubtypeDeclaration(self) -> None:
        """A record subtype declaration constrains several elements, each by its own kind of element constraint."""
        _, _, _, subtype = self.parse(
            "\n".join(
                [
                    "  type inner_t is record",
                    "    field : bit_vector;",
                    "  end record;",
                    "  type matrix_t is array (natural range <>, natural range <>) of bit;",
                    "  type all_t is record",
                    "    inner : inner_t;",
                    "    m     : matrix_t;",
                    "    data  : bit_vector;",
                    "  end record;",
                    "  subtype all_sub is all_t(inner(field(3 downto 0)), m(0 to 1, 0 to 7), data(7 downto 0));",
                ]
            )
        )

        self.assertIsInstance(subtype, Subtype)
        self.assertEqual("all_sub", subtype.Identifier)
        symbol = subtype.Type
        self.assertIsInstance(symbol, ConstrainedRecordSubtypeSymbol)
        self.assertEqual("all_t", symbol.Name.Identifier)
        self.assertEqual(["inner", "m", "data"], [element.Name.Identifier for element in symbol.Constraints])

        for element in symbol.Constraints:
            self.assertIsInstance(element, RecordElementSymbol)
            self.assertIs(symbol, element.Parent)
        for constraint in symbol.Constraints.values():
            self.assertIs(symbol, constraint.Parent)

        inner, m, data = symbol.Constraints.values()
        self.assertIsInstance(inner, RecordElementConstraint)
        self.assertEqual("(field(3 downto 0))", str(inner))
        self.assertIsInstance(m, ArrayElementConstraint)
        self.assertEqual("(0 to 1, 0 to 7)", str(m))
        self.assertIsInstance(data, ArrayElementConstraint)
        self.assertEqual("(7 downto 0)", str(data))
        self.assertEqual(
            "subtype all_sub is all_t(inner(field(3 downto 0)), m(0 to 1, 0 to 7), data(7 downto 0));\n",
            VHDLWriter().Format(subtype),
        )
