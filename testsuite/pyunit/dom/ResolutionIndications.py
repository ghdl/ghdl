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
# Testsuite:        Check libghdl IIR translation of resolution indications.
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
from pyGHDL.dom.Symbol import (
    SimpleSubtypeSymbol,
    ConstrainedScalarSubtypeSymbol,
    ConstrainedArraySubtypeSymbol,
    ConstrainedRecordSubtypeSymbol,
    RecordElementSymbol,
    ResolutionFunctionSymbol,
    ArrayElementResolution,
    RecordResolution,
    RecordElementResolution,
)

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ResolutionIndications(TestCase):
    """
    The resolution indication of a subtype indication (IEEE Std 1076-2008, 6.3) was dropped: ``resolved std_ulogic``
    arrived as the type mark ``std_ulogic`` alone. It is translated to the ``resolution`` of the subtype symbol now.
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
              type T is ('0', '1', 'Z');
              type T_vector is array (natural range <>) of T;
              type T_matrix is array (natural range <>) of T_vector;
              type R is record
                a : T;
                b : T_vector;
              end record;
              function res (v : T_vector) return T;
            {declarations}
            end package;
            """).format(declarations=declarations)
        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        return document.Packages["p"].DeclaredItems[5:]

    def test_None(self) -> None:
        """``subtype ST is T;``: a type mark without resolution indication."""
        (subtype,) = self.parse("  subtype ST is T;")

        self.assertIsNone(subtype.Type.Resolution)

    def test_ResolutionFunction(self) -> None:
        """``subtype RT is res T;``"""
        (subtype,) = self.parse("  subtype RT is res T;")

        symbol = subtype.Type
        self.assertIsInstance(symbol, SimpleSubtypeSymbol)
        self.assertIsInstance(symbol.Resolution, ResolutionFunctionSymbol)
        self.assertEqual("res", symbol.Resolution.Name.Identifier)
        self.assertIs(symbol, symbol.Resolution.Parent)
        self.assertEqual("t", symbol.Name.NormalizedIdentifier)
        self.assertEqual("subtype RT is res T;\n", VHDLWriter().Format(subtype))

    def test_ResolutionFunction_SelectedName(self) -> None:
        """``subtype RT is work.p.res T;``"""
        (subtype,) = self.parse("  subtype RT is work.p.res T;")

        self.assertIsInstance(subtype.Type.Resolution, ResolutionFunctionSymbol)
        self.assertEqual("subtype RT is work.p.res T;\n", VHDLWriter().Format(subtype))

    def test_RangeConstraint(self) -> None:
        """``subtype X01 is res T range '0' to '1';``"""
        (subtype,) = self.parse("  subtype X01 is res T range '0' to '1';")

        self.assertIsInstance(subtype.Type, ConstrainedScalarSubtypeSymbol)
        self.assertIsInstance(subtype.Type.Resolution, ResolutionFunctionSymbol)
        self.assertEqual("subtype X01 is res T range '0' to '1';\n", VHDLWriter().Format(subtype))

    def test_ArrayElementResolution(self) -> None:
        """``subtype RTV is (res) T_vector;``"""
        (subtype,) = self.parse("  subtype RTV is (res) T_vector;")

        resolution = subtype.Type.Resolution
        self.assertIsInstance(resolution, ArrayElementResolution)
        self.assertIsInstance(resolution.Resolution, ResolutionFunctionSymbol)
        self.assertIs(resolution, resolution.Resolution.Parent)
        self.assertEqual("subtype RTV is (res) T_vector;\n", VHDLWriter().Format(subtype))

    def test_ArrayElementResolution_Nested(self) -> None:
        """``subtype RTM is ((res)) T_matrix;``"""
        (subtype,) = self.parse("  subtype RTM is ((res)) T_matrix;")

        self.assertIsInstance(subtype.Type.Resolution.Resolution, ArrayElementResolution)
        self.assertEqual("subtype RTM is ((res)) T_matrix;\n", VHDLWriter().Format(subtype))

    def test_ArrayElementResolution_IndexConstraint(self) -> None:
        """``subtype RTV8 is (res) T_vector(7 downto 0);``"""
        (subtype,) = self.parse("  subtype RTV8 is (res) T_vector(7 downto 0);")

        self.assertIsInstance(subtype.Type, ConstrainedArraySubtypeSymbol)
        self.assertIsInstance(subtype.Type.Resolution, ArrayElementResolution)
        self.assertEqual("subtype RTV8 is (res) T_vector(7 downto 0);\n", VHDLWriter().Format(subtype))

    def test_RecordResolution(self) -> None:
        """``subtype RR is (a res, b (res)) R;``"""
        (subtype,) = self.parse("  subtype RR is (a res, b (res)) R;")

        resolution = subtype.Type.Resolution
        self.assertIsInstance(resolution, RecordResolution)
        self.assertEqual(2, len(resolution.Elements))
        a, b = resolution.Elements
        self.assertIsInstance(a, RecordElementResolution)
        self.assertIsInstance(a.Element, RecordElementSymbol)
        self.assertEqual("a", a.Element.Name.Identifier)
        self.assertIsInstance(a.Resolution, ResolutionFunctionSymbol)
        self.assertEqual("b", b.Element.Name.Identifier)
        self.assertIsInstance(b.Resolution, ArrayElementResolution)
        self.assertEqual("subtype RR is (a res, b (res)) R;\n", VHDLWriter().Format(subtype))

    def test_RecordResolution_Spaces(self) -> None:
        """``subtype RR is ( A res , B res ) R;``: the first element name is read after the opening parenthesis."""
        (subtype,) = self.parse("  subtype RR is ( A res , B res ) R;")

        self.assertListEqual(
            ["A", "B"], [element.Element.Name.Identifier for element in subtype.Type.Resolution.Elements]
        )
        self.assertEqual("subtype RR is (A res, B res) R;\n", VHDLWriter().Format(subtype))

    def test_RecordResolution_RecordConstraint(self) -> None:
        """``subtype RR8 is (a res) R(b(7 downto 0));``"""
        (subtype,) = self.parse("  subtype RR8 is (a res) R(b(7 downto 0));")

        self.assertIsInstance(subtype.Type, ConstrainedRecordSubtypeSymbol)
        self.assertIsInstance(subtype.Type.Resolution, RecordResolution)
        self.assertEqual("subtype RR8 is (a res) R(b(7 downto 0));\n", VHDLWriter().Format(subtype))

    def test_Signal(self) -> None:
        """``signal s : res T;``: every subtype indication carries its resolution indication."""
        (signal,) = self.parse("  signal s : res T;")

        self.assertIsInstance(signal.Subtype.Resolution, ResolutionFunctionSymbol)
        self.assertEqual("signal s : res T;\n", VHDLWriter().Format(signal))
