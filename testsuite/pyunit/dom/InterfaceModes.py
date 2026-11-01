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
# Testsuite:        Check the modes of interface items.
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

from pyVHDLModel import VHDLVersion
from pyVHDLModel.Base import Mode

from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class InterfaceModes(TestCase):
    """
    An interface item without a written mode has the mode ``Mode.Default``; a written mode is translated as given.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/InterfaceModes.vhdl"

    @staticmethod
    def _document() -> Document:
        design = Design()
        document = Document(InterfaceModes._filename)
        design.Documents.append(document)
        return document

    def test_Generics(self) -> None:
        """``generic (G0 : natural := 0; constant G1 : in natural := 1)``"""
        entity = self._document().Entities["interfacemodes"]
        g0, g1 = entity.GenericItems

        self.assertIs(Mode.Default, g0.Mode)
        self.assertIs(Mode.In, g1.Mode)

    def test_Ports(self) -> None:
        """``port (P0 : bit; P1 : in bit; P2 : out bit; P3 : inout bit; P4 : buffer bit)``"""
        entity = self._document().Entities["interfacemodes"]
        p0, p1, p2, p3, p4 = entity.PortItems

        self.assertIs(Mode.Default, p0.Mode)
        self.assertIs(Mode.In, p1.Mode)
        self.assertIs(Mode.Out, p2.Mode)
        self.assertIs(Mode.InOut, p3.Mode)
        self.assertIs(Mode.Buffer, p4.Mode)

    def test_ProcedureParameters(self) -> None:
        """``procedure Proc(a : integer; b : in integer; variable c : out integer; signal d : inout bit)``"""
        package = self._document().Packages["interfacemodespkg"]
        a, b, c, d = package.DeclaredItems[0].ParameterItems

        self.assertIs(Mode.Default, a.Mode)
        self.assertIs(Mode.In, b.Mode)
        self.assertIs(Mode.Out, c.Mode)
        self.assertIs(Mode.InOut, d.Mode)

    def test_FunctionParameters(self) -> None:
        """``function Func(x : integer; constant y : in integer) return integer``"""
        package = self._document().Packages["interfacemodespkg"]
        x, y = package.DeclaredItems[1].ParameterItems

        self.assertIs(Mode.Default, x.Mode)
        self.assertIs(Mode.In, y.Mode)

    def test_ModeViewElements(self) -> None:
        """``view InnerView of InnerRecord is x : out; y : in; end view;`` - an element always has a written mode."""
        design = Design(vhdlVersion=VHDLVersion.VHDL2019)
        document = Document(self._root / "examples/ModeViews.vhdl")
        design.Documents.append(document)

        innerView = document.Packages["modeviews"].DeclaredItems[2]
        x, y = innerView.Elements

        self.assertIs(Mode.Out, x.Mode)
        self.assertIs(Mode.In, y.Mode)
