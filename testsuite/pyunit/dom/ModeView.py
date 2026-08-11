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
# Testsuite:        Check libghdl IIR translation of VHDL-2019 mode views.
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

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.InterfaceItem import (
    ModeViewDeclaration,
    SimpleModeViewElement,
    CompositeModeViewElement,
    PortViewSignalInterfaceItem,
    ParameterViewSignalInterfaceItem,
)


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ModeViews(TestCase):
    """
    Checks translation of VHDL-2019 mode views: Mode_View_Declaration, Simple/Array/Record
    Mode_View_Element (Array/Record merged into CompositeModeViewElement), Interface_View_Declaration
    on ports and subprogram parameters, multi-identifier elements sharing one mode, nested/hierarchical
    composite elements, and the 'converse attribute.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/ModeViews.vhdl"

    @staticmethod
    def _design() -> Design:
        design = Design(vhdlVersion=VHDLVersion.VHDL2019)
        document = Document(ModeViews._filename)
        design.Documents.append(document)
        return design

    def test_SimpleAndCompositeElements(self) -> None:
        design = self._design()
        document = design.Documents[0]
        outerView = document.Packages["modeviews"].DeclaredItems[3]

        self.assertIsInstance(outerView, ModeViewDeclaration)
        self.assertEqual("OuterView", outerView.Identifier)
        self.assertEqual(2, len(outerView.Elements))

        ab, c = outerView.Elements
        self.assertIsInstance(ab, SimpleModeViewElement)
        self.assertEqual(("a", "b"), ab.Identifiers)

        self.assertIsInstance(c, CompositeModeViewElement)
        self.assertEqual(("c",), c.Identifiers)
        self.assertEqual("InnerView", c.ModeViewName.Name.Identifier)

    def test_ElementsKeepTheirDocumentation(self) -> None:
        """A mode view element is a declaration, so it carries its own comment."""
        design = self._design()
        innerView = design.Documents[0].Packages["modeviews"].DeclaredItems[2]

        x, y = innerView.Elements
        self.assertIn("--! An output element.", x.Documentation)
        self.assertEqual("--! An input element.", y.Documentation)

    def test_PortWithModeView(self) -> None:
        design = self._design()
        document = design.Documents[0]
        entity = document.Entities["consumer"]

        p1, p2 = entity.PortItems
        self.assertIsInstance(p1, PortViewSignalInterfaceItem)
        self.assertEqual(("p1",), p1.Identifiers)
        self.assertEqual("OuterView", p1.ModeViewIndication.Name.Identifier)

        self.assertIsInstance(p2, PortViewSignalInterfaceItem)
        self.assertEqual(("p2",), p2.Identifiers)
        # 'converse - the reference is an AttributeName, not a plain SimpleName
        self.assertEqual("converse", p2.ModeViewIndication.Name.Identifier)

    def test_ParameterWithModeView(self) -> None:
        design = self._design()
        document = design.Documents[0]
        architecture = document.Architectures["consumer"]["rtl"]
        procedure = architecture.DeclaredItems[0]

        parameter = procedure.ParameterItems[0]
        self.assertIsInstance(parameter, ParameterViewSignalInterfaceItem)
        self.assertEqual(("s",), parameter.Identifiers)
        self.assertEqual("OuterView", parameter.ModeViewIndication.Name.Identifier)
