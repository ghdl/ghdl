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
# Testsuite:        Check the pretty printer used by `ghdl-dom pretty`.
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
from pyGHDL.dom.formatting.prettyprint import PrettyPrint


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class PrettyPrinter(TestCase):
    """
    Regression tests for the pretty printer behind ``ghdl-dom pretty``.

    Nothing in the testsuite imported this module before, which is how it came to import a
    ``PortSignalInterfaceItem`` that ``pyGHDL.dom.InterfaceItem`` doesn't define - an ImportError that
    made the whole module unusable while every test stayed green.
    """

    _root = Path(__file__).resolve().parent

    def _format(self, filename: str, vhdlVersion: VHDLVersion = VHDLVersion.VHDL2008):
        design = Design(vhdlVersion=vhdlVersion)
        library = design.GetLibrary("prettyprint")
        document = Document(self._root / "examples" / filename)
        design.AddDocument(document, library)

        return PrettyPrint().formatDocument(document, 1)

    def test_ModuleIsImportable(self) -> None:
        """The import above is the actual regression check - this makes the intent explicit."""
        from pyGHDL.dom.formatting import prettyprint

        self.assertTrue(hasattr(prettyprint.PrettyPrint, "formatPortSignal"))

    def test_PlainEntity(self) -> None:
        buffer = self._format("SimpleEntity.vhdl")

        self.assertGreater(len(buffer), 0)
        self.assertTrue(any("Name: Counter" in line for line in buffer))

    def test_ModeViewDeclarationAndViewPorts(self) -> None:
        """VHDL-2019 mode views: the declaration, both element kinds, and view-typed ports."""
        buffer = self._format("ModeViews.vhdl", VHDLVersion.VHDL2019)
        text = "\n".join(buffer)

        # The mode view declaration and both element kinds.
        self.assertIn("view InnerView of InnerRecord", text)
        self.assertIn("x : out", text)
        self.assertIn("c : view InnerView", text)

        # Ports declared with a mode view instead of a mode.
        self.assertIn("p1 : view OuterView", text)
        self.assertIn("p2 : view OuterView'converse", text)
