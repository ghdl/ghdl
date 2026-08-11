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
# Testsuite:        Check Design's selectable VHDL version ('--std=' option).
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

from pyGHDL.dom import DOMException
from pyGHDL.dom.NonStandard import Design, Document


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class VHDLVersionSelection(TestCase):
    """
    Design previously hardcoded '--std=08' unconditionally, with no way to select a different VHDL
    version - VHDL-2019 syntax (e.g. mode views) could never be parsed through pyGHDL.dom, even
    though GHDL's own parser/analyzer supports it. Design now accepts a vhdlVersion parameter,
    restricted to VHDLVersion.VHDL2008/VHDL2019 for now (older revisions are not planned to be
    supported).
    """

    _root = Path(__file__).resolve().parent

    def test_DefaultIsVHDL2008(self):
        design = Design()

        self.assertEqual(VHDLVersion.VHDL2008, design.VHDLVersion)

    def test_ExplicitVHDL2008(self):
        design = Design(vhdlVersion=VHDLVersion.VHDL2008)

        self.assertEqual(VHDLVersion.VHDL2008, design.VHDLVersion)

    def test_ExplicitVHDL2019(self):
        design = Design(vhdlVersion=VHDLVersion.VHDL2019)

        self.assertEqual(VHDLVersion.VHDL2019, design.VHDLVersion)

    def test_UnsupportedVersion_RaisesDOMException(self):
        for version in (VHDLVersion.VHDL87, VHDLVersion.VHDL93, VHDLVersion.VHDL2000, VHDLVersion.VHDL2002):
            with self.subTest(version=version):
                with self.assertRaises(DOMException):
                    Design(vhdlVersion=version)

    def test_VHDL2019SyntaxIsRejectedUnderVHDL2008(self):
        design = Design(vhdlVersion=VHDLVersion.VHDL2008)

        with self.assertRaises(DOMException):
            document = Document(self._root / "examples/ModeView.vhdl")
            design.Documents.append(document)

    def test_VHDL2019SyntaxIsAcceptedUnderVHDL2019(self):
        design = Design(vhdlVersion=VHDLVersion.VHDL2019)

        document = Document(self._root / "examples/ModeView.vhdl")
        design.Documents.append(document)

        modeView = document.Packages["modeviewpkg"].DeclaredItems[1]
        self.assertEqual("MasterView", modeView.Identifier)
        self.assertEqual(2, len(modeView.Elements))
