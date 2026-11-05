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
# Testsuite:        Check Design's selectable tab size ('-ftabstop=' option).
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


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unittest <testcase module>'")
    exit(1)


class TabSize(TestCase):
    _root = Path(__file__).resolve().parent / "examples"

    def _Documentation(self, tabSize: int) -> dict:
        design = Design(tabSize=tabSize)
        document = Document(self._root / "TabSize.vhdl")
        design.AddDocument(document, design.GetLibrary("default"))

        architecture = document.Architectures["e"]["a"]
        return {item.Identifiers[0]: item.Documentation for item in architecture.DeclaredItems}

    def test_Default(self) -> None:
        self.assertEqual(8, Design().TabSize)

    def test_Value(self) -> None:
        self.assertEqual(4, Design(tabSize=4).TabSize)

    def test_Limits(self) -> None:
        self.assertEqual(1, Design(tabSize=1).TabSize)
        self.assertEqual(120, Design(tabSize=120).TabSize)

    def test_Continuation_Default(self) -> None:
        design = Design()
        document = Document(self._root / "TabSize.vhdl")
        design.AddDocument(document, design.GetLibrary("default"))

        items = document.Architectures["e"]["a"].DeclaredItems
        self.assertIn("second line at tab size 8", items[1].Documentation)

    def test_Continuation_8(self) -> None:
        documentation = self._Documentation(8)

        self.assertIn("second line at tab size 8", documentation["s8"])
        self.assertNotIn("second line at tab size 4", documentation["s4"])

    def test_Continuation_4(self) -> None:
        documentation = self._Documentation(4)

        self.assertIn("second line at tab size 4", documentation["s4"])
        self.assertNotIn("second line at tab size 8", documentation["s8"])

    def test_Reset(self) -> None:
        """A new design doesn't inherit the tab size of the previous one."""
        Design(tabSize=4)

        documentation = self._Documentation(8)

        self.assertIn("second line at tab size 8", documentation["s8"])

    def test_None(self) -> None:
        with self.assertRaises(ValueError) as context:
            Design(tabSize=None)

        self.assertEqual("Parameter 'tabSize' is None.", str(context.exception))

    def test_str(self) -> None:
        with self.assertRaises(TypeError) as context:
            Design(tabSize="4")

        self.assertEqual("Parameter 'tabSize' is not of type 'int'.", str(context.exception))
        self.assertListEqual(["Got type 'str'."], context.exception.__notes__)

    def test_bool(self) -> None:
        with self.assertRaises(TypeError) as context:
            Design(tabSize=True)

        self.assertEqual("Parameter 'tabSize' is not of type 'int'.", str(context.exception))
        self.assertListEqual(["Got type 'bool'."], context.exception.__notes__)

    def test_OutOfRange(self) -> None:
        for value in (0, 121, -1):
            with self.subTest(value=value):
                with self.assertRaises(ValueError) as context:
                    Design(tabSize=value)

                self.assertEqual("Parameter 'tabSize' is out of range 1..120.", str(context.exception))
                self.assertListEqual([f"Got value '{value}'."], context.exception.__notes__)

    def test_Positional(self) -> None:
        self.assertEqual(4, Design("name", VHDLVersion.VHDL2008, 4).TabSize)

    def test_CheckOrder(self) -> None:
        """The VHDL version is checked before the tab size, in parameter order."""
        with self.assertRaises(DOMException):
            Design(vhdlVersion=VHDLVersion.VHDL93, tabSize=0)
