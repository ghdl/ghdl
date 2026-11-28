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
# Testsuite:        Check libghdl IIR translation of file declarations.
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

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class FileOpenInformation(TestCase):
    """The file open information of a file declaration: an optional open kind and the logical name."""

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()

    def parse(self, declarations: str) -> list:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"
        sourceCode = dedent("""\
            library std;
            use std.textio.all;

            package p is
            {declarations}
            end package;
            """).format(declarations=declarations)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        return document.Packages["p"].DeclaredItems

    def test_OpenKind(self) -> None:
        (file,) = self.parse('  file f : text open read_mode is "data.txt";')

        self.assertEqual("read_mode", str(file.OpenKind.Name))
        self.assertEqual("data.txt", file.LogicalName.Value)
        self.assertEqual('file f : text open read_mode is "data.txt";\n', VHDLWriter().Format(file))

    def test_LogicalNameOnly(self) -> None:
        (file,) = self.parse('  file f : text is "data.txt";')

        self.assertIsNone(file.OpenKind)
        self.assertEqual("data.txt", file.LogicalName.Value)

    def test_NotOpened(self) -> None:
        (file,) = self.parse("  file f : text;")

        self.assertIsNone(file.OpenKind)
        self.assertIsNone(file.LogicalName)
        self.assertEqual("file f : text;\n", VHDLWriter().Format(file))
