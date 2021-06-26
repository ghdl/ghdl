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
# Testsuite:        Check libghdl IIR translation to DOM for literals.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
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
from textwrap import dedent
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Object import Constant
from pyGHDL.dom.Literal import IntegerLiteral


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Literals(TestCase):
    _root = Path(__file__).resolve().parent.parent

    def test_IntegerLiteral(self):
        self._filename: Path = self._root / "{className}.vhdl".format(
            className=self.__class__.__name__
        )

        sourceCode = dedent(
            """\
            package package_1 is
              constant c0 : integer := 0;
              constant c1 : integer := 1;
              constant c2 : integer := 1024;
              constant c3 : integer := 1048576;
            end package;
            """
        )
        expected = (0, 1, 1024, 1048576)

        with self._filename.open(mode="w", encoding="utf-8") as file:
            file.write(sourceCode)

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(len(design.Documents[0].Packages), 1)
        package = design.Documents[0].Packages[0]
        self.assertTrue(package.Name == "package_1")
        self.assertEqual(len(package.DeclaredItems), len(expected))
        for i in range(len(expected)):
            item: Constant = package.DeclaredItems[i]
            self.assertTrue(isinstance(item, Constant))
            self.assertTrue(item.Name == "c{}".format(i))
            self.assertTrue(str(item.SubType.SymbolName) == "integer")
            self.assertTrue(isinstance(item.DefaultExpression, IntegerLiteral))
            self.assertTrue(item.DefaultExpression.Value == expected[i])
