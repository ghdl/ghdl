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

from pyVHDLModel.SyntaxModel import ExpressionUnion

from pyGHDL.dom.DesignUnit import Package

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Object import Constant
from pyGHDL.dom.Literal import IntegerLiteral


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Literals(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _design = Design()
    _packageTemplate = dedent(
            """\
            package package_1 is
              {code}
            end package;
            """
        )

    def parse(self, filename: Path, code: str) -> ExpressionUnion:
        sourceCode = self._packageTemplate.format(code=code)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        # Traverse already to default value expression
        package: Package = document.Packages[0]
        item: Constant = package.DeclaredItems[0]
        default: ExpressionUnion = item.DefaultExpression

        return default

    def test_IntegerLiteral(self):
        _filename: Path = self._root / "{className}.vhdl".format(
            className=self.__class__.__name__
        )

        constantDeclartion = "constant c0 : integer := 0;"
        expected = (0, 1, 1024, 1048576)

        # Parse in-memory
        default: ExpressionUnion = self.parse(_filename, constantDeclartion)

        self.assertIsInstance(default, IntegerLiteral)
        self.assertEqual(expected[0], default.Value)
