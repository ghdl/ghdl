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
# Testsuite:        Check libghdl IIR translation to DOM for expressions.
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
import ctypes
from inspect import currentframe
from pathlib import Path
from textwrap import dedent
from unittest import TestCase


from pyGHDL.dom import Expression
from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.DesignUnit import Package
from pyGHDL.dom.Symbol import SimpleObjectOrFunctionCallSymbol
from pyGHDL.dom.Object import Constant
from pyGHDL.dom.Expression import InverseExpression, AbsoluteExpression


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Expressions(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _design = Design()
    _packageTemplate = dedent(
            """\
            package package_1 is
              {code}
            end package;
            """
        )

    def parse(self, filename: Path, code: str) -> Expression:
        sourceCode = self._packageTemplate.format(code=code)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        # Traverse already to default value expression
        package: Package = document.Packages[0]
        item: Constant = package.DeclaredItems[0]
        default: Expression = item.DefaultExpression

        return default

    def test_NotExpression(self):
        filename: Path = self._root / "{className}_{funcName}.vhdl".format(
            className=self.__class__.__name__, funcName= currentframe().f_code.co_name[5:]
        )

        # Define test data
        constantDeclartion = "constant c0 : boolean := not true;"

        # Parse in-memory
        default: Expression = self.parse(filename, constantDeclartion)

        # Start checks
        self.assertIsInstance(default, InverseExpression)
        self.assertIsInstance(default.Operand, SimpleObjectOrFunctionCallSymbol)
        self.assertEqual("true", str(default.Operand.SymbolName))

    # def test_AbsExpression(self):
    #     filename: Path = self._root / "{className}_{funcName}.vhdl".format(
    #         className=self.__class__.__name__, funcName= currentframe().f_code.co_name[5:]
    #     )
    #
    #     # Define test data
    #     constantDeclartion = "constant c0 : integer := abs 3;"
    #
    #     # Parse in-memory
    #     default: Expression = self.parse(filename, constantDeclartion)
    #
    #     # Start checks
    #     self.assertIsInstance(default, AbsoluteExpression)
    #     self.assertIsInstance(default.Operand, SimpleObjectOrFunctionCallSymbol)
    #     self.assertTrue(default.Operand.SymbolName == "-3")

    # def test_Aggregare(self):
    #     self._filename: Path = self._root / "{className}.vhdl".format(className=self.__class__.__name__)
    #
    #     sourceCode = dedent("""\
    #         package package_1 is
    #           constant c0 : integer_vector := (0, 1, 2); 0 =>);
    #           constant c1 : integer_vector := (0 => 0, 1 => 1, 2 => 2);
    #           constant c3 : integer_vector := (a => 0, b => 1, c => 2);
    #           constant c3 : integer_vector := (0 to 2 => 3, 3 to 4 => 2);
    #           constant c2 : integer_vector := (others => 0);
    #         end package;
    #         """)
    #
    #     with self._filename.open(mode="w", encoding="utf-8") as file:
    #         file.write(sourceCode)
    #
    #     design = Design()
    #     document = Document(self._filename)
    #     design.Documents.append(document)
    #
    #     package: Package = design.Documents[0].Packages[0]
    #     item: Constant = package.DeclaredItems[0]
    #     default: Expression = item.DefaultExpression
    #     self.assertIsInstance(default, InverseExpression)
    #     self.assertIsInstance(default.Operand, SimpleObjectOrFunctionCallSymbol)
    #     self.assertTrue(default.Operand.SymbolName == "true")
