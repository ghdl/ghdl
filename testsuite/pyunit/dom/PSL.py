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
# Testsuite:        Check libghdl IIR translation of PSL declarations.
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

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.PSL import DefaultClock

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class DefaultClocks(TestCase):
    """
    ``default clock is ...;`` aborted the Python process: ``DefaultClock.parse`` read the identifier of a
    ``Psl_Default_Clock`` node, which has none, and libghdl failed an internal assertion.
    """

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()

    def parse(self, declarations: str) -> list:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"
        sourceCode = dedent("""\
            entity e is
            end entity;

            architecture a of e is
              signal clk, en : bit;
            {declarations}
            begin
            end architecture;
            """).format(declarations=declarations)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        return document.Architectures["e"]["a"].DeclaredItems[1:]

    def test_FunctionCall(self) -> None:
        (defaultClock,) = self.parse("  default clock is rising_edge(clk);")

        self.assertIsInstance(defaultClock, DefaultClock)
        self.assertEqual("rising_edge(clk)", defaultClock.Identifier)
        self.assertEqual("default clock rising_edge(clk)", str(defaultClock))

    def test_MultiLineWithComment(self) -> None:
        declaration = "\n".join(
            [
                "  DEFAULT CLOCK IS  -- the clock",
                "    (clk'event and  clk = '1')",
                "    and en = '1';",
            ]
        )
        (defaultClock,) = self.parse(declaration)

        self.assertIsInstance(defaultClock, DefaultClock)
        self.assertEqual("(clk'event and clk = '1') and en = '1'", defaultClock.Identifier)

    def test_FollowingDeclaration(self) -> None:
        defaultClock, signal = self.parse("  default clock is clk;\n  signal s : bit;")

        self.assertIsInstance(defaultClock, DefaultClock)
        self.assertEqual("clk", defaultClock.Identifier)
        self.assertEqual(("s",), signal.Identifiers)
