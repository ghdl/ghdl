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
# Testsuite:        Check libghdl IIR translation of concurrent statements.
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

from pyGHDL.dom.Concurrent import ProcessStatement
from pyGHDL.dom.Name import AllName, SimpleName
from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ProcessSensitivityList(TestCase):
    """
    A process sensitivity list holds the names of the listed signals. ``process (all)`` holds a single ``AllName``
    without prefix; a process without sensitivity list has ``None``.
    """

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()

    def parse(self, statements: str) -> list:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"
        sourceCode = dedent("""\
            entity e is
            end entity;

            architecture a of e is
              signal clk, d, q : bit;
            begin
            {statements}
            end architecture;
            """).format(statements=statements)

        document = Document(filename, sourceCode)
        self._design.Documents.append(document)

        return document.Architectures["e"]["a"].Statements

    def test_All(self) -> None:
        (process,) = self.parse("\n".join(["  process (all)", "  begin", "    q <= d;", "  end process;"]))

        self.assertIsInstance(process, ProcessStatement)
        (name,) = process.SensitivityList
        self.assertIsInstance(name, AllName)
        self.assertIsNone(name.Prefix)
        self.assertEqual(
            "process (all) is\nbegin\n  q <= d;\nend process;\n",
            VHDLWriter().Format(process),
        )

    def test_Names(self) -> None:
        (process,) = self.parse("\n".join(["  process (clk, d)", "  begin", "    q <= d;", "  end process;"]))

        clk, d = process.SensitivityList
        self.assertIsInstance(clk, SimpleName)
        self.assertEqual("clk", clk.Identifier)
        self.assertIsInstance(d, SimpleName)
        self.assertEqual("d", d.Identifier)

    def test_None(self) -> None:
        (process,) = self.parse("\n".join(["  process", "  begin", "    wait;", "  end process;"]))

        self.assertIsNone(process.SensitivityList)
