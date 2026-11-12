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
# Testsuite:        Check the VHDL code written by `ghdl-dom pretty`.
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
from re import compile as re_compile
from subprocess import run
from sys import executable
from textwrap import dedent
from unittest import TestCase, expectedFailure

from pyVHDLModel.Formatting.VHDL import VHDLWriter

from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class DocumentWriter(TestCase):
    """
    ``ghdl-dom pretty`` writes each document with pyVHDLModel's ``VHDLWriter``: the design units with their context
    items, in source order.
    """

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()

    def parse(self, sourceCode: str) -> Document:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"

        document = Document(filename, dedent(sourceCode))
        self._design.Documents.append(document)

        return document

    def test_DesignUnitsInSourceOrder(self) -> None:
        """Context items stay with their design unit; the units keep their order; comments aren't written."""
        document = self.parse("""\
            -- File header.
            context c is
              library ieee;
              use ieee.numeric_std.all;
            end context;

            library ieee;
            use     ieee.std_logic_1164.all;

            -- Documentation of entity e.
            entity e is
              port (
                clk : in std_logic
              );
            end entity;

            package p is
              constant C : natural := 4;
            end package;

            use work.p.all;

            architecture a of e is
              signal s : std_logic_vector(C - 1 downto 0);
            begin
              s <= (others => clk);
            end architecture;

            package body p is
            end package body;

            configuration cfg of e is
              for a
              end for;
            end configuration;
            """)

        self.assertEqual(
            dedent("""\
                context c is
                  library ieee;
                  use ieee.numeric_std.all;
                end context c;

                library ieee;
                use ieee.std_logic_1164.all;

                entity e is
                  port (
                    clk : in std_logic
                  );
                end entity e;

                package p is
                  constant C : natural := 4;
                end package p;

                use work.p.all;

                architecture a of e is
                  signal s : std_logic_vector(C - 1 downto 0);
                begin
                  s <= (others => clk);
                end architecture a;

                package body p is
                end package body p;

                configuration cfg of e is
                  for a
                  end for;
                end configuration cfg;
                """),
            VHDLWriter().Format(document),
        )

    def test_UnresolvedReferences(self) -> None:
        """Names declared in another document are written as they are; nothing is analyzed."""
        document = self.parse("""\
            context work.StopWatch_ctx;

            configuration seg7_Display_cfg of seg7_Display is
              for rtl
                for enc : seg7_Encoder
                  use entity work.seg7_Encoder(rtl);
                end for;
              end for;
            end configuration;
            """)

        self.assertEqual(
            dedent("""\
                context work.StopWatch_ctx;

                configuration seg7_Display_cfg of seg7_Display is
                  for rtl
                    for enc : seg7_Encoder
                      use entity work.seg7_Encoder(rtl);
                    end for;
                  end for;
                end configuration seg7_Display_cfg;
                """),
            VHDLWriter().Format(document),
        )

    @expectedFailure
    def test_ConcurrentAssertion(self) -> None:
        """pyGHDL.dom translates a concurrent assertion (a PSL assert directive) without its condition."""
        document = self.parse("""\
            entity e is
            end entity;

            architecture a of e is
            begin
              assert false report "message";
            end architecture;
            """)

        self.assertEqual(
            dedent("""\
                entity e is
                end entity e;

                architecture a of e is
                begin
                  assert false report "message";
                end architecture a;
                """),
            VHDLWriter().Format(document),
        )


class PrettyCommand(TestCase):
    """
    ``ghdl-dom pretty`` on the files of the StopWatch example, which refer to design units of other files.
    """

    _sourceDirectory = Path(__file__).resolve().parent / "examples" / "StopWatch"
    _colors = re_compile(r"\x1b\[[0-9;]*m")

    def pretty(self, *arguments: str):
        result = run(
            [executable, "-m", "pyGHDL.cli.dom", "pretty", *arguments], capture_output=True, text=True, timeout=120
        )
        return result.returncode, self._colors.sub("", result.stdout), self._colors.sub("", result.stderr)

    def test_Files(self) -> None:
        """Each file is written on its own; the context and entity they refer to are in other files."""
        returnCode, stdout, stderr = self.pretty(
            "-f",
            str(self._sourceDirectory / "StopWatch.ctx.vhdl"),
            "-f",
            str(self._sourceDirectory / "seg7_Display.cfg.vhdl"),
        )

        self.assertEqual(0, returnCode, stdout + stderr)
        self.assertIn(
            dedent("""\
                context StopWatch_ctx is
                  library lib_Utilities;
                  context lib_Utilities.Utilities_ctx;
                  use work.StopWatch_pkg.all;
                end context StopWatch_ctx;
                """),
            stdout,
        )
        self.assertIn(
            dedent("""\
                context work.StopWatch_ctx;

                configuration seg7_Display_cfg of seg7_Display is
                  for rtl
                    for enc : seg7_Encoder
                      use entity work.seg7_Encoder(rtl);
                    end for;
                  end for;
                end configuration seg7_Display_cfg;
                """),
            stdout,
        )

    def test_Directory(self) -> None:
        """
        Each file of the directory is written, also the three files declaring an entity ``toplevel``.

        ``Debouncer.vhdl`` is reported as an error, see :meth:`DocumentWriter.test_ConcurrentAssertion`.
        """
        returnCode, stdout, stderr = self.pretty("-D", str(self._sourceDirectory), "-L", "lib_StopWatch")
        output = stdout + stderr

        files = sorted(self._sourceDirectory.glob("*.vhdl"))
        self.assertEqual(15, len(files))
        readFiles = {Path(line.split("'")[1]).name for line in stdout.splitlines() if "Reading file '" in line}
        self.assertEqual({file.name for file in files}, readFiles, output)

        self.assertEqual(3, stdout.count("\nend entity toplevel;\n"), output)
        self.assertIn("\nend package body Utilities_pkg;\n", stdout)
        self.assertIn("\nend configuration seg7_Display_cfg;\n", stdout)

        errors = [line for line in output.splitlines() if line.startswith("[ERROR]")]
        self.assertEqual(1, len(errors), output)
        self.assertIn(
            f"Document '{self._sourceDirectory / 'Debouncer.vhdl'!s}' can't be written as VHDL code", errors[0]
        )
        self.assertEqual(255, returnCode, output)
