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
# Testsuite:        Check that libghdl reports errors of a document parsed without a design.
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
from subprocess import run
from sys import executable
from textwrap import dedent
from unittest import TestCase

if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ReportHandler(TestCase):
    def test_DocumentWithoutDesign(self) -> None:
        """
        A document parsed before any design is created reports a syntax error as a ``DOMException``.

        The script runs in a new process: this process' libghdl has a report handler already, installed by the design of
        another testcase.
        """
        script = dedent("""\
            from pathlib import Path

            from pyGHDL.dom import DOMException
            from pyGHDL.dom.NonStandard import Document

            sourceCode = "entity e is end entity;\\narchitecture a of e is begin x <= ; end architecture;\\n"
            try:
                Document(Path("ReportHandler.vhdl"), sourceCode=sourceCode)
            except DOMException as ex:
                print(f"DOMException: {ex}")
            """)

        result = run([executable, "-c", script], capture_output=True, text=True, timeout=60)

        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(result.stdout.startswith("DOMException: "), result.stdout + result.stderr)
