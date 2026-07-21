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
# Testsuite:        Check libghdl IIR translation of sequential statements.
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

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Sequential import WhileLoopStatement, ExitStatement, NextStatement


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class LoopConditions(TestCase):
    """
    Regression tests: WhileLoopStatement, ExitStatement, and NextStatement previously hardcoded
    Condition to None instead of reading it - a while loop's own condition, and the 'when ...'
    clause on exit/next statements, were silently dropped.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/LoopConditions.vhdl"

    def test_WhileLoopCondition(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["loopconditions"]["rtl"]
        process = architecture.Statements[0]
        loop = process.Statements[0]

        self.assertIsInstance(loop, WhileLoopStatement)
        self.assertIsNotNone(loop.Condition)

    def test_ExitAndNextConditions(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["loopconditions"]["rtl"]
        process = architecture.Statements[0]
        loop = process.Statements[0]

        exitStatement, nextStatement = loop.Statements

        self.assertIsInstance(exitStatement, ExitStatement)
        self.assertIsNotNone(exitStatement.Condition)

        self.assertIsInstance(nextStatement, NextStatement)
        self.assertIsNotNone(nextStatement.Condition)
