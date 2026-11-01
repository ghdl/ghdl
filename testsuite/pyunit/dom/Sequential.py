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

        exitStatement, nextStatement, variableAssignment = loop.Statements

        self.assertIsInstance(exitStatement, ExitStatement)
        self.assertIsNotNone(exitStatement.Condition)

        self.assertIsInstance(nextStatement, NextStatement)
        self.assertIsNotNone(nextStatement.Condition)

        # Regression check: 'i := i + 1;' was previously silently dropped (warn-only), not even
        # appearing in loop.Statements at all.
        from pyGHDL.dom.Sequential import SequentialVariableAssignment
        self.assertIsInstance(variableAssignment, SequentialVariableAssignment)


class CaseStatements(TestCase):
    """
    Regression tests for sequential case statements: the alternative written directly before ``when others =>`` was
    built from the ``others`` choice node and got the ``others`` statements, and ``when others =>`` itself lost its
    statements.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/CaseStatements.vhdl"

    @staticmethod
    def _process():
        design = Design()
        document = Document(CaseStatements._filename)
        design.Documents.append(document)

        architecture = document.Architectures["casestatements"]["rtl"]
        return architecture.Statements[0]

    def test_AlternativesBeforeOthers(self) -> None:
        """``when 0 => ... when 1 | 2 => ... when others => ...``"""
        from pyGHDL.dom.Sequential import CaseStatement, Case, OthersCase

        statement = self._process().Statements[0]
        self.assertIsInstance(statement, CaseStatement)

        case0, case12, others = statement.Cases
        self.assertIsInstance(case0, Case)
        self.assertEqual(1, len(case0.Choices))
        self.assertEqual(1, len(case0.Statements))

        self.assertIsInstance(case12, Case)
        self.assertEqual(2, len(case12.Choices))
        self.assertEqual(2, len(case12.Statements))
        self.assertEqual("b", case12.Statements[0].Target.Name.Identifier)
        self.assertEqual("c", case12.Statements[1].Target.Name.Identifier)

        self.assertIsInstance(others, OthersCase)
        self.assertEqual(3, len(others.Statements))
        self.assertEqual("d", others.Statements[0].Target.Name.Identifier)
        self.assertEqual("f", others.Statements[2].Target.Name.Identifier)

    def test_SingleAlternativeBeforeOthers(self) -> None:
        """``when 0 => a := 1; when others => b := 2; c := 3;``"""
        from pyGHDL.dom.Sequential import Case, OthersCase

        statement = self._process().Statements[1]

        case0, others = statement.Cases
        self.assertIsInstance(case0, Case)
        self.assertEqual(1, len(case0.Statements))
        self.assertEqual("a", case0.Statements[0].Target.Name.Identifier)

        self.assertIsInstance(others, OthersCase)
        self.assertEqual(2, len(others.Statements))
        self.assertEqual("b", others.Statements[0].Target.Name.Identifier)
        self.assertEqual("c", others.Statements[1].Target.Name.Identifier)

    def test_OthersWithNullStatement(self) -> None:
        """``when others => null;``"""
        from pyGHDL.dom.Sequential import OthersCase, NullStatement

        statement = self._process().Statements[2]

        others = statement.Cases[1]
        self.assertIsInstance(others, OthersCase)
        self.assertEqual(1, len(others.Statements))
        self.assertIsInstance(others.Statements[0], NullStatement)
