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
# Testsuite:        Check libghdl IIR translation of conditional/selected/force/release assignments.
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
from pyGHDL.dom.Concurrent import ConcurrentConditionalSignalAssignment, ConcurrentSelectedSignalAssignment
from pyGHDL.dom.Concurrent import SelectedWaveform, OthersSelectedWaveform
from pyGHDL.dom.Sequential import (
    SequentialVariableAssignment,
    SequentialConditionalVariableAssignment, SequentialConditionalSignalAssignment,
    SequentialSelectedVariableAssignment, SequentialSelectedSignalAssignment,
    SelectedExpression, OthersSelectedExpression,
    SignalForceAssignment, SignalReleaseAssignment,
)


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Assignments(TestCase):
    """
    Regression tests: conditional/selected/force/release assignment statements were previously the
    only gap remaining in both the concurrent and sequential statement dispatchers - either warn-only
    (silently dropped) or a hard crash ('Unknown ... statement kind').
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Assignments.vhdl"

    @staticmethod
    def _architecture():
        design = Design()
        document = Document(Assignments._filename)
        design.Documents.append(document)
        return document.Architectures["assignments"]["rtl"]

    def test_ConcurrentConditionalSignalAssignment(self) -> None:
        """``s <= '1' when sel = 0 else '0' when sel = 1 else 'Z';``"""
        architecture = self._architecture()
        statement = architecture.Statements[0]

        self.assertIsInstance(statement, ConcurrentConditionalSignalAssignment)
        self.assertEqual(3, len(statement.ConditionalWaveforms))
        self.assertIsNotNone(statement.ConditionalWaveforms[0].Condition)
        self.assertIsNone(statement.ConditionalWaveforms[-1].Condition)

    def test_ConcurrentSelectedSignalAssignment(self) -> None:
        """``with sel select s2 <= '1' when 0 | 1, '0' when 2, 'Z' when others;`` - includes a
        grouped choice ('0 | 1')."""
        architecture = self._architecture()
        statement = architecture.Statements[1]

        self.assertIsInstance(statement, ConcurrentSelectedSignalAssignment)
        self.assertEqual(3, len(statement.SelectedWaveforms))

        grouped = statement.SelectedWaveforms[0]
        self.assertIsInstance(grouped, SelectedWaveform)
        self.assertEqual(2, len(grouped.Choices))

        self.assertIsInstance(statement.SelectedWaveforms[-1], OthersSelectedWaveform)

    def test_SequentialVariableAssignment(self) -> None:
        """``v := '1';``"""
        process = self._architecture().Statements[2]
        statement = process.Statements[0]

        self.assertIsInstance(statement, SequentialVariableAssignment)
        self.assertIsNotNone(statement.Expression)

    def test_SequentialConditionalVariableAssignment(self) -> None:
        """``v2 := '1' when sel = 0 else '0' when sel = 1 else 'Z';`` (VHDL-2008)"""
        process = self._architecture().Statements[2]
        statement = process.Statements[1]

        self.assertIsInstance(statement, SequentialConditionalVariableAssignment)
        self.assertEqual(3, len(statement.ConditionalExpressions))
        self.assertIsNone(statement.ConditionalExpressions[-1].Condition)

    def test_SequentialConditionalSignalAssignment(self) -> None:
        """``s <= '1' when sel = 0 else '0';`` (sequential form, VHDL-2008)"""
        process = self._architecture().Statements[2]
        statement = process.Statements[2]

        self.assertIsInstance(statement, SequentialConditionalSignalAssignment)
        self.assertEqual(2, len(statement.ConditionalWaveforms))

    def test_SequentialSelectedVariableAssignment(self) -> None:
        """``with sel select v := '1' when 0, '0' when others;``"""
        process = self._architecture().Statements[2]
        statement = process.Statements[3]

        self.assertIsInstance(statement, SequentialSelectedVariableAssignment)
        self.assertEqual(2, len(statement.SelectedExpressions))
        self.assertIsInstance(statement.SelectedExpressions[0], SelectedExpression)
        self.assertIsInstance(statement.SelectedExpressions[-1], OthersSelectedExpression)

    def test_SequentialSelectedSignalAssignment(self) -> None:
        """``with sel select s <= '1' when 0, '0' when others;`` (sequential form)"""
        process = self._architecture().Statements[2]
        statement = process.Statements[4]

        self.assertIsInstance(statement, SequentialSelectedSignalAssignment)
        self.assertEqual(2, len(statement.SelectedWaveforms))

    def test_SignalForceAssignment(self) -> None:
        """``s <= force '1';`` (VHDL-2008)"""
        process = self._architecture().Statements[2]
        statement = process.Statements[5]

        self.assertIsInstance(statement, SignalForceAssignment)
        self.assertIsNotNone(statement.Expression)

    def test_SignalReleaseAssignment(self) -> None:
        """``s <= release;`` (VHDL-2008)"""
        process = self._architecture().Statements[2]
        statement = process.Statements[6]

        self.assertIsInstance(statement, SignalReleaseAssignment)
        self.assertIsNotNone(statement.Target)
