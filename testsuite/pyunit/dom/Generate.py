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
# Testsuite:        Check libghdl IIR translation of generate-statement alternative labels.
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


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class GenerateAlternativeLabels(TestCase):
    """
    Regression tests: generate-statement alternative labels (if/elsif/else branches, case-generate
    alternatives) were always read as an empty string ('# TODO: alternative label'), regardless of
    whether the source actually gave one - and there was no way to tell "no label given" apart from
    "label happens to be empty".

    Also caught a real, independent, pre-existing bug while testing this: when a case-generate
    alternative was immediately followed by 'others' with no other cases in between, the wrong IIR
    node was passed to GenerateCase.parse() (the already-advanced 'others' node instead of the
    original case's node), corrupting that case's label (and, by the same mechanism, its declared
    items/statements).
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/GenerateLabels.vhdl"

    @staticmethod
    def _architecture():
        design = Design()
        document = Document(GenerateAlternativeLabels._filename)
        design.Documents.append(document)
        return document.Architectures["generatelabels"]["rtl"]

    def test_IfBranchWithLabel(self) -> None:
        """``if label_a : true generate``"""
        architecture = self._architecture()
        statement = architecture.Statements[0]

        self.assertEqual("label_a", statement.IfBranch.AlternativeLabel)

    def test_IfBranchWithoutLabel_ElsifAndElseWithLabels(self) -> None:
        """``if false generate elsif label_b : true generate ... else label_c : generate``"""
        architecture = self._architecture()
        statement = architecture.Statements[1]

        self.assertIsNone(statement.IfBranch.AlternativeLabel)
        self.assertEqual("label_b", statement.ElsifBranches[0].AlternativeLabel)
        self.assertEqual("label_c", statement.ElseBranch.AlternativeLabel)

    def test_CaseGenerateAlternatives(self) -> None:
        """``when case_label : 1 => ... when others_label : others =>`` - also confirms the
        immediately-followed-by-others fix: without it, both cases would show 'others_label'."""
        architecture = self._architecture()
        statement = architecture.Statements[2]

        case, othersCase = statement.Cases
        self.assertEqual("case_label", case.Label)
        self.assertEqual("others_label", othersCase.Label)
