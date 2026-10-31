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
# Testsuite:        Check that labels are translated as written in the source.
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


class Labels(TestCase):
    """
    A label keeps the spelling of the source file (``U1``); the model derives the lower-case ``NormalizedLabel``.
    Labels were translated lower-cased (``u1``).
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Labels.vhdl"

    @staticmethod
    def _architecture():
        design = Design()
        document = Document(Labels._filename)
        design.Documents.append(document)
        return document.Architectures["top"]["rtl"]

    def test_ConcurrentStatements(self) -> None:
        statements = self._architecture().Statements

        self.assertEqual("U1", statements[0].Label)
        self.assertEqual("u1", statements[0].NormalizedLabel)
        self.assertEqual("ProcA", statements[1].Label)
        self.assertEqual("BlkX", statements[2].Label)
        self.assertEqual("GenIf", statements[3].Label)
        self.assertEqual("GenCase", statements[4].Label)
        self.assertEqual("Asgn_Lbl", statements[5].Label)

    def test_GenerateAlternative(self) -> None:
        """``when AltOne: 1 =>`` - the alternative label of a case-generate branch."""
        caseGenerate = self._architecture().Statements[4]

        self.assertEqual("AltOne", caseGenerate.Cases[0].Label)

    def test_SequentialStatement(self) -> None:
        """``SeqLoop: for i in 0 to 1 loop`` inside a process."""
        process = self._architecture().Statements[6]

        self.assertEqual("SeqLoop", process.Statements[0].Label)
