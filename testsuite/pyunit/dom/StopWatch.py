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
# Testsuite:        Check libghdl IIR translation with a simple package.
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
from pathlib import Path
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Display(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _sourceDirectory: Path = _root / "dom/examples/StopWatch"

    def test_Encoder(self):
        design = Design()
        files = (
            Path("Utilities.pkg.vhdl"),
            Path("StopWatch.pkg.vhdl"),
            Path("seg7_Encoder.vhdl"),
            Path("toplevel.Encoder.vhdl"),
        )
        for file in files:
            document = Document(self._sourceDirectory / file)
            design.Documents.append(document)

        self.assertEqual(len(files), len(design.Documents))

    def test_Display(self):
        design = Design()
        files = (
            Path("Utilities.pkg.vhdl"),
            Path("StopWatch.pkg.vhdl"),
            Path("Counter.vhdl"),
            Path("seg7_Encoder.vhdl"),
            Path("seg7_Display.vhdl"),
            Path("toplevel.Display.vhdl"),
        )
        for file in files:
            document = Document(self._sourceDirectory / file)
            design.Documents.append(document)

        self.assertEqual(len(files), len(design.Documents))

    def test_StopWatch(self):
        design = Design()
        files = (
            Path("Utilities.pkg.vhdl"),
            Path("StopWatch.pkg.vhdl"),
            Path("Counter.vhdl"),
            Path("seg7_Encoder.vhdl"),
            Path("seg7_Display.vhdl"),
            Path("StopWatch.vhdl"),
            Path("Debouncer.vhdl"),
            Path("toplevel.StopWatch.vhdl"),
        )
        for file in files:
            document = Document(self._sourceDirectory / file)
            design.Documents.append(document)

        self.assertEqual(len(files), len(design.Documents))
