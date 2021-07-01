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
# Testsuite:        Check libghdl IIR translation with a simple entity.
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


class SimpleEntity(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _filename: Path = _root / "SimpleEntity.vhdl"

    def test_Design(self):
        design = Design()

        self.assertIsNotNone(design)

    # def test_Library(self):
    # 	library = Library()

    def test_Document(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents))

    def test_Entity(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].Entities))
        self.assertEqual("entity_1", design.Documents[0].Entities[0].Identifier)

    def test_Architecture(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].Architectures))
        self.assertEqual("behav", design.Documents[0].Architectures[0].Identifier)
