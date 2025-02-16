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
from typing import TypeVar, Dict
from unittest import TestCase

from pyTooling.Common import firstValue

from pyGHDL.dom.NonStandard import Design, Document


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class SimpleEntity(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _filename: Path = _root / "dom/examples/SimpleEntity.vhdl"

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
        print()
        print(document.Documentation)
        self.assertEqual(4, len(document.Documentation.splitlines()))

    def test_Entity(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].Entities))

        entity = firstValue(design.Documents[0].Entities)
        self.assertEqual("Counter", entity.Identifier)
        print()
        print(entity.Documentation)
        self.assertEqual(11, len(entity.Documentation.splitlines()))

    def test_Architecture(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].Architectures))

        architecture = firstValue(firstValue(design.Documents[0].Architectures))
        self.assertEqual("rtl", architecture.Identifier)
        print()
        print(architecture.Documentation)
        self.assertEqual(1, len(architecture.Documentation.splitlines()))


class SimplePackage(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _filename: Path = _root / "dom/examples/SimplePackage.vhdl"

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
        print()
        print(document.Documentation)
        self.assertEqual(4, len(document.Documentation.splitlines()))

    def test_Package(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].Packages))

        package = firstValue(design.Documents[0].Packages)
        self.assertEqual("utilities", package.Identifier)
        print()
        print(package.Documentation)
        self.assertEqual(1, len(package.Documentation.splitlines()))

    def test_PackageBody(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(1, len(design.Documents[0].PackageBodies))

        packageBodies = firstValue(design.Documents[0].PackageBodies)
        self.assertEqual("utilities", packageBodies.Identifier)
        print()
        print(packageBodies.Documentation)
        self.assertEqual(0, len(packageBodies.Documentation.splitlines()))
