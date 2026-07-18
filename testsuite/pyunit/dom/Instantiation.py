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
# Testsuite:        Check libghdl IIR translation of a package instantiation.
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
from pyGHDL.dom.DesignUnit import LibraryClause, UseClause


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class PackageInstantiation(TestCase):
    """
    Checks translation of ``Iir_Kind.Package_Instantiation_Declaration``, in particular:

    * that ``library``/``use`` clauses preceding the instantiation are handed over as ``ContextItems`` (previously
      computed by ``Document.translate()`` and then silently discarded instead of being forwarded to
      ``PackageInstantiation.parse()``), and
    * that the ``generic map (...)`` aspect written at the instantiation site is translated into
      ``GenericAssociationItems``.

    Note: the generic *declarations* of the referenced (uninstantiated) package cannot be read from the
    instantiation at this stage - ``Get_Uninstantiated_Package_Decl``/``Get_Generic_Chain`` on the instantiation node
    are only populated after semantic analysis resolves and macro-expands the reference, which
    ``Document.translate()`` (parse-only) does not perform.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/PackageInstantiation.vhdl"

    def test_Document(self):
        print()

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)
        print(f"{document.Path}:")
        for warning in document._warnings:
            print(f"  {warning}")

        self.assertEqual(2, len(document.Packages))

    def test_ContextItems(self):
        print()

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)
        print(f"{document.Path}:")
        for warning in document._warnings:
            print(f"  {warning}")

        packageInstantiation = document.Packages["instantiatedpackage"]

        self.assertEqual("InstantiatedPackage", packageInstantiation.Identifier)
        self.assertEqual(2, len(packageInstantiation.ContextItems))

        libraryClause, useClause = packageInstantiation.ContextItems

        self.assertIsInstance(libraryClause, LibraryClause)
        self.assertEqual(1, len(libraryClause.Symbols))
        self.assertEqual("ieee", libraryClause.Symbols[0].Name.Identifier)

        self.assertIsInstance(useClause, UseClause)
        self.assertEqual(1, len(useClause.Symbols))

    def test_GenericAssociationItems(self):
        print()

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)
        print(f"{document.Path}:")
        for warning in document._warnings:
            print(f"  {warning}")

        packageInstantiation = document.Packages["instantiatedpackage"]

        self.assertEqual(1, len(packageInstantiation.GenericAssociationItems))

        association = packageInstantiation.GenericAssociationItems[0]
        self.assertEqual("WIDTH", association.Formal.Identifier)
        self.assertEqual(16, association.Actual.Value)

    def test_PackageWithoutContextItemsOrGenericMap(self):
        print()

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)
        print(f"{document.Path}:")
        for warning in document._warnings:
            print(f"  {warning}")

        package = document.Packages["genericpackage"]

        self.assertEqual("GenericPackage", package.Identifier)
        self.assertEqual(0, len(package.ContextItems))
