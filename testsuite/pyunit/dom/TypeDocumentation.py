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
# Testsuite:        Check libghdl IIR translation of alias declarations.
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
from pathlib import Path
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class TypeDocumentation(TestCase):
    """
    Regression test: a doc comment on a type declaration was never extracted. `GetTypeFromNode` and
    `GetAnonymousTypeFromNode` received the *type declaration* node - the node a comment hangs off -
    but passed only the identifier and the inner type-definition node down to each `.parse()`, so
    every type was constructed with `documentation=None`.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/TypeDocumentation.vhdl"

    @staticmethod
    def _declaredItems(filename: Path):
        design = Design()
        document = Document(filename)
        design.Documents.append(document)

        return list(document.Packages.values())[0].DeclaredItems

    def test_EveryTypeKindKeepsItsDocumentation(self) -> None:
        expected = [
            ("stateA", "--! An enumerated type."),
            ("stateB", "--! A second enumerated type."),
            ("frame", "--! A record type."),
            ("memory", "--! An array type."),
            ("pointer", "--! An access type."),
            ("storage", "--! A file type."),
        ]
        items = self._declaredItems(self._filename)

        self.assertEqual(len(expected), len(items))
        for item, (identifier, documentation) in zip(items, expected):
            with self.subTest(type=identifier):
                self.assertEqual(documentation, item.Documentation)

    def test_RecordElementsKeepTheirDocumentation(self) -> None:
        """A record element is a declaration too, so it carries its own comment."""
        record = next(item for item in self._declaredItems(self._filename) if item.Identifier == "frame")

        expected = [
            (("a",), "--! The first field."),
            (("b", "c"), "--! The second and third fields."),
        ]
        self.assertEqual(len(expected), len(record.Elements))
        for element, (identifiers, documentation) in zip(record.Elements, expected):
            with self.subTest(element=identifiers):
                self.assertEqual(identifiers, element.Identifiers)
                self.assertEqual(documentation, element.Documentation)
