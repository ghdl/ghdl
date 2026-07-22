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
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
from pathlib import Path
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.Symbol import Symbol


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Aliases(TestCase):
    """
    Regression tests: Alias.parse() previously did nothing except read the alias's own identifier -
    the aliased name and optional subtype indication were never read at all.

    Also confirms that every alias variant (object, type, operator-with-signature) arrives as a single
    Object_Alias_Declaration kind at the parse-only level this project operates at - GHDL's parser
    never produces Non_Object_Alias_Declaration; that split only happens during semantic analysis.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Aliases.vhdl"

    @staticmethod
    def _package():
        design = Design()
        document = Document(Aliases._filename)
        design.Documents.append(document)
        return document.Packages["aliases"]

    def test_AliasWithSubtype(self) -> None:
        """``alias a : bit_vector(3 downto 0) is s(3 downto 0);``"""
        pkg = self._package()
        alias = pkg.DeclaredItems[1]

        self.assertIsInstance(alias, Alias)
        self.assertEqual("a", alias.Identifier)
        self.assertIsInstance(alias.Name, Symbol)
        self.assertIsNotNone(alias.Subtype)

    def test_AliasWithoutSubtype(self) -> None:
        """``alias b is s;``"""
        pkg = self._package()
        alias = pkg.DeclaredItems[2]

        self.assertIsInstance(alias, Alias)
        self.assertEqual("b", alias.Identifier)
        self.assertIsNotNone(alias.Name)
        self.assertIsNone(alias.Subtype)

    def test_TypeAlias(self) -> None:
        """``alias MyInt is Integer2;`` - arrives as Object_Alias_Declaration, same as any other alias."""
        pkg = self._package()
        alias = pkg.DeclaredItems[4]

        self.assertIsInstance(alias, Alias)
        self.assertEqual("MyInt", alias.Identifier)
        self.assertIsNotNone(alias.Name)

    def test_OperatorAliasWithSignature(self) -> None:
        """``alias "+" is add[integer, integer return integer];`` - Name is extracted from the
        Signature node's prefix; the parameter/return type marks are not captured (documented FIXME)."""
        pkg = self._package()
        alias = pkg.DeclaredItems[5]

        self.assertIsInstance(alias, Alias)
        self.assertIsNotNone(alias.Name)
