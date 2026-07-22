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
# Testsuite:        Check libghdl IIR translation of constrained scalar subtypes.
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
from pyGHDL.dom.Symbol import ConstrainedScalarSubtypeSymbol


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class ConstrainedScalarSubtypes(TestCase):
    """
    Regression tests: ConstrainedScalarSubtypeSymbol previously read the range constraint (rng) but
    never forwarded it to the base class ('# , rng)  # XXX: hacked') - every constrained scalar
    subtype silently lost its range constraint.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/ConstrainedScalar.vhdl"

    @staticmethod
    def _architecture():
        design = Design()
        document = Document(ConstrainedScalarSubtypes._filename)
        design.Documents.append(document)
        return document.Architectures["constrainedscalar"]["rtl"]

    def test_IntegerRange(self) -> None:
        """``signal s : integer range 0 to 15;``"""
        architecture = self._architecture()
        signal = architecture.DeclaredItems[0]

        self.assertIsInstance(signal.Subtype, ConstrainedScalarSubtypeSymbol)
        self.assertIsNotNone(signal.Subtype.Constraint)

    def test_NaturalRange(self) -> None:
        """``signal t : natural range 3 to 9;``"""
        architecture = self._architecture()
        signal = architecture.DeclaredItems[1]

        self.assertIsInstance(signal.Subtype, ConstrainedScalarSubtypeSymbol)
        self.assertIsNotNone(signal.Subtype.Constraint)
