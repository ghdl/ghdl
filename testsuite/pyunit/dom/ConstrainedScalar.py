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
from pyGHDL.dom.Range import RangeFromName
from pyGHDL.dom.Symbol import ConstrainedScalarSubtypeSymbol, RangeAttributeSymbol, SimpleSubtypeSymbol


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

    def test_LiteralBoundsRange(self) -> None:
        """``signal s : integer range 0 to 15;`` - plain integer literal bounds."""
        architecture = self._architecture()
        signal = architecture.DeclaredItems[2]

        self.assertIsInstance(signal.Subtype, ConstrainedScalarSubtypeSymbol)
        self.assertIsNotNone(signal.Subtype.Constraint)

    def test_ExpressionBoundsRange(self) -> None:
        """``signal t : natural range 0 to max - 1;`` - one bound is an expression, not a literal."""
        architecture = self._architecture()
        signal = architecture.DeclaredItems[3]

        self.assertIsInstance(signal.Subtype, ConstrainedScalarSubtypeSymbol)
        self.assertIsNotNone(signal.Subtype.Constraint)

    def test_EnumerationRange(self) -> None:
        """``signal u : color_t range red to blue;`` - range over enumeration literals, not a scalar
        numeric type."""
        architecture = self._architecture()
        signal = architecture.DeclaredItems[4]

        self.assertIsInstance(signal.Subtype, ConstrainedScalarSubtypeSymbol)
        self.assertIsNotNone(signal.Subtype.Constraint)

    def test_RangeAttributeRange(self) -> None:
        """``subtype index_t is natural range v'range;`` - the range constraint is a range attribute
        rather than explicit bounds, which GHDL reports as an ``Attribute_Name``."""
        architecture = self._architecture()
        subtype = architecture.DeclaredItems[6]

        self.assertIsInstance(subtype.Type, ConstrainedScalarSubtypeSymbol)
        self.assertIsInstance(subtype.Type.Constraint, RangeFromName)
        self.assertIsInstance(subtype.Type.Constraint.Symbol, RangeAttributeSymbol)

    def test_ResolutionIndicationWithoutRange(self) -> None:
        """``subtype resolved_t is resolveBit myBit;`` - a subtype indication that only adds a
        resolution function. GHDL reports it as a ``Subtype_Definition`` too, but there is no range
        constraint, so it is not a constrained scalar subtype."""
        architecture = self._architecture()
        subtype = architecture.DeclaredItems[10]

        self.assertIsInstance(subtype.Type, SimpleSubtypeSymbol)
        self.assertNotIsInstance(subtype.Type, ConstrainedScalarSubtypeSymbol)
