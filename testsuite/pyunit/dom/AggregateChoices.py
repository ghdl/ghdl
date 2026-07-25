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
# Testsuite:        Check libghdl IIR translation of aggregate choices.
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

from pyVHDLModel.Base import Range

from pyGHDL.dom.Expression import (
    IndexedAggregateElement,
    OthersAggregateElement,
    RangedAggregateElement,
)
from pyGHDL.dom.NonStandard import Design, Document


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class AggregateChoices(TestCase):
    """
    Regression tests for aggregate choice lists.

    In ``(b | c => '0')`` only the first choice owns the associated expression; the others carry
    ``Same_Alternative_Flag`` and a null expression. That flag wasn't checked, so translating the second
    choice fed ``Null_Iir`` into ``GetExpressionFromNode`` and raised a ``ValueError``.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/AggregateChoices.vhdl"

    def setUp(self) -> None:
        design = Design()
        self._document = Document(self._filename)
        design.Documents.append(self._document)

        architecture = self._document.Architectures["aggregatechoices"]["rtl"]
        statements = architecture.Statements[0].Statements
        self._recordAggregate = statements[0].Expression
        self._arrayAggregate = statements[1].Expression

    def test_ChoiceListExpandsToOneElementPerChoice(self) -> None:
        elements = self._recordAggregate.Elements

        # `a => '1', b | c => '0'` - three choices, so three elements.
        self.assertEqual(3, len(elements))
        for element in elements:
            self.assertIsInstance(element, IndexedAggregateElement)

    def test_ChoiceListElementsShareTheExpressionValue(self) -> None:
        first, second, third = self._recordAggregate.Elements

        self.assertEqual("1", str(first.Expression))
        self.assertEqual("0", str(second.Expression))
        self.assertEqual("0", str(third.Expression))

    def test_ChoiceListElementsOwnTheirExpression(self) -> None:
        # Each element parents its own expression object, so the grouped choices must not share one.
        for element in self._recordAggregate.Elements:
            self.assertIs(element.Expression.Parent, element)

        second, third = self._recordAggregate.Elements[1:]
        self.assertIsNot(second.Expression, third.Expression)

    def test_IndexedRangedAndOthersChoices(self) -> None:
        indexed, ranged, others = self._arrayAggregate.Elements

        self.assertIsInstance(indexed, IndexedAggregateElement)
        self.assertIsInstance(ranged, RangedAggregateElement)
        self.assertIsInstance(others, OthersAggregateElement)
        self.assertIsInstance(ranged.Range, Range)
