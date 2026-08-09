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
# Package module:   DOM: Aggregates.
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

"""
This module contains all DOM classes for VHDL's design units (:class:`context <Entity>`,
:class:`architecture <Architecture>`, :class:`package <Package>`,
:class:`package body <PackageBody>`, :class:`context <Context>` and
:class:`configuration <Configuration>`.


"""

from pyTooling.Decorators import export

from pyVHDLModel.Base import ExpressionUnion, Range
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Expression import SimpleAggregateElement as VHDLModel_SimpleAggregateElement
from pyVHDLModel.Expression import IndexedAggregateElement as VHDLModel_IndexedAggregateElement
from pyVHDLModel.Expression import RangedAggregateElement as VHDLModel_RangedAggregateElement
from pyVHDLModel.Expression import NamedAggregateElement as VHDLModel_NamedAggregateElement
from pyVHDLModel.Expression import OthersAggregateElement as VHDLModel_OthersAggregateElement

from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin


@export
class SimpleAggregateElement(VHDLModel_SimpleAggregateElement, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion) -> None:
        """
        Initializes a simple aggregate element.

        :param node:       The IIR node this object was translated from.
        :param expression: The expression this aggregate element supplies.
        """
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class IndexedAggregateElement(VHDLModel_IndexedAggregateElement, DOMMixin):
    def __init__(self, node: Iir, index: ExpressionUnion, expression: ExpressionUnion) -> None:
        """
        Initializes an aggregate element chosen by an index.

        :param node:       The IIR node this object was translated from.
        :param index:      The index selecting the element this value is assigned to.
        :param expression: The expression this aggregate element supplies.
        """
        super().__init__(index, expression)
        DOMMixin.__init__(self, node)


@export
class RangedAggregateElement(VHDLModel_RangedAggregateElement, DOMMixin):
    def __init__(self, node: Iir, rng: Range, expression: ExpressionUnion) -> None:
        """
        Initializes an aggregate element chosen by a range.

        :param node:       The IIR node this object was translated from.
        :param rng:        The range selecting the elements this value is assigned to.
        :param expression: The expression this aggregate element supplies.
        """
        super().__init__(rng, expression)
        DOMMixin.__init__(self, node)


@export
class NamedAggregateElement(VHDLModel_NamedAggregateElement, DOMMixin):
    def __init__(self, node: Iir, name: Symbol, expression: ExpressionUnion) -> None:
        """
        Initializes an aggregate element chosen by a name.

        :param node:       The IIR node this object was translated from.
        :param name:       Reference to the name selecting the element this value is assigned to.
        :param expression: The expression this aggregate element supplies.
        """
        super().__init__(name, expression)
        DOMMixin.__init__(self, node)


@export
class OthersAggregateElement(VHDLModel_OthersAggregateElement, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion) -> None:
        """
        Initializes an :vhdlkw:`others` aggregate element.

        :param node:       The IIR node this object was translated from.
        :param expression: The expression this aggregate element supplies.
        """
        super().__init__(expression)
        DOMMixin.__init__(self, node)
