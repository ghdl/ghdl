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
# Package module:   DOM: VHDL design units (e.g. context or package).
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
from pydecor import export

from pyVHDLModel.VHDLModel import (
    SimpleAggregateElement as VHDLModel_SimpleAggregateElement,
    IndexedAggregateElement as VHDLModel_IndexedAggregateElement,
    RangedAggregateElement as VHDLModel_RangedAggregateElement,
    NamedAggregateElement as VHDLModel_NamedAggregateElement,
    OthersAggregateElement as VHDLModel_OthersAggregateElement, Expression
)


__all__ = []



@export
class SimpleAggregateElement(VHDLModel_SimpleAggregateElement):
    def __init__(self, expression: Expression):
        super().__init__()
        self._expression = expression


@export
class IndexedAggregateElement(VHDLModel_IndexedAggregateElement):
	pass


@export
class RangedAggregateElement(VHDLModel_RangedAggregateElement):
	pass


@export
class NamedAggregateElement(VHDLModel_NamedAggregateElement):
    pass


@export
class OthersAggregateElement(VHDLModel_OthersAggregateElement):
	pass
