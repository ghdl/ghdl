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
# Package module:   DOM: Interface items (e.g. generic or port)
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
This module implements derived range classes from :mod:`pyVHDLModel.Base`.
"""

from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel.Base import Direction, ExpressionUnion
from pyVHDLModel.Base import RangeFromName as VHDLModel_RangeFromName
from pyVHDLModel.Base import SimpleRange as VHDLModel_SimpleRange
from pyVHDLModel.Symbol import Symbol

from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin


@export
class SimpleRange(VHDLModel_SimpleRange, DOMMixin):
    @InheritDocString(VHDLModel_SimpleRange)
    def __init__(
        self, node: Iir, leftBound: ExpressionUnion, rightBound: ExpressionUnion, direction: Direction
    ) -> None:
        super().__init__(leftBound, rightBound, direction)
        DOMMixin.__init__(self, node)


@export
class RangeFromName(VHDLModel_RangeFromName, DOMMixin):
    @InheritDocString(VHDLModel_RangeFromName)
    def __init__(self, node: Iir, symbol: Symbol) -> None:
        super().__init__(symbol)
        DOMMixin.__init__(self, node)
