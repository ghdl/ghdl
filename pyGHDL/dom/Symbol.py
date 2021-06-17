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
from pydecor import export

from typing import List

from pyVHDLModel.VHDLModel import (
    SimpleSubTypeSymbol as VHDLModel_SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol as VHDLModel_ConstrainedSubTypeSymbol,
    SimpleObjectSymbol as VHDLModel_SimpleObjectSymbol,
    Constraint,
)

__all__ = []


@export
class SimpleSubTypeSymbol(VHDLModel_SimpleSubTypeSymbol):
    def __init__(self, subTypeName: str):
        super().__init__(subTypeName=subTypeName)


@export
class ConstrainedSubTypeSymbol(VHDLModel_ConstrainedSubTypeSymbol):
    def __init__(self, subTypeName: str, constraints: List[Constraint] = None):
        super().__init__(subTypeName=subTypeName, constraints=constraints)


@export
class SimpleObjectSymbol(VHDLModel_SimpleObjectSymbol):
    def __init__(self, symbolName: str):
        super().__init__(symbolName)
