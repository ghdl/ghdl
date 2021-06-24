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

from pyVHDLModel.VHDLModel import (
    SimpleName as VHDLModel_SimpleName,
    ParenthesisName as VHDLModel_ParenthesisName,
    IndexedName as VHDLModel_IndexedName,
    SlicedName as VHDLModel_SlicedName,
    SelectedName as VHDLModel_SelectedName,
    AttributeName as VHDLModel_AttributeName,
)

__all__ = []


@export
class SimpleName(VHDLModel_SimpleName):
    pass


@export
class ParenthesisName(VHDLModel_ParenthesisName):
    pass


@export
class IndexedName(VHDLModel_IndexedName):
    pass


@export
class SlicedName(VHDLModel_SlicedName):
    pass


@export
class SelectedName(VHDLModel_SelectedName):
    pass


@export
class AttributeName(VHDLModel_AttributeName):
    pass
