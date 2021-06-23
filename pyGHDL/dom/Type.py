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

from pyGHDL.dom.Range import Range
from pyVHDLModel.VHDLModel import (
    IntegerType as VHDLModel_IntegerType,
    EnumeratedType as VHDLModel_EnumeratedType,
    ArrayType as VHDLModel_ArrayType,
    RecordTypeElement as VHDLModel_RecordTypeElement,
    RecordType as VHDLModel_RecordType,
    AccessType as VHDLModel_AccessType,
    SubType as VHDLModel_SubType,
    Expression,
)


@export
class IntegerType(VHDLModel_IntegerType):
    def __init__(self, typeName: str, range: Range):
        super().__init__(typeName)
        self._leftBound = range.LeftBound
        self._rightBound = range.RightBound


@export
class EnumeratedType(VHDLModel_EnumeratedType):
    pass


@export
class ArrayType(VHDLModel_ArrayType):
    pass


@export
class RecordTypeElement(VHDLModel_RecordTypeElement):
    pass


@export
class RecordType(VHDLModel_RecordType):
    pass


@export
class AccessType(VHDLModel_AccessType):
    pass


@export
class SubType(VHDLModel_SubType):
    def __init__(self, subtypeName: str):
        super().__init__(subtypeName)
