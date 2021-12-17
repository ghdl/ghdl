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
from typing import List

from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    SimpleName as VHDLModel_SimpleName,
    ParenthesisName as VHDLModel_ParenthesisName,
    IndexedName as VHDLModel_IndexedName,
    SlicedName as VHDLModel_SlicedName,
    SelectedName as VHDLModel_SelectedName,
    AttributeName as VHDLModel_AttributeName,
    AllName as VHDLModel_AllName,
    OpenName as VHDLModel_OpenName,
    Name,
)
from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin

__all__ = []


@export
class SimpleName(VHDLModel_SimpleName, DOMMixin):
    def __init__(self, node: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class ParenthesisName(VHDLModel_ParenthesisName, DOMMixin):
    def __init__(self, node: Iir, prefix: Name, associations: List):
        super().__init__(prefix, associations)
        DOMMixin.__init__(self, node)


@export
class IndexedName(VHDLModel_IndexedName, DOMMixin):
    def __init__(self, node: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class SlicedName(VHDLModel_SlicedName, DOMMixin):
    def __init__(self, node: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)


@export
class SelectedName(VHDLModel_SelectedName, DOMMixin):
    def __init__(self, node: Iir, identifier: str, prefix: Name):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, node)


@export
class AttributeName(VHDLModel_AttributeName, DOMMixin):
    def __init__(self, node: Iir, identifier: str, prefix: Name):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, node)


@export
class AllName(VHDLModel_AllName, DOMMixin):
    def __init__(self, node: Iir, prefix: Name):
        super().__init__(prefix)
        DOMMixin.__init__(self, node)


@export
class OpenName(VHDLModel_OpenName, DOMMixin):
    def __init__(self, node: Iir):
        super().__init__()
        DOMMixin.__init__(self, node)
