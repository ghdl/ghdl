# =============================================================================
#                ____ _   _ ____  _          _
#   _ __  _   _ / ___| | | |  _ \| |      __| | ___  _ __ ___
#  | '_ \| | | | |  _| |_| | | | | |     / _` |/ _ \| '_ ` _ \
#  | |_) | |_| | |_| |  _  | |_| | |___ | (_| | (_) | | | | | |
#  | .__/ \__, |\____|_| |_|____/|_____(_)__,_|\___/|_| |_| |_|
#  |_|    |___/
# =============================================================================
#  Authors:
#    Patrick Lehmann
#
# Package module:   DOM: Interface items (e.g. generic or port)
#
# License:
# ============================================================================
#  Copyright (C) 2019-2020 Tristan Gingold
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
    PortSignalInterfaceItem as VHDLModel_PortSignalInterfaceItem,
)
from pyVHDLModel.VHDLModel import (
    GenericConstantInterfaceItem as VHDLModel_GenericConstantInterfaceItem,
)

from pyGHDL.dom.Common import GHDLMixin

__all__ = []


@export
class GenericConstantInterfaceItem(VHDLModel_GenericConstantInterfaceItem, GHDLMixin):
    @classmethod
    def parse(cls, generic):
        name = cls._ghdlNodeToName(generic)
        mode = cls._ghdlPortToMode(generic)

        generic = cls(name, mode)

        return generic


@export
class PortSignalInterfaceItem(VHDLModel_PortSignalInterfaceItem, GHDLMixin):
    @classmethod
    def parse(cls, port):
        name = cls._ghdlNodeToName(port)
        mode = cls._ghdlPortToMode(port)

        port = cls(name, mode)

        return port
