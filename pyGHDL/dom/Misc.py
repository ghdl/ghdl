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
# Package module:   DOM: Elements not covered by the VHDL standard.
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
.. todo::
   Add a module documentation.
"""
from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    Alias as VHDLModel_Alias,
)
from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode


__all__ = []


@export
class Alias(VHDLModel_Alias, DOMMixin):
    def __init__(self, node: Iir, aliasName: str):
        super().__init__(aliasName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        aliasName = GetNameOfNode(node)

        # FIXME: add an implementation

        return cls(node, aliasName)
