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
# Package module:   DOM: Common classes for package pyGHDL.dom.
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

"""
.. todo::
   Add a module documentation.
"""
from pydecor import export

from pyVHDLModel.VHDLModel import Mode

from pyGHDL.libghdl import name_table
from pyGHDL.libghdl.vhdl import nodes

__all__ = []


@export
class GHDLBaseException(Exception):
    pass


@export
class LibGHDLException(GHDLBaseException):
    pass


@export
class GHDLException(GHDLBaseException):
    pass


@export
class GHDLMixin:
    _MODE_TRANSLATION = {
        nodes.Iir_Mode.In_Mode: Mode.In,
        nodes.Iir_Mode.Out_Mode: Mode.Out,
        nodes.Iir_Mode.Inout_Mode: Mode.InOut,
        nodes.Iir_Mode.Buffer_Mode: Mode.Buffer,
        nodes.Iir_Mode.Linkage_Mode: Mode.Linkage,
    }

    @classmethod
    def _ghdlNodeToName(cls, node) -> str:
        """Return the python string from node :obj:`node` identifier"""
        return name_table.Get_Name_Ptr(nodes.Get_Identifier(node))

    @classmethod
    def _ghdlPortToMode(cls, port):
        """Return the mode of a :obj:`port`."""
        try:
            return cls._MODE_TRANSLATION[nodes.Get_Mode(port)]
        except KeyError:
            raise LibGHDLException("Unknown mode.")
