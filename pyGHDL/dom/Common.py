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
# Package module:   DOM: Common classes for package pyGHDL.dom.
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
from pydecor import export

from pyGHDL import GHDLBaseException
from pyGHDL.libghdl import LibGHDLException, errorout_memory

__all__ = []


@export
class DOMException(GHDLBaseException):
    pass


@export
class GHDLException(GHDLBaseException):
    pass


@export
class GHDLMixin:
    def CheckForErrors(self) -> None:
        errorCount = errorout_memory.Get_Nbr_Messages()
        if errorCount != 0:
            for i in range(errorCount):
                print(errorout_memory.Get_Error_Message(i + 1))

            raise DOMException("Error in libghdl.") from LibGHDLException(
                "libghdl: Internal error 2."
            )
