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
from pyTooling.Decorators import export

from pyVHDLModel.PSLModel import (
    VerificationUnit as VHDLModel_VerificationUnit,
    VerificationProperty as VHDLModel_VerificationProperty,
    VerificationMode as VHDLModel_VerificationMode,
    DefaultClock as VHDLModel_DefaultClock,
)

from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode


__all__ = []


@export
class VerificationUnit(VHDLModel_VerificationUnit, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, vunitNode: Iir):
        name = GetNameOfNode(vunitNode)

        # FIXME: needs an implementation

        return cls(vunitNode, name)


@export
class VerificationProperty(VHDLModel_VerificationProperty, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, vpropNode: Iir):
        name = GetNameOfNode(vpropNode)

        # FIXME: needs an implementation

        return cls(vpropNode, name)


@export
class VerificationMode(VHDLModel_VerificationMode, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, vmodeNode: Iir):
        name = GetNameOfNode(vmodeNode)

        # FIXME: needs an implementation

        return cls(vmodeNode, name)


@export
class DefaultClock(VHDLModel_DefaultClock, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, defaultClockNode: Iir):
        name = GetNameOfNode(defaultClockNode)

        # FIXME: needs an implementation

        return cls(defaultClockNode, name)
