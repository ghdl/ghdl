# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:
#   Tristan Gingold
#   Patrick Lehmann
#
# Package module:   Python binding and low-level API for shared library 'libghdl'.
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

from ctypes import c_bool, sizeof, c_int
from enum import IntEnum, unique

from pyTooling.Decorators import export
from pyTooling.MetaClasses import ExtendedType

from pyGHDL.libghdl import libghdl


assert sizeof(c_bool) == 1


@export
@unique
class VhdlStandard(IntEnum):
    """An enumeration representing libghdl's internal ``Vhdl_Std_Type`` enumeration type."""

    Vhdl_87 = 0   #: VHDL'87
    Vhdl_93 = 1   #: VHDL'93
    Vhdl_00 = 2   #: VHDL'2000
    Vhdl_02 = 3   #: VHDL'2002
    Vhdl_08 = 4   #: VHDL'2008
    Vhdl_19 = 5   #: VHDL'2019


@export
class Flags(metaclass=ExtendedType, singleton=True):
    __Elocations = c_bool.in_dll(libghdl, "flags__flag_elocations")
    __Verbose = c_bool.in_dll(libghdl, "flags__verbose")
    __MB_Comment = c_bool.in_dll(libghdl, "flags__mb_comment")
    __Explicit = c_bool.in_dll(libghdl, "flags__flag_explicit")
    __Relaxed = c_bool.in_dll(libghdl, "flags__flag_relaxed_rules")

    __Elaborate_With_Outdated = c_bool.in_dll(libghdl, "flags__flag_elaborate_with_outdated")
    __Force_Analysis = c_bool.in_dll(libghdl, "flags__flag_force_analysis")
    __Vhdl_Std = c_int.in_dll(libghdl, "flags__vhdl_std")
    __AMS_Vhdl = c_bool.in_dll(libghdl, "flags__ams_vhdl")

    @property
    def Elocations(self) -> bool:
        """
        If set to true, the parser builds extended locations (defined in Ada package elocations). This saves possibly many
        locations per node, so it uses more memory.  Useful when a tool (like a style checker) wants to know the precise layout.
        Not used to report errors.
        """
        return self.__Elocations.value

    @Elocations.setter
    def Elocations(self, value: bool):
        self.__Elocations.value = value

    @property
    def Verbose(self) -> bool:
        """
        Internal boolean flag representing :option:`-v`.
        """
        return self.__Verbose.value

    @Verbose.setter
    def Verbose(self, value: bool):
        self.__Verbose.value = value


    @property
    def MB_Comment(self) -> bool:
        """
        If set, a multi-bytes sequence can appear in a comment, i.e. all characters except ``VT``, ``CR``, ``LF`` and ``FF`` are allowed in a comment.

        Internal boolean flag representing CLI option :option:`--mb-comment`.
        """
        return self.__MB_Comment.value

    @MB_Comment.setter
    def MB_Comment(self, value: bool):
        self.__MB_Comment.value = value


    @property
    def Explicit(self) -> bool:
        """
        If set, explicit subprogram declarations take precedence over implicit declarations, even through use clauses.

        Internal boolean flag representing CLI option :option:`-fexplicit`.
        """
        return self.__Explicit.value

    @Explicit.setter
    def Explicit(self, value: bool):
        self.__Explicit.value = value


    @property
    def Relaxed(self) -> bool:
        """
        If true, relax some rules:

         * the scope of an object declaration names start after the declaration, so that it is possible to use the old name in the default expression:

           .. code-block:: vhdl

              constant x : xtype := x;

        Internal boolean flag representing CLI option :option:`-frelaxed`.
        """
        return self.__Relaxed.value

    @Relaxed.setter
    def Relaxed(self, value: bool):
        self.__Relaxed.value = value


    @property
    def Elaborate_With_Outdated (self) -> bool:
        """
        If set, a default aspect entity aspect might be an outdated unit. Used by ghdldrv.
        """
        return self.__Elaborate_With_Outdated .value

    @Elaborate_With_Outdated .setter
    def Elaborate_With_Outdated (self, value: bool):
        self.__Elaborate_With_Outdated .value = value


    @property
    def Force_Analysis(self) -> bool:
        """
        Set if analysis is done even after parsing errors. The analysis code that handles and tolerates incorrect parse tree
        should check that this flag is set.
        """
        return self.__Force_Analysis.value

    @Force_Analysis.setter
    def Force_Analysis(self, value: bool):
        self.__Force_Analysis.value = value


    @property
    def Vhdl_Std(self) -> VhdlStandard:
        """
        Standard accepted.

        Internal boolean flag representing CLI option :option:`--std=<STANDARD>`.
        """
        return VhdlStandard(self.__Vhdl_Std.value)

    @Vhdl_Std.setter
    def Vhdl_Std(self, value: VhdlStandard):
        self.__Vhdl_Std.value = int(value)


    @property
    def AMS_Vhdl(self) -> bool:
        """
        Enable VHDL-AMS extensions.

        Internal boolean flag representing CLI option :option:`--ams`.
        """
        return self.__AMS_Vhdl.value

    @AMS_Vhdl.setter
    def AMS_Vhdl(self, value: bool):
        self.__AMS_Vhdl.value = value
