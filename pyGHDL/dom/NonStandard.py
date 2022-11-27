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
import ctypes
import time
from pathlib import Path
from typing import Any

from pyTooling.Decorators import export

from pyGHDL.dom.Names import SimpleName
from pyVHDLModel.SyntaxModel import (
    Design as VHDLModel_Design,
    Library as VHDLModel_Library,
    Document as VHDLModel_Document,
)

from pyGHDL.libghdl import (
    initialize as libghdl_initialize,
    finalize as libghdl_finalize,
    set_option as libghdl_set_option,
    analyze_init_status as libghdl_analyze_init_status,
    name_table,
    files_map,
    errorout_memory,
    LibGHDLException,
    utils,
    files_map_editor,
)
from pyGHDL.libghdl.vhdl import nodes, sem_lib, parse
from pyGHDL.dom import DOMException, Position
from pyGHDL.dom._Utils import GetIirKindOfNode, CheckForErrors, GetNameOfNode
from pyGHDL.dom.DesignUnit import (
    Entity,
    Architecture,
    Package,
    PackageBody,
    Context,
    Configuration,
    PackageInstantiation,
    LibraryClause,
    UseClause,
    ContextReference,
)
from pyGHDL.dom.PSL import VerificationUnit, VerificationProperty, VerificationMode

__all__ = []


@export
class Design(VHDLModel_Design):
    def __init__(self):
        super().__init__()

        self.__ghdl_init()

    def __ghdl_init(self):
        """Initialization: set options and then load libraries."""
        # Initialize libghdl
        libghdl_finalize()
        libghdl_initialize()

        # Collect error messages in memory
        errorout_memory.Install_Handler()

        libghdl_set_option("--std=08")
        libghdl_set_option("--ams")

        parse.Flag_Parse_Parenthesis.value = True

        # Finish initialization. This will load the standard package.
        if libghdl_analyze_init_status() != 0:
            raise LibGHDLException("Error initializing 'libghdl'.")


@export
class Library(VHDLModel_Library):
    pass


@export
class Document(VHDLModel_Document):
    _filename: Path
    __ghdlFileID: Any
    __ghdlSourceFileEntry: Any
    __ghdlFile: Any

    __ghdlProcessingTime: float
    __domTranslateTime: float

    def __init__(
        self,
        path: Path,
        sourceCode: str = None,
        dontParse: bool = False,
        dontTranslate: bool = False,
    ):
        super().__init__(path)

        self._filename = path

        if sourceCode is None:
            self.__loadFromPath()
        else:
            self.__loadFromString(sourceCode)

        if dontParse == False:
            # Parse input file
            t1 = time.perf_counter()
            self.__ghdlFile = sem_lib.Load_File(self.__ghdlSourceFileEntry)
            CheckForErrors()
            self.__ghdlProcessingTime = time.perf_counter() - t1

            if dontTranslate == False:
                t1 = time.perf_counter()
                self.translate()
                self.__domTranslateTime = time.perf_counter() - t1

    def __loadFromPath(self):
        with self._filename.open("r", encoding="utf-8") as file:
            self.__loadFromString(file.read())

    def __loadFromString(self, sourceCode: str):
        sourcesBytes = sourceCode.encode("utf-8")
        sourceLength = len(sourcesBytes)
        bufferLength = sourceLength + 128
        self.__ghdlFileID = name_table.Get_Identifier(str(self._filename))
        dirId = name_table.Null_Identifier
        self.__ghdlSourceFileEntry = files_map.Reserve_Source_File(dirId, self.__ghdlFileID, bufferLength)
        files_map_editor.Fill_Text(self.__ghdlSourceFileEntry, ctypes.c_char_p(sourcesBytes), sourceLength)

        CheckForErrors()

    def translate(self):
        firstUnit = nodes.Get_First_Design_Unit(self.__ghdlFile)

        for unit in utils.chain_iter(firstUnit):
            libraryUnit = nodes.Get_Library_Unit(unit)
            nodeKind = GetIirKindOfNode(libraryUnit)

            contextItems = []
            contextNames = []
            context = nodes.Get_Context_Items(unit)
            if context is not nodes.Null_Iir:
                for item in utils.chain_iter(context):
                    itemKind = GetIirKindOfNode(item)
                    if itemKind is nodes.Iir_Kind.Library_Clause:
                        contextNames.append(SimpleName(item, GetNameOfNode(item)))
                        if nodes.Get_Has_Identifier_List(item):
                            continue

                        contextItems.append(LibraryClause(item, contextNames))
                        contextNames = []
                    elif itemKind is nodes.Iir_Kind.Use_Clause:
                        contextItems.append(UseClause.parse(item))
                    elif itemKind is nodes.Iir_Kind.Context_Reference:
                        contextItems.append(ContextReference.parse(item))
                    else:
                        pos = Position.parse(item)
                        raise DOMException(
                            f"Unknown context item kind '{itemKind.name}' in context at line {pos.Line}."
                        )

            if nodeKind == nodes.Iir_Kind.Entity_Declaration:
                entity = Entity.parse(libraryUnit, contextItems)
                self.Entities.append(entity)

            elif nodeKind == nodes.Iir_Kind.Architecture_Body:
                architecture = Architecture.parse(libraryUnit, contextItems)
                self.Architectures.append(architecture)

            elif nodeKind == nodes.Iir_Kind.Package_Declaration:
                package = Package.parse(libraryUnit, contextItems)
                self.Packages.append(package)

            elif nodeKind == nodes.Iir_Kind.Package_Body:
                packageBody = PackageBody.parse(libraryUnit, contextItems)
                self.PackageBodies.append(packageBody)

            elif nodeKind == nodes.Iir_Kind.Package_Instantiation_Declaration:
                package = PackageInstantiation.parse(libraryUnit)
                self.Packages.append(package)

            elif nodeKind == nodes.Iir_Kind.Context_Declaration:
                context = Context.parse(libraryUnit)
                self.Contexts.append(context)

            elif nodeKind == nodes.Iir_Kind.Configuration_Declaration:
                configuration = Configuration.parse(libraryUnit, contextItems)
                self.Configurations.append(configuration)

            elif nodeKind == nodes.Iir_Kind.Vunit_Declaration:
                vunit = VerificationUnit.parse(libraryUnit)
                self.VerificationUnits.append(vunit)

            elif nodeKind == nodes.Iir_Kind.Vprop_Declaration:
                vprop = VerificationProperty.parse(libraryUnit)
                self.VerificationProperties.append(vprop)

            elif nodeKind == nodes.Iir_Kind.Vmode_Declaration:
                vmod = VerificationMode.parse(libraryUnit)
                self.VerificationModes.append(vmod)

            else:
                raise DOMException(f"Unknown design unit kind '{nodeKind.name}'.")

    @property
    def LibGHDLProcessingTime(self) -> float:
        return self.__ghdlProcessingTime

    @property
    def DOMTranslationTime(self) -> float:
        return self.__domTranslateTime
