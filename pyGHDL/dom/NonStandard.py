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
#  Copyright (C) 2019-2022 Tristan Gingold
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
from typing import Any, Optional as Nullable

from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel import VHDLVersion, IEEEFlavor
from pyVHDLModel import Design as VHDLModel_Design
from pyVHDLModel import Library as VHDLModel_Library
from pyVHDLModel import Document as VHDLModel_Document

from pyGHDL.libghdl import (
    ENCODING,
    initialize as libghdl_initialize,
    finalize as libghdl_finalize,
    set_option as libghdl_set_option,
    analyze_init_status as libghdl_analyze_init_status,
    name_table,
    files_map,
    errorout_memory,
    LibGHDLException,
    flags,
    utils,
    files_map_editor,
)
from pyGHDL.libghdl.flags import Flag_Gather_Comments
from pyGHDL.libghdl.vhdl import nodes, sem_lib
from pyGHDL.libghdl.vhdl.parse import Flag_Parse_Parenthesis
from pyGHDL.dom import DOMException, Position
from pyGHDL.dom._Utils import GetIirKindOfNode, CheckForErrors, GetNameOfNode, GetDocumentationOfNode
from pyGHDL.dom.Name import SimpleName
from pyGHDL.dom.Symbol import LibraryReferenceSymbol
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


@export
class Design(VHDLModel_Design):
    _loadDefaultLibraryTime: Nullable[float]
    _analyzeTime: Nullable[float]

    @InheritDocString(VHDLModel_Design)
    def __init__(self, name: str = None) -> None:
        super().__init__(name)

        self._loadDefaultLibraryTime = None
        self._analyzeTime = None

        self.__ghdl_init()

    def __ghdl_init(self):
        """Initialization: set options and then load libraries."""
        # Initialize libghdl
        libghdl_finalize()
        libghdl_initialize()

        # Collect error messages in memory
        errorout_memory.Install_Handler()

        libghdl_set_option("--std=08")

        Flag_Gather_Comments.value = True
        Flag_Parse_Parenthesis.value = True

        # Finish initialization. This will load the standard package.
        if libghdl_analyze_init_status() != 0:
            raise DOMException("Error initializing 'pyGHDL.dom'.") from LibGHDLException(
                "Error initializing 'libghdl'."
            )

    def LoadDefaultLibraries(self, flavor: Nullable[IEEEFlavor] = None):
        t1 = time.perf_counter()

        super().LoadStdLibrary()
        super().LoadIEEELibrary(flavor)

        self._loadDefaultLibraryTime = time.perf_counter() - t1

    def Analyze(self):
        t1 = time.perf_counter()

        super().Analyze()

        self._analyzeTime = time.perf_counter() - t1


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
        vhdlVersion: VHDLVersion = VHDLVersion.VHDL2008,
        dontParse: bool = False,
        dontTranslate: bool = False,
    ) -> None:
        super().__init__(path)

        self._filename = path

        if sourceCode is None:
            self.__loadFromPath()
        else:
            self.__loadFromString(sourceCode)

        if not dontParse:
            # Parse input file
            t1 = time.perf_counter()

            if vhdlVersion.IsAMS:
                flags.AMS_Vhdl.value = True

            self.__ghdlFile = sem_lib.Load_File(self.__ghdlSourceFileEntry)
            CheckForErrors()

            if vhdlVersion.IsAMS:
                flags.AMS_Vhdl.value = False

            self.__ghdlProcessingTime = time.perf_counter() - t1

            if not dontTranslate:
                t1 = time.perf_counter()
                self.translate()
                self.__domTranslateTime = time.perf_counter() - t1

    def __loadFromPath(self):
        try:
            with self._filename.open("r", encoding=ENCODING) as file:
                self.__loadFromString(file.read())
        except FileNotFoundError as ex:
            raise DOMException(f"Sourcefile '{self._filename}' not found.") from ex

    def __loadFromString(self, sourceCode: str):
        sourcesBytes = sourceCode.encode(ENCODING)
        sourceLength = len(sourcesBytes)
        bufferLength = sourceLength + 128
        dirId = name_table.Null_Identifier
        self.__ghdlFileID = name_table.Get_Identifier(str(self._filename))
        if files_map.Find_Source_File(dirId, self.__ghdlFileID) == files_map.No_Source_File_Entry:
            self.__ghdlSourceFileEntry = files_map.Reserve_Source_File(dirId, self.__ghdlFileID, bufferLength)
            files_map_editor.Fill_Text(self.__ghdlSourceFileEntry, ctypes.c_char_p(sourcesBytes), sourceLength)

            CheckForErrors()
        else:
            raise DOMException(f"Source file '{self._filename}' already loaded.")

    def translate(self):
        firstUnit = nodes.Get_First_Design_Unit(self.__ghdlFile)
        self._documentation = GetDocumentationOfNode(firstUnit)

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
                        libraryIdentifier = GetNameOfNode(item)
                        contextNames.append(LibraryReferenceSymbol(item, SimpleName(item, libraryIdentifier)))
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
                self._AddEntity(entity)

            elif nodeKind == nodes.Iir_Kind.Architecture_Body:
                architecture = Architecture.parse(libraryUnit, contextItems)
                self._AddArchitecture(architecture)

            elif nodeKind == nodes.Iir_Kind.Package_Declaration:
                package = Package.parse(libraryUnit, contextItems)
                self._AddPackage(package)

            elif nodeKind == nodes.Iir_Kind.Package_Body:
                packageBody = PackageBody.parse(libraryUnit, contextItems)
                self._AddPackageBody(packageBody)

            elif nodeKind == nodes.Iir_Kind.Package_Instantiation_Declaration:
                package = PackageInstantiation.parse(libraryUnit)
                self._AddPackage(package)

            elif nodeKind == nodes.Iir_Kind.Context_Declaration:
                context = Context.parse(libraryUnit)
                self._AddContext(context)

            elif nodeKind == nodes.Iir_Kind.Configuration_Declaration:
                configuration = Configuration.parse(libraryUnit, contextItems)
                self._AddConfiguration(configuration)

            elif nodeKind == nodes.Iir_Kind.Vunit_Declaration:
                vunit = VerificationUnit.parse(libraryUnit)
                self._AddVerificationUnit(vunit)

            elif nodeKind == nodes.Iir_Kind.Vprop_Declaration:
                vprop = VerificationProperty.parse(libraryUnit)
                self._AddVerificationProperty(vprop)

            elif nodeKind == nodes.Iir_Kind.Vmode_Declaration:
                vmod = VerificationMode.parse(libraryUnit)
                self._AddVerificationMode(vmod)

            else:
                raise DOMException(f"Unknown design unit kind '{nodeKind.name}'.")

    @property
    def LibGHDLProcessingTime(self) -> float:
        return self.__ghdlProcessingTime

    @property
    def DOMTranslationTime(self) -> float:
        return self.__domTranslateTime
