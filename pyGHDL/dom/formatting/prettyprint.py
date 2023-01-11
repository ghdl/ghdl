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
# Package module:   A pretty printer to format the DOM as a tree in text form.
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
from typing import List, Union

from pyTooling.Decorators import export

from pyVHDLModel.Base import NamedEntityMixin
from pyVHDLModel.Interface import GenericInterfaceItem, PortInterfaceItem
from pyVHDLModel.Subprogram import Function
from pyVHDLModel.Object import BaseConstant, WithDefaultExpressionMixin
from pyVHDLModel.Type import BaseType, FullType
from pyVHDLModel.Concurrent import ConcurrentStatement

from pyGHDL import GHDLBaseException
from pyGHDL.dom.NonStandard import Document, Design, Library
from pyGHDL.dom.Concurrent import (
    ConcurrentBlockStatement,
    ProcessStatement,
    IfGenerateStatement,
    CaseGenerateStatement,
    ForGenerateStatement,
    ComponentInstantiation,
    ConfigurationInstantiation,
    EntityInstantiation,
    ConcurrentProcedureCall,
)
from pyGHDL.dom.DesignUnit import (
    Entity,
    Architecture,
    Package,
    PackageBody,
    Configuration,
    Context,
    Component,
    UseClause,
    PackageInstantiation,
)
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol, ConstrainedCompositeSubtypeSymbol
from pyGHDL.dom.Type import (
    IntegerType,
    Subtype,
    ArrayType,
    RecordType,
    AccessType,
    EnumeratedType,
    FileType,
    ProtectedType,
    ProtectedTypeBody,
    PhysicalType,
    IncompleteType,
)
from pyGHDL.dom.InterfaceItem import (
    GenericConstantInterfaceItem,
    PortSignalInterfaceItem,
    GenericTypeInterfaceItem,
)
from pyGHDL.dom.Object import Constant, Signal, SharedVariable, File
from pyGHDL.dom.Attribute import Attribute, AttributeSpecification
from pyGHDL.dom.Subprogram import Procedure
from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.PSL import DefaultClock


StringBuffer = List[str]


@export
class PrettyPrintException(GHDLBaseException):
    pass


@export
class PrettyPrint:
    # _buffer: StringBuffer
    #
    # def __init__(self):
    #     self._buffer = []

    def CleanupDocumentationBlocks(self, documentationContent: str, level: int = 0):
        prefix = "  " * level
        if documentationContent is None:
            return prefix

        documentationLines = documentationContent.split("\n")
        return f"{prefix}{documentationLines[0][2:].lstrip()}"

    def formatDesign(self, design: Design, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}Libraries ({len(design.Libraries)}):")
        for library in design.Libraries.values():
            buffer.append(f"{prefix}  - Name: {library.Identifier}")
            for line in self.formatLibrary(library, level + 2):
                buffer.append(line)
        buffer.append(f"{prefix}Documents ({len(design.Documents)}):")
        for document in design.Documents:
            buffer.append(f"{prefix}  - Path: '{document.Path}':")
            for line in self.formatDocument(document, level + 2):
                buffer.append(line)

        return buffer

    def formatLibrary(self, library: Library, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}Contexts ({len(library.Contexts)}):")
        for context in library.Contexts.values():
            buffer.append(f"{prefix}  - {context.Identifier}")
        buffer.append(f"{prefix}Packages ({len(library.Packages)}):")
        for package in library.Packages.values():
            if isinstance(package, Package):
                buffer.append(f"{prefix}  - {package.Identifier}")
            elif isinstance(package, PackageInstantiation):
                buffer.append(f"{prefix}  - {package.Identifier} instantiate from {package.PackageReference}")
        buffer.append(f"{prefix}Entities ({len(library.Entities)}):")
        for entity in library.Entities.values():
            buffer.append(f"{prefix}  - {entity.Identifier}({', '.join([a.Identifier for a in entity.Architectures])})")
        buffer.append(f"{prefix}Configurations ({len(library.Configurations)}):")
        for configuration in library.Configurations.values():
            buffer.append(f"{prefix}  - {configuration.Identifier}")

        return buffer

    def formatDocument(self, document: Document, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}Contexts ({len(document.Contexts)}):")
        for context in document.Contexts.values():
            for line in self.formatContext(context, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}Packages ({len(document.Packages)}):")
        for package in document.Packages.values():
            if isinstance(package, Package):
                gen = self.formatPackage
            else:
                gen = self.formatPackageInstance

            for line in gen(package, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}PackageBodies ({len(document.PackageBodies)}):")
        for packageBodies in document.PackageBodies.values():
            for line in self.formatPackageBody(packageBodies, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}Entities ({len(document.Entities)}):")
        for entity in document.Entities.values():
            for line in self.formatEntity(entity, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}Architectures ({len(document.Architectures)}):")
        for architectures in document.Architectures.values():
            for architecture in architectures.values():
                for line in self.formatArchitecture(architecture, level + 1):
                    buffer.append(line)
        buffer.append(f"{prefix}Configurations ({len(document.Configurations)}):")
        for configuration in document.Configurations.values():
            for line in self.formatConfiguration(configuration, level + 1):
                buffer.append(line)

        return buffer

    def formatEntity(self, entity: Entity, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(entity.Documentation)
        buffer.append(
            f"{prefix}- Name: {entity.Identifier}\n"
            f"{prefix}  File: {entity.Position.Filename.name}\n"
            f"{prefix}  Position: {entity.Position.Line}:{entity.Position.Column}\n"
            f"{prefix}  Documentation: {documentationFirstLine}"
        )
        buffer.append(f"{prefix}  Generics:")
        for generic in entity.GenericItems:
            for line in self.formatGeneric(generic, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}  Ports:")
        for port in entity.PortItems:
            for line in self.formatPort(port, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}  Declared:")
        for item in entity.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}  Statements:")
        for item in entity.Statements:
            buffer.append(f"{prefix}    ...")
        buffer.append(f"{prefix}  Architectures:")
        for item in entity.Architectures:
            buffer.append(f"{prefix}  - {item.Identifier}")

        return buffer

    def formatArchitecture(self, architecture: Architecture, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(architecture.Documentation)
        buffer.append(
            f"{prefix}- Name: {architecture.Identifier}\n"
            f"{prefix}  File: {architecture.Position.Filename.name}\n"
            f"{prefix}  Position: {architecture.Position.Line}:{architecture.Position.Column}\n"
            f"{prefix}  Documentation: {documentationFirstLine}"
        )
        buffer.append(f"{prefix}  Entity: {architecture.Entity.Identifier}")
        buffer.append(f"{prefix}  Declared:")
        for item in architecture.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 2):
                buffer.append(line)
        buffer.append(f"{prefix}  Hierarchy:")
        for item in architecture.Statements:
            for line in self.formatHierarchy(item, level + 2):
                buffer.append(line)
        buffer.append(f"{prefix}  Statements:")
        for item in architecture.Statements:
            buffer.append(f"{prefix}    ...")
        #            for line in self.formatStatements(item, level + 2):
        #                buffer.append(line)

        return buffer

    def formatComponent(self, component: Component, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(component.Documentation)
        buffer.append(f"{prefix}- Component: {component.Identifier}")
        buffer.append(f"{prefix}  Generics:")
        for generic in component.GenericItems:
            for line in self.formatGeneric(generic, level + 1):
                buffer.append(line)
        buffer.append(f"{prefix}  Ports:")
        for port in component.PortItems:
            for line in self.formatPort(port, level + 1):
                buffer.append(line)

        return buffer

    def formatPackage(self, package: Package, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(package.Documentation)
        buffer.append(
            f"{prefix}- Name: {package.Identifier}\n"
            f"{prefix}  File: {package.Position.Filename.name}\n"
            f"{prefix}  Position: {package.Position.Line}:{package.Position.Column}\n"
            f"{prefix}  Documentation: {documentationFirstLine}"
        )
        buffer.append(f"{prefix}  Declared:")
        for item in package.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)

        return buffer

    def formatPackageInstance(self, package: PackageInstantiation, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(package.Documentation)
        buffer.append(f"{prefix}- Name: {package.Identifier}")
        buffer.append(f"{prefix}  Package: {package.PackageReference!s}")
        buffer.append(f"{prefix}  Generic Map: ...")
        #        for item in package.GenericItems:
        #            for line in self.formatGeneric(item, level + 1):
        #                buffer.append(line)

        return buffer

    def formatPackageBody(self, packageBody: PackageBody, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(packageBody.Documentation)
        buffer.append(f"{prefix}- Name: {packageBody.Identifier}\n{prefix}  Documentation: {documentationFirstLine}")
        buffer.append(f"{prefix}  Declared:")
        for item in packageBody.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)

        return buffer

    def formatConfiguration(self, configuration: Configuration, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}- Name: {configuration.Identifier}")

        return buffer

    def formatContext(self, context: Context, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}- Name: {context.Identifier}")

        return buffer

    def formatGeneric(self, generic: Union[NamedEntityMixin, GenericInterfaceItem], level: int = 0) -> StringBuffer:
        if isinstance(generic, GenericConstantInterfaceItem):
            return self.formatGenericConstant(generic, level)
        elif isinstance(generic, GenericTypeInterfaceItem):
            return self.formatGenericType(generic, level)
        else:
            raise PrettyPrintException(
                f"Unhandled generic kind '{generic.__class__.__name__}' for generic '{generic.Identifiers[0]}'."
            )

    def formatPort(self, port: Union[NamedEntityMixin, PortInterfaceItem], level: int = 0) -> StringBuffer:
        if isinstance(port, PortSignalInterfaceItem):
            return self.formatPortSignal(port, level)
        else:
            raise PrettyPrintException(
                f"Unhandled port kind '{port.__class__.__name__}' for port '{port.Identifiers[0]}'."
            )

    def formatGenericConstant(self, generic: GenericConstantInterfaceItem, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        subTypeIndication = self.formatSubtypeIndication(generic.Subtype, "generic", generic.Identifiers[0])
        buffer.append(
            f"{prefix}  - {', '.join(generic.Identifiers)} : {generic.Mode!s} {subTypeIndication}{self.formatInitialValue(generic)}"
        )

        return buffer

    def formatGenericType(self, generic: GenericConstantInterfaceItem, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        buffer.append(f"{prefix}  - type {generic.Identifier}")

        return buffer

    def formatPortSignal(self, port: PortSignalInterfaceItem, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        subTypeIndication = self.formatSubtypeIndication(port.Subtype, "port", port.Identifiers[0])
        buffer.append(
            f"{prefix}  - {', '.join(port.Identifiers)} : {port.Mode} {subTypeIndication}{self.formatInitialValue(port)}"
        )

        return buffer

    def formatDeclaredItems(self, item, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        if isinstance(item, BaseConstant):
            subTypeIndication = self.formatSubtypeIndication(item.Subtype, "constant", item.Identifiers[0])
            initValue = f" := {item.DefaultExpression}" if isinstance(item, Constant) else ""
            buffer.append(f"{prefix}- constant {', '.join(item.Identifiers)} : {subTypeIndication}{initValue}")
        elif isinstance(item, SharedVariable):
            subTypeIndication = self.formatSubtypeIndication(item.Subtype, "shared variable", item.Identifiers[0])
            buffer.append(f"{prefix}- shared variable {', '.join(item.Identifiers)} : {subTypeIndication}")
        elif isinstance(item, Signal):
            subTypeIndication = self.formatSubtypeIndication(item.Subtype, "signal", item.Identifiers[0])
            initValue = f" := {item.DefaultExpression}" if item.DefaultExpression is not None else ""
            buffer.append(f"{prefix}- signal {', '.join(item.Identifiers)} : {subTypeIndication}{initValue}")
        elif isinstance(item, File):
            subTypeIndication = self.formatSubtypeIndication(item.Subtype, "file", item.Identifiers[0])
            buffer.append(f"{prefix}- File {', '.join(item.Identifiers)} : {subTypeIndication}")
        elif isinstance(item, (FullType, IncompleteType)):
            buffer.append(f"{prefix}- {self.formatType(item)}")
        elif isinstance(item, Subtype):
            buffer.append(f"{prefix}- subtype {item.Identifier} is ?????")
        elif isinstance(item, Alias):
            buffer.append(f"{prefix}- alias {item.Identifier} is ?????")
        elif isinstance(item, Function):
            buffer.append(f"{prefix}- function {item.Identifier} return {item.ReturnType}")
        elif isinstance(item, Procedure):
            buffer.append(f"{prefix}- procedure {item.Identifier}")
        elif isinstance(item, Component):
            for line in self.formatComponent(item, level):
                buffer.append(line)
        elif isinstance(item, Attribute):
            buffer.append(f"{prefix}- attribute {item.Identifier} : {item.Subtype}")
        elif isinstance(item, AttributeSpecification):
            buffer.append(f"{prefix}- attribute {item.Attribute} of {'????'} : {'????'} is {'????'}")
        elif isinstance(item, UseClause):
            buffer.append(f"{prefix}- use {', '.join([str(n) for n in item.Names])}")
        elif isinstance(item, Package):
            buffer.append(f"{prefix}- package {item.Identifier} is ..... end package")
        elif isinstance(item, PackageInstantiation):
            buffer.append(f"{prefix}- package {item.Identifier} is new {item.PackageReference} generic map (.....)")
        elif isinstance(item, DefaultClock):
            buffer.append(f"{prefix}- default {item.Identifier} is {'...'}")
        else:
            raise PrettyPrintException(f"Unhandled declared item kind '{item.__class__.__name__}'.")

        return buffer

    def formatType(self, item: BaseType) -> str:
        result = f"type {item.Identifier} is "
        if isinstance(item, IncompleteType):
            result += ""
        elif isinstance(item, IntegerType):
            result += f"range {item.Range!s}"
        elif isinstance(item, EnumeratedType):
            result += "(........)"
        elif isinstance(item, PhysicalType):
            result += " is range ....... units ..... end units"
        elif isinstance(item, ArrayType):
            result += "array(........) of ....."
        elif isinstance(item, RecordType):
            result += "record ..... end record"
        elif isinstance(item, AccessType):
            result += "access ....."
        elif isinstance(item, FileType):
            result += "file ....."
        elif isinstance(item, ProtectedType):
            result += "protected ..... end protected"
        elif isinstance(item, ProtectedTypeBody):
            result += "protected body ..... end protected body"
        else:
            raise PrettyPrintException(f"Unknown type '{item.__class__.__name__}'")

        return result

    def formatSubtypeIndication(self, subtypeIndication, entity: str, name: str) -> str:
        if isinstance(subtypeIndication, SimpleSubtypeSymbol):
            return f"{subtypeIndication.SymbolName}"
        elif isinstance(subtypeIndication, ConstrainedCompositeSubtypeSymbol):
            constraints = []
            for constraint in subtypeIndication.Constraints:
                constraints.append(str(constraint))

            return f"{subtypeIndication.SymbolName}({', '.join(constraints)})"
        else:
            raise PrettyPrintException(
                f"Unhandled subtype kind '{subtypeIndication.__class__.__name__}' for {entity} '{name}'."
            )

    def formatInitialValue(self, item: WithDefaultExpressionMixin) -> str:
        return f" := {item.DefaultExpression}" if item.DefaultExpression is not None else ""

    def formatHierarchy(self, statement: ConcurrentStatement, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        if isinstance(statement, ProcessStatement):
            buffer.append(f"{prefix}- {statement.Label}: process(...)")
        elif isinstance(statement, EntityInstantiation):
            buffer.append(f"{prefix}- {statement.Label}: entity {statement.Entity}")
        elif isinstance(statement, ComponentInstantiation):
            buffer.append(f"{prefix}- {statement.Label}: component {statement.Component}")
        elif isinstance(statement, ConfigurationInstantiation):
            buffer.append(f"{prefix}- {statement.Label}: configuration {statement.Configuration}")
        elif isinstance(statement, ConcurrentBlockStatement):
            buffer.append(f"{prefix}- {statement.Label}: block")
            for stmt in statement.Statements:
                for line in self.formatHierarchy(stmt, level + 2):
                    buffer.append(line)
        elif isinstance(statement, IfGenerateStatement):
            buffer.append(f"{prefix}- {statement.Label}: if {statement.IfBranch.Condition} generate")
            for stmt in statement.IfBranch.Statements:
                for line in self.formatHierarchy(stmt, level + 2):
                    buffer.append(line)
            for elsifBranch in statement.ElsifBranches:
                buffer.append(f"{prefix}  {statement.Label}: elsif {elsifBranch.Condition} generate")
                for stmt in elsifBranch.Statements:
                    for line in self.formatHierarchy(stmt, level + 2):
                        buffer.append(line)
            if statement.ElseBranch is not None:
                buffer.append(f"{prefix}  {statement.Label}: else generate")
                for stmt in statement.ElseBranch.Statements:
                    for line in self.formatHierarchy(stmt, level + 2):
                        buffer.append(line)
        elif isinstance(statement, CaseGenerateStatement):
            buffer.append(f"{prefix}- {statement.Label}: case {statement.SelectExpression} generate")
            for case in statement.Cases:
                buffer.append(f"{prefix}    {case!s}")
                for stmt in case.Statements:
                    for line in self.formatHierarchy(stmt, level + 2):
                        buffer.append(line)
        elif isinstance(statement, ForGenerateStatement):
            buffer.append(f"{prefix}- {statement.Label}: for {statement.LoopIndex} in {statement.Range} generate")
            for stmt in statement.Statements:
                for line in self.formatHierarchy(stmt, level + 2):
                    buffer.append(line)
        elif isinstance(statement, ConcurrentProcedureCall):
            buffer.append(f"{prefix}- {statement.Label}: {statement.Procedure!s}(...)")

        return buffer
