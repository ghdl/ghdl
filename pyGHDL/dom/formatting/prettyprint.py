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
"""
This module offers a formatter to render a design, document or design unit as an indented plain-text tree.
"""

from typing import List, Union

from pyTooling.Common import getFullyQualifiedName
from pyTooling.Decorators import export

from pyVHDLModel.Base import NamedEntityMixin
from pyVHDLModel.Interface import GenericInterfaceItemMixin, PortInterfaceItemMixin
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
from pyGHDL.dom.Symbol import SimpleSubtypeSymbol, ConstrainedArraySubtypeSymbol
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
    PortSimpleSignalInterfaceItem,
    PortViewSignalInterfaceItem,
    GenericTypeInterfaceItem,
    ModeViewDeclaration,
    SimpleModeViewElement,
    CompositeModeViewElement,
)
from pyVHDLModel.Interface import ModeViewElement
from pyGHDL.dom.Object import Constant, Signal, SharedVariable, File
from pyGHDL.dom.Attribute import Attribute, AttributeSpecification
from pyGHDL.dom.Subprogram import Procedure
from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.PSL import DefaultClock

StringBuffer = List[str]
"""
A list of already formatted lines, which the ``Format*`` methods append to and return.
"""


@export
class PrettyPrintException(GHDLBaseException):
    """
    The exception is raised when a model item cannot be rendered back to VHDL source code.

    It reports an item the pretty-printer has no formatting rule for.
    """


@export
class PrettyPrint:
    """
    Renders a :mod:`pyGHDL.dom` design, document or design unit as an indented plain-text tree.

    Each ``Format*`` method returns a list of lines and is given an indentation ``level``, so the
    methods compose from a whole design down to the smallest model item.
    """

    # _buffer: StringBuffer
    #
    # def __init__(self) -> None:
    #     self._buffer = []

    def CleanupDocumentationBlocks(self, documentationContent: str, level: int = 0):
        """
        Renders a documentation comment as its first line, indented to the given level.

        :param documentationContent: The documentation comment, or ``None`` if the item has none.
        :param level:                The indentation level, two spaces each.
        :returns:                    The indented first line of the comment, or just the indentation.
        """
        prefix = "  " * level
        if documentationContent is None:
            return prefix

        documentationLines = documentationContent.split("\n")
        return f"{prefix}{documentationLines[0][2:].lstrip()}"

    def formatDesign(self, design: Design, level: int = 0) -> StringBuffer:
        """
        Renders a design's libraries and documents.

        :param design: The design to render.
        :param level:  The indentation level, two spaces each.
        :returns:      The rendered lines.
        """
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
        """
        Renders a library's design units.

        :param library: The library to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
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
            buffer.append(
                f"{prefix}  - {entity.Identifier}({', '.join([a.Identifier for a in entity.Architectures.values()])})"
            )
        buffer.append(f"{prefix}Configurations ({len(library.Configurations)}):")
        for configuration in library.Configurations.values():
            buffer.append(f"{prefix}  - {configuration.Identifier}")

        return buffer

    def formatDocument(self, document: Document, level: int = 0) -> StringBuffer:
        """
        Renders a document's design units.

        :param document: The document to render.
        :param level:    The indentation level, two spaces each.
        :returns:        The rendered lines.
        """
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
        """
        Renders an entity's generics, ports, declarations and statements.

        :param entity: The entity to render.
        :param level:  The indentation level, two spaces each.
        :returns:      The rendered lines.
        """
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
        for item in entity.Architectures.values():
            buffer.append(f"{prefix}  - {item.Identifier}")

        return buffer

    def formatArchitecture(self, architecture: Architecture, level: int = 0) -> StringBuffer:
        """
        Renders an architecture's declarations and statements.

        :param architecture: The architecture to render.
        :param level:        The indentation level, two spaces each.
        :returns:            The rendered lines.
        """
        buffer = []
        prefix = "  " * level
        documentationFirstLine = self.CleanupDocumentationBlocks(architecture.Documentation)
        buffer.append(
            f"{prefix}- Name: {architecture.Identifier}\n"
            f"{prefix}  File: {architecture.Position.Filename.name}\n"
            f"{prefix}  Position: {architecture.Position.Line}:{architecture.Position.Column}\n"
            f"{prefix}  Documentation: {documentationFirstLine}"
        )
        buffer.append(f"{prefix}  Entity: {architecture.Entity.Name.Identifier}")
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
        """
        Renders a component's generics and ports.

        :param component: The component to render.
        :param level:     The indentation level, two spaces each.
        :returns:         The rendered lines.
        """
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
        """
        Renders a package's generics and declarations.

        :param package: The package to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
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
        """
        Renders a package instantiation's generic map.

        :param package: The package instantiation to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
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
        """
        Renders a package body's declarations.

        :param packageBody: The package body to render.
        :param level:       The indentation level, two spaces each.
        :returns:           The rendered lines.
        """
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
        """
        Renders a configuration.

        :param configuration: The configuration to render.
        :param level:         The indentation level, two spaces each.
        :returns:             The rendered lines.
        """
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}- Name: {configuration.Identifier}")

        return buffer

    def formatContext(self, context: Context, level: int = 0) -> StringBuffer:
        """
        Renders a context's clauses.

        :param context: The context to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
        buffer = []
        prefix = "  " * level
        buffer.append(f"{prefix}- Name: {context.Identifier}")

        return buffer

    def formatGeneric(
        self, generic: Union[NamedEntityMixin, GenericInterfaceItemMixin], level: int = 0
    ) -> StringBuffer:
        """
        Renders a generic, dispatching on its kind.

        :param generic: The generic interface item to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
        if isinstance(generic, GenericConstantInterfaceItem):
            return self.formatGenericConstant(generic, level)
        elif isinstance(generic, GenericTypeInterfaceItem):
            return self.formatGenericType(generic, level)
        else:
            raise PrettyPrintException(
                f"Unhandled generic kind '{getFullyQualifiedName(generic)}' for generic '{generic.Identifiers[0]}'."
            )

    def formatPort(self, port: Union[NamedEntityMixin, PortInterfaceItemMixin], level: int = 0) -> StringBuffer:
        """
        Renders a port, dispatching on its kind.

        :param port:  The port interface item to render.
        :param level: The indentation level, two spaces each.
        :returns:     The rendered lines.
        """
        if isinstance(port, PortSimpleSignalInterfaceItem):
            return self.formatPortSignal(port, level)
        elif isinstance(port, PortViewSignalInterfaceItem):
            return self.formatPortView(port, level)
        else:
            raise PrettyPrintException(
                f"Unhandled port kind '{getFullyQualifiedName(port)}' for port '{port.Identifiers[0]}'."
            )

    def formatGenericConstant(self, generic: GenericConstantInterfaceItem, level: int = 0) -> StringBuffer:
        """
        Renders a generic constant.

        :param generic: The generic constant to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
        buffer = []
        prefix = "  " * level

        subTypeIndication = self.formatSubtypeIndication(generic.Subtype, "generic", generic.Identifiers[0])
        buffer.append(
            f"{prefix}  - {', '.join(generic.Identifiers)} : {generic.Mode!s} {subTypeIndication}{self.formatInitialValue(generic)}"
        )

        return buffer

    def formatGenericType(self, generic: GenericConstantInterfaceItem, level: int = 0) -> StringBuffer:
        """
        Renders a generic type.

        :param generic: The generic type to render.
        :param level:   The indentation level, two spaces each.
        :returns:       The rendered lines.
        """
        buffer = []
        prefix = "  " * level

        buffer.append(f"{prefix}  - type {generic.Identifier}")

        return buffer

    def formatPortSignal(self, port: PortSimpleSignalInterfaceItem, level: int = 0) -> StringBuffer:
        """
        Renders a port signal.

        :param port:  The port signal to render.
        :param level: The indentation level, two spaces each.
        :returns:     The rendered lines.
        """
        buffer = []
        prefix = "  " * level

        subTypeIndication = self.formatSubtypeIndication(port.Subtype, "port", port.Identifiers[0])
        buffer.append(
            f"{prefix}  - {', '.join(port.Identifiers)} : {port.Mode} {subTypeIndication}{self.formatInitialValue(port)}"
        )

        return buffer

    def formatPortView(self, port: PortViewSignalInterfaceItem, level: int = 0) -> StringBuffer:
        """
        Renders a port declared with a mode view.

        :param port:  The port mode view to render.
        :param level: The indentation level, two spaces each.
        :returns:     The rendered lines.
        """
        buffer = []
        prefix = "  " * level

        # A mode view port has no mode of its own - the mode view reference takes that position.
        buffer.append(f"{prefix}  - {', '.join(port.Identifiers)} : view {port.ModeViewIndication!s}")

        return buffer

    def formatDeclaredItems(self, item, level: int = 0) -> StringBuffer:
        """
        Renders the declared items of a declarative region.

        :param item:  The declarative region to render.
        :param level: The indentation level, two spaces each.
        :returns:     The rendered lines.
        """
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
        elif isinstance(item, ModeViewDeclaration):
            buffer.append(f"{prefix}- view {item.Identifier} of {item.Subtype!s} is")
            for element in item.Elements:
                buffer.append(f"{prefix}  - {self.formatModeViewElement(element)}")
        else:
            raise PrettyPrintException(f"Unhandled declared item kind '{getFullyQualifiedName(item)}'.")

        return buffer

    def formatModeViewElement(self, element: ModeViewElement) -> str:
        """
        Renders a mode view element.

        :param element: The mode view element to render.
        :returns:       The rendered line.
        """
        identifiers = ", ".join(element.Identifiers)

        if isinstance(element, SimpleModeViewElement):
            return f"{identifiers} : {element.Mode!s}"
        elif isinstance(element, CompositeModeViewElement):
            return f"{identifiers} : view {element.ModeViewName!s}"

        raise PrettyPrintException(f"Unhandled mode view element kind '{getFullyQualifiedName(element)}'.")

    def formatType(self, item: BaseType) -> str:
        """
        Renders an object's subtype indication.

        :param item: The object to render.
        :returns:    The rendered line.
        """
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
            raise PrettyPrintException(f"Unknown type '{getFullyQualifiedName(item)}'")

        return result

    def formatSubtypeIndication(self, subtypeIndication, entity: str, name: str) -> str:
        """
        Renders a subtype indication.

        :param subtypeIndication:     The subtype indication to render.
        :param entity:                The kind of item the subtype indication belongs to. Used in exception messages.
        :param name:                  The name of that item. Used in exception messages.
        :returns:                     The rendered subtype indication.
        :raises PrettyPrintException: If the subtype indication's kind is not handled.
        """
        if isinstance(subtypeIndication, SimpleSubtypeSymbol):
            return f"{subtypeIndication.Name.Identifier}"
        elif isinstance(subtypeIndication, ConstrainedArraySubtypeSymbol):
            constraints = []
            # FIXME: disabled due to problems with symbols
            # for constraint in subtypeIndication.Constraints:
            #     constraints.append(str(constraint))

            return f"{subtypeIndication.Name.Identifier}({', '.join(constraints)})"
        else:
            raise PrettyPrintException(
                f"Unhandled subtype kind '{getFullyQualifiedName(subtypeIndication)}' for {entity} '{name}'."
            )

    def formatInitialValue(self, item: WithDefaultExpressionMixin) -> str:
        """
        Renders an object's default value, if it has one.

        :param item: The object to render.
        :returns:    The rendered line.
        """
        return f" := {item.DefaultExpression}" if item.DefaultExpression is not None else ""

    def formatHierarchy(self, statement: ConcurrentStatement, level: int = 0) -> StringBuffer:
        """
        Renders a statement and the statements nested in it.

        :param statement: The statement to render.
        :param level:     The indentation level, two spaces each.
        :returns:         The rendered lines.
        """
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
