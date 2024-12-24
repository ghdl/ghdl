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
This module contains all DOM classes for VHDL's design units (:class:`context <Entity>`,
:class:`architecture <Architecture>`, :class:`package <Package>`,
:class:`package body <PackageBody>`, :class:`context <Context>` and
:class:`configuration <Configuration>`.


"""
from typing import Iterable

from pyTooling.Decorators import export

from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Instantiation import PackageInstantiation as VHDLModel_PackageInstantiation
from pyVHDLModel.Interface import GenericInterfaceItemMixin, PortInterfaceItemMixin
from pyVHDLModel.Concurrent import ConcurrentStatement
from pyVHDLModel.DesignUnit import Context as VHDLModel_Context
from pyVHDLModel.DesignUnit import Package as VHDLModel_Package
from pyVHDLModel.DesignUnit import PackageBody as VHDLModel_PackageBody
from pyVHDLModel.DesignUnit import Entity as VHDLModel_Entity
from pyVHDLModel.DesignUnit import Architecture as VHDLModel_Architecture
from pyVHDLModel.DesignUnit import Component as VHDLModel_Component
from pyVHDLModel.DesignUnit import Configuration as VHDLModel_Configuration
from pyVHDLModel.DesignUnit import LibraryClause as VHDLModel_LibraryClause
from pyVHDLModel.DesignUnit import UseClause as VHDLModel_UseClause
from pyVHDLModel.DesignUnit import ContextReference as VHDLModel_ContextReference
from pyVHDLModel.DesignUnit import ContextUnion as VHDLModel_ContextUnion

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, Position, DOMException
from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode
from pyGHDL.dom._Translate import GetGenericsFromChainedNodes, GetPortsFromChainedNodes, GetName
from pyGHDL.dom._Translate import GetDeclaredItemsFromChainedNodes, GetConcurrentStatementsFromChainedNodes
from pyGHDL.dom.Name import SimpleName, AllName
from pyGHDL.dom.Symbol import (
    EntitySymbol,
    ContextReferenceSymbol,
    LibraryReferenceSymbol,
    PackageSymbol,
    PackageMemberReferenceSymbol,
    AllPackageMembersReferenceSymbol,
)


@export
class LibraryClause(VHDLModel_LibraryClause, DOMMixin):
    def __init__(self, libraryNode: Iir, symbols: Iterable[Symbol]) -> None:
        super().__init__(symbols)
        DOMMixin.__init__(self, libraryNode)


@export
class UseClause(VHDLModel_UseClause, DOMMixin):
    def __init__(self, useNode: Iir, symbols: Iterable[Symbol]) -> None:
        super().__init__(symbols)
        DOMMixin.__init__(self, useNode)

    @classmethod
    def parse(cls, useNode: Iir):
        nameNode = nodes.Get_Selected_Name(useNode)
        name = GetName(nameNode)
        symbolType = AllPackageMembersReferenceSymbol if isinstance(name, AllName) else PackageMemberReferenceSymbol
        uses = [symbolType(nameNode, name)]
        for use in utils.chain_iter(nodes.Get_Use_Clause_Chain(useNode)):
            nameNode = nodes.Get_Selected_Name(use)
            name = GetName(nameNode)
            symbolType = AllPackageMembersReferenceSymbol if isinstance(name, AllName) else PackageMemberReferenceSymbol
            uses.append(symbolType(nameNode, name))

        return cls(useNode, uses)


@export
class ContextReference(VHDLModel_ContextReference, DOMMixin):
    def __init__(self, contextNode: Iir, symbols: Iterable[Symbol]) -> None:
        super().__init__(symbols)
        DOMMixin.__init__(self, contextNode)

    @classmethod
    def parse(cls, contextNode: Iir):
        nameNode = nodes.Get_Selected_Name(contextNode)
        contexts = [ContextReferenceSymbol(nameNode, GetName(nameNode))]
        for context in utils.chain_iter(nodes.Get_Context_Reference_Chain(contextNode)):
            nameNode = nodes.Get_Selected_Name(context)
            contexts.append(ContextReferenceSymbol(nameNode, GetName(nameNode)))

        return cls(contextNode, contexts)


@export
class Entity(VHDLModel_Entity, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[VHDLModel_ContextUnion] = None,
        genericItems: Iterable[GenericInterfaceItemMixin] = None,
        portItems: Iterable[PortInterfaceItemMixin] = None,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, contextItems, genericItems, portItems, declaredItems, statements, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityNode: Iir, contextItems: Iterable[VHDLModel_ContextUnion]):
        name = GetNameOfNode(entityNode)
        documentation = GetDocumentationOfNode(entityNode)
        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(entityNode))
        ports = GetPortsFromChainedNodes(nodes.Get_Port_Chain(entityNode))
        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(entityNode), "entity", name)
        statements = GetConcurrentStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(entityNode), "entity", name
        )

        # FIXME: read use clauses

        return cls(entityNode, name, contextItems, generics, ports, declaredItems, statements, documentation)


@export
class Architecture(VHDLModel_Architecture, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        entity: EntitySymbol,
        contextItems: Iterable[VHDLModel_ContextUnion] = None,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, entity, contextItems, declaredItems, statements, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, architectureNode: Iir, contextItems: Iterable[VHDLModel_ContextUnion]):
        name = GetNameOfNode(architectureNode)
        documentation = GetDocumentationOfNode(architectureNode)
        entityNameNode = nodes.Get_Entity_Name(architectureNode)
        entitySymbol = EntitySymbol(entityNameNode, GetName(entityNameNode))
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(architectureNode), "architecture", name
        )
        statements = GetConcurrentStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(architectureNode), "architecture", name
        )

        # FIXME: read use clauses

        return cls(architectureNode, name, entitySymbol, contextItems, declaredItems, statements, documentation)


@export
class Component(VHDLModel_Component, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        genericItems: Iterable[GenericInterfaceItemMixin] = None,
        portItems: Iterable[PortInterfaceItemMixin] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, genericItems, portItems, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, componentNode: Iir):
        name = GetNameOfNode(componentNode)
        documentation = GetDocumentationOfNode(componentNode)
        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(componentNode))
        ports = GetPortsFromChainedNodes(nodes.Get_Port_Chain(componentNode))

        return cls(componentNode, name, generics, ports, documentation)


@export
class Package(VHDLModel_Package, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[VHDLModel_ContextUnion] = None,
        genericItems: Iterable[GenericInterfaceItemMixin] = None,
        declaredItems: Iterable = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, contextItems, genericItems, declaredItems, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageNode: Iir, contextItems: Iterable[VHDLModel_ContextUnion]):
        name = GetNameOfNode(packageNode)
        documentation = GetDocumentationOfNode(packageNode)

        packageHeader = nodes.Get_Package_Header(packageNode)
        if packageHeader is not nodes.Null_Iir:
            generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(packageHeader))
        else:
            generics = []

        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(packageNode), "package", name)

        # FIXME: read use clauses

        return cls(packageNode, name, contextItems, generics, declaredItems, documentation)


@export
class PackageBody(VHDLModel_PackageBody, DOMMixin):
    def __init__(
        self,
        node: Iir,
        packageSymbol: PackageSymbol,
        contextItems: Iterable[VHDLModel_ContextUnion] = None,
        declaredItems: Iterable = None,
        documentation: str = None,
    ) -> None:
        super().__init__(packageSymbol, contextItems, declaredItems, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageBodyNode: Iir, contextItems: Iterable[VHDLModel_ContextUnion]):
        packageIdentifier = GetNameOfNode(packageBodyNode)
        packageSymbol = PackageSymbol(packageBodyNode, SimpleName(packageBodyNode, packageIdentifier))
        documentation = GetDocumentationOfNode(packageBodyNode)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(packageBodyNode), "package", packageIdentifier
        )

        # FIXME: read use clauses

        return cls(packageBodyNode, packageSymbol, contextItems, declaredItems, documentation)


@export
class PackageInstantiation(VHDLModel_PackageInstantiation, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        uninstantiatedPackageName: Symbol,
        #        genericItems: List[GenericInterfaceItem] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, uninstantiatedPackageName, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageNode: Iir):
        name = GetNameOfNode(packageNode)
        documentation = GetDocumentationOfNode(packageNode)
        uninstantiatedPackageName = nodes.Get_Uninstantiated_Package_Name(packageNode)

        # FIXME: read use clauses (does it apply here too?)
        # FIXME: read generics
        # FIXME: read generic map
        # genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))

        return cls(packageNode, name, uninstantiatedPackageName, documentation)


@export
class Context(VHDLModel_Context, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        references: Iterable[VHDLModel_ContextUnion] = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifier, references, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, contextNode: Iir):
        from pyGHDL.dom._Utils import GetIirKindOfNode

        name = GetNameOfNode(contextNode)
        documentation = GetDocumentationOfNode(contextNode)

        items = []
        names = []
        for item in utils.chain_iter(nodes.Get_Context_Items(contextNode)):
            kind = GetIirKindOfNode(item)
            if kind is nodes.Iir_Kind.Library_Clause:
                libraryIdentifier = GetNameOfNode(item)
                names.append(LibraryReferenceSymbol(item, SimpleName(item, libraryIdentifier)))
                if nodes.Get_Has_Identifier_List(item):
                    continue

                items.append(LibraryClause(item, names))
                names = []
            elif kind is nodes.Iir_Kind.Use_Clause:
                items.append(UseClause.parse(item))
            elif kind is nodes.Iir_Kind.Context_Reference:
                items.append(ContextReference.parse(item))
            else:
                pos = Position.parse(item)
                raise DOMException(f"Unknown context item kind '{kind.name}' in context at line {pos.Line}.")

        return cls(contextNode, name, items, documentation)


@export
class Configuration(VHDLModel_Configuration, DOMMixin):
    def __init__(
        self, node: Iir, identifier: str, contextItems: Iterable[Context] = None, documentation: str = None
    ) -> None:
        super().__init__(identifier, contextItems, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, configurationNode: Iir, contextItems: Iterable[Context]):
        name = GetNameOfNode(configurationNode)
        documentation = GetDocumentationOfNode(configurationNode)

        # FIXME: read use clauses
        # FIXME: read specifications

        return cls(configurationNode, name, contextItems, documentation)
