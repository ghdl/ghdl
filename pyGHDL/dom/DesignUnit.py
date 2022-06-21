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
from typing import Iterable

from pyTooling.Decorators import export

from pyVHDLModel import ContextUnion, EntityOrSymbol
from pyVHDLModel.SyntaxModel import (
    LibraryClause as VHDLModel_LibraryClause,
    UseClause as VHDLModel_UseClause,
    ContextReference as VHDLModel_ContextReference,
    Entity as VHDLModel_Entity,
    Architecture as VHDLModel_Architecture,
    Package as VHDLModel_Package,
    PackageBody as VHDLModel_PackageBody,
    PackageInstantiation as VHDLModel_PackageInstantiation,
    Context as VHDLModel_Context,
    Configuration as VHDLModel_Configuration,
    Component as VHDLModel_Component,
    GenericInterfaceItem,
    PortInterfaceItem,
    Name,
    ConcurrentStatement,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, Position, DOMException
from pyGHDL.dom._Utils import GetNameOfNode
from pyGHDL.dom._Translate import (
    GetGenericsFromChainedNodes,
    GetPortsFromChainedNodes,
    GetDeclaredItemsFromChainedNodes,
    GetConcurrentStatementsFromChainedNodes,
)
from pyGHDL.dom.Names import SimpleName
from pyGHDL.dom.Symbol import EntitySymbol


__all__ = []


@export
class LibraryClause(VHDLModel_LibraryClause, DOMMixin):
    def __init__(self, libraryNode: Iir, names: Iterable[Name]):
        super().__init__(names)
        DOMMixin.__init__(self, libraryNode)


@export
class UseClause(VHDLModel_UseClause, DOMMixin):
    def __init__(self, useNode: Iir, names: Iterable[Name]):
        super().__init__(names)
        DOMMixin.__init__(self, useNode)

    @classmethod
    def parse(cls, useNode: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        uses = [GetNameFromNode(nodes.Get_Selected_Name(useNode))]
        for use in utils.chain_iter(nodes.Get_Use_Clause_Chain(useNode)):
            uses.append(GetNameFromNode(nodes.Get_Selected_Name(use)))

        return cls(useNode, uses)


@export
class ContextReference(VHDLModel_ContextReference, DOMMixin):
    def __init__(self, contextNode: Iir, names: Iterable[Name]):
        super().__init__(names)
        DOMMixin.__init__(self, contextNode)

    @classmethod
    def parse(cls, contextNode: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        contexts = [GetNameFromNode(nodes.Get_Selected_Name(contextNode))]
        for context in utils.chain_iter(nodes.Get_Context_Reference_Chain(contextNode)):
            contexts.append(GetNameFromNode(nodes.Get_Selected_Name(context)))

        return cls(contextNode, contexts)


@export
class Entity(VHDLModel_Entity, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[ContextUnion] = None,
        genericItems: Iterable[GenericInterfaceItem] = None,
        portItems: Iterable[PortInterfaceItem] = None,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(identifier, contextItems, genericItems, portItems, declaredItems, statements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityNode: Iir, contextItems: Iterable[ContextUnion]):
        name = GetNameOfNode(entityNode)
        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(entityNode))
        ports = GetPortsFromChainedNodes(nodes.Get_Port_Chain(entityNode))
        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(entityNode), "entity", name)
        statements = GetConcurrentStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(entityNode), "entity", name
        )

        # FIXME: read use clauses

        return cls(entityNode, name, contextItems, generics, ports, declaredItems, statements)


@export
class Architecture(VHDLModel_Architecture, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        entity: EntityOrSymbol,
        contextItems: Iterable[ContextUnion] = None,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(identifier, entity, contextItems, declaredItems, statements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, architectureNode: Iir, contextItems: Iterable[ContextUnion]):
        name = GetNameOfNode(architectureNode)
        entityNameNode = nodes.Get_Entity_Name(architectureNode)
        entityName = GetNameOfNode(entityNameNode)
        entity = EntitySymbol(entityNameNode, SimpleName(entityNameNode, entityName))
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(architectureNode), "architecture", name
        )
        statements = GetConcurrentStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(architectureNode), "architecture", name
        )

        # FIXME: read use clauses

        return cls(architectureNode, name, entity, contextItems, declaredItems, statements)


@export
class Component(VHDLModel_Component, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        genericItems: Iterable[GenericInterfaceItem] = None,
        portItems: Iterable[PortInterfaceItem] = None,
    ):
        super().__init__(identifier, genericItems, portItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, componentNode: Iir):
        name = GetNameOfNode(componentNode)
        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(componentNode))
        ports = GetPortsFromChainedNodes(nodes.Get_Port_Chain(componentNode))

        return cls(componentNode, name, generics, ports)


@export
class Package(VHDLModel_Package, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[ContextUnion] = None,
        genericItems: Iterable[GenericInterfaceItem] = None,
        declaredItems: Iterable = None,
    ):
        super().__init__(identifier, contextItems, genericItems, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageNode: Iir, contextItems: Iterable[ContextUnion]):
        name = GetNameOfNode(packageNode)

        packageHeader = nodes.Get_Package_Header(packageNode)
        if packageHeader is not nodes.Null_Iir:
            generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(packageHeader))
        else:
            generics = []

        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(packageNode), "package", name)

        # FIXME: read use clauses

        return cls(packageNode, name, contextItems, generics, declaredItems)


@export
class PackageBody(VHDLModel_PackageBody, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[ContextUnion] = None,
        declaredItems: Iterable = None,
    ):
        super().__init__(identifier, contextItems, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageBodyNode: Iir, contextItems: Iterable[ContextUnion]):
        name = GetNameOfNode(packageBodyNode)
        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(packageBodyNode), "package", name)

        # FIXME: read use clauses

        return cls(packageBodyNode, name, contextItems, declaredItems)


@export
class PackageInstantiation(VHDLModel_PackageInstantiation, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        uninstantiatedPackageName: Name,
        #        genericItems: List[GenericInterfaceItem] = None,
    ):
        super().__init__(identifier, uninstantiatedPackageName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageNode: Iir):
        name = GetNameOfNode(packageNode)
        uninstantiatedPackageName = nodes.Get_Uninstantiated_Package_Name(packageNode)

        # FIXME: read use clauses (does it apply here too?)
        # FIXME: read generics
        # FIXME: read generic map
        # genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))

        return cls(packageNode, name, uninstantiatedPackageName)


@export
class Context(VHDLModel_Context, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        libraryReferences: Iterable[LibraryClause] = None,
        packageReferences: Iterable[UseClause] = None,
    ):
        super().__init__(identifier, libraryReferences, packageReferences)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, contextNode: Iir):
        from pyGHDL.dom._Utils import GetIirKindOfNode

        name = GetNameOfNode(contextNode)

        items = []
        names = []
        for item in utils.chain_iter(nodes.Get_Context_Items(contextNode)):
            kind = GetIirKindOfNode(item)
            if kind is nodes.Iir_Kind.Library_Clause:
                names.append(SimpleName(item, GetNameOfNode(item)))
                if nodes.Get_Has_Identifier_List(item):
                    continue

                items.append(LibraryClause(item, names))
                names = []
            elif kind is nodes.Iir_Kind.Use_Clause:
                items.append(UseClause.parse(item))
            else:
                pos = Position.parse(item)
                raise DOMException(f"Unknown context item kind '{kind.name}' in context at line {pos.Line}.")

        return cls(contextNode, name, items)


@export
class Configuration(VHDLModel_Configuration, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        contextItems: Iterable[Context] = None,
    ):
        super().__init__(identifier, contextItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, configurationNode: Iir, contextItems: Iterable[Context]):
        name = GetNameOfNode(configurationNode)

        # FIXME: read use clauses
        # FIXME: read specifications

        return cls(configurationNode, name, contextItems)
