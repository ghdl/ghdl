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

from pydecor import export

from pyVHDLModel.SyntaxModel import (
    UseClause as VHDLModel_UseClause,
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
    EntityOrSymbol,
    Name,
    ConcurrentStatement,
)

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode
from pyGHDL.dom._Translate import (
    GetGenericsFromChainedNodes,
    GetPortsFromChainedNodes,
    GetDeclaredItemsFromChainedNodes,
    GetStatementsFromChainedNodes,
)
from pyGHDL.dom.Names import SimpleName
from pyGHDL.dom.Symbol import EntitySymbol


__all__ = []


@export
class UseClause(VHDLModel_UseClause, DOMMixin):
    def __init__(self, node: Iir, name: Name):
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, useNode: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        selectedName = nodes.Get_Selected_Name(useNode)
        name = GetNameFromNode(selectedName)

        return cls(useNode, name)


@export
class Entity(VHDLModel_Entity, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        genericItems: Iterable[GenericInterfaceItem] = None,
        portItems: Iterable[PortInterfaceItem] = None,
        declaredItems: Iterable = None,
        bodyItems: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(identifier, genericItems, portItems, declaredItems, bodyItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityNode: Iir):
        name = GetNameOfNode(entityNode)
        generics = GetGenericsFromChainedNodes(nodes.Get_Generic_Chain(entityNode))
        ports = GetPortsFromChainedNodes(nodes.Get_Port_Chain(entityNode))
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(entityNode), "entity", name
        )
        bodyItems = GetStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(entityNode), "entity", name
        )

        return cls(entityNode, name, generics, ports, declaredItems, bodyItems)


@export
class Architecture(VHDLModel_Architecture, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        entity: EntityOrSymbol,
        declaredItems: Iterable = None,
        bodyItems: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(identifier, entity, declaredItems, bodyItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, architectureNode: Iir):
        name = GetNameOfNode(architectureNode)
        entityNameNode = nodes.Get_Entity_Name(architectureNode)
        entityName = GetNameOfNode(entityNameNode)
        entity = EntitySymbol(entityNameNode, SimpleName(entityNameNode, entityName))
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(architectureNode), "architecture", name
        )
        bodyItems = GetStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(architectureNode), "architecture", name
        )

        return cls(architectureNode, name, entity, declaredItems, bodyItems)

    def resolve(self):
        pass


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
        genericItems: Iterable[GenericInterfaceItem] = None,
        declaredItems: Iterable = None,
    ):
        super().__init__(identifier, genericItems, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageNode: Iir):
        name = GetNameOfNode(packageNode)

        packageHeader = nodes.Get_Package_Header(packageNode)
        if packageHeader is not nodes.Null_Iir:
            generics = GetGenericsFromChainedNodes(
                nodes.Get_Generic_Chain(packageHeader)
            )
        else:
            generics = []

        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(packageNode), "package", name
        )

        return cls(packageNode, name, generics, declaredItems)


@export
class PackageBody(VHDLModel_PackageBody, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
        declaredItems: Iterable = None,
    ):
        super().__init__(identifier, declaredItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, packageBodyNode: Iir):
        name = GetNameOfNode(packageBodyNode)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(packageBodyNode), "package", name
        )

        return cls(packageBodyNode, name, declaredItems)


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

        # FIXME: read generics
        # FIXME: read generic map

        return cls(packageNode, name, uninstantiatedPackageName)


@export
class Context(VHDLModel_Context, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, contextNode: Iir):
        name = GetNameOfNode(contextNode)

        # FIXME: read use clauses

        return cls(contextNode, name)


@export
class Configuration(VHDLModel_Configuration, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, configurationNode: Iir):
        name = GetNameOfNode(configurationNode)

        # FIXME: needs an implementation

        return cls(configurationNode, name)
