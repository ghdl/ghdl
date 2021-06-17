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
from pydecor import export

from pyVHDLModel.VHDLModel import Entity as VHDLModel_Entity
from pyVHDLModel.VHDLModel import Architecture as VHDLModel_Architecture
from pyVHDLModel.VHDLModel import Package as VHDLModel_Package
from pyVHDLModel.VHDLModel import PackageBody as VHDLModel_PackageBody
from pyVHDLModel.VHDLModel import Context as VHDLModel_Context
from pyVHDLModel.VHDLModel import Configuration as VHDLModel_Configuration

from pyGHDL.libghdl.vhdl import nodes

from pyGHDL.dom._Utils import NodeToName
from pyGHDL.dom._Translate import (
    GetGenericsFromChainedNodes,
    GetPortsFromChainedNodes,
    GetDeclaredItemsFromChainedNodes,
)
from pyGHDL.dom.Common import GHDLMixin


__all__ = []


@export
class Entity(VHDLModel_Entity, GHDLMixin):
    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)
        entity = cls(name)

        for generic in GetGenericsFromChainedNodes(
            nodes.Get_Generic_Chain(libraryUnit)
        ):
            entity.GenericItems.append(generic)

        for port in GetPortsFromChainedNodes(nodes.Get_Port_Chain(libraryUnit)):
            entity.PortItems.append(port)

        for item in GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(libraryUnit), "entity", name
        ):
            entity.DeclaredItems.append(item)

        return entity


@export
class Architecture(VHDLModel_Architecture, GHDLMixin):
    def __init__(self, name: str, entityName: str):
        super().__init__(name)

        self.__entityName = entityName

    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)
        entityName = NodeToName(nodes.Get_Entity_Name(libraryUnit))

        architecture = cls(name, entityName)

        for item in GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(libraryUnit), "architecture", name
        ):
            architecture.DeclaredItems.append(item)

        return architecture

    def resolve(self):
        pass


@export
class Package(VHDLModel_Package, GHDLMixin):
    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)

        package = cls(name)

        for item in GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(libraryUnit), "package", name
        ):
            package.DeclaredItems.append(item)

        return package


@export
class PackageBody(VHDLModel_PackageBody, GHDLMixin):
    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)

        packageBody = cls(name)

        for item in GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(libraryUnit), "package body", name
        ):
            packageBody.DeclaredItems.append(item)

        return packageBody


@export
class Context(VHDLModel_Context, GHDLMixin):
    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)
        return cls(name)


@export
class Configuration(VHDLModel_Configuration, GHDLMixin):
    @classmethod
    def parse(cls, libraryUnit):
        name = NodeToName(libraryUnit)
        return cls(name)
