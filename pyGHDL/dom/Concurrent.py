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
# Package module:   DOM: Concurrent statements.
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
from typing import Iterable

from pydecor import export

from pyVHDLModel.SyntaxModel import (
    ConcurrentBlockStatement as VHDLModel_ConcurrentBlockStatement,
    ComponentInstantiation as VHDLModel_ComponentInstantiation,
    EntityInstantiation as VHDLModel_EntityInstantiation,
    ConfigurationInstantiation as VHDLModel_ConfigurationInstantiation,
    ConcurrentStatement, Name,
)

from pyGHDL.libghdl import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode


@export
class ComponentInstantiation(VHDLModel_ComponentInstantiation, DOMMixin):
    def __init__(self, node: Iir, label: str, componentName: Name):
        super().__init__(label, componentName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ComponentInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        componentName = GetNameFromNode(instantiatedUnit)

        return cls(instantiationNode, label, componentName)


@export
class EntityInstantiation(VHDLModel_EntityInstantiation, DOMMixin):
    def __init__(self, node: Iir, label: str, entityName: Name, architectureName: Name = None):
        super().__init__(label, entityName, architectureName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "EntityInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        entityId = nodes.Get_Entity_Name(instantiatedUnit)
        entityName = GetNameFromNode(entityId)

        architectureName = None
        architectureId = nodes.Get_Architecture(instantiatedUnit)
        if architectureId != nodes.Null_Iir:
            architectureName = GetNameOfNode(architectureId)

        return cls(instantiationNode, label, entityName, architectureName)


@export
class ConfigurationInstantiation(VHDLModel_ConfigurationInstantiation, DOMMixin):
    def __init__(self, node: Iir, label: str, configurationName: Name):
        super().__init__(label, configurationName)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ConfigurationInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        configurationId = nodes.Get_Configuration_Name(instantiatedUnit)
        configurationName = GetNameFromNode(configurationId)

        return cls(instantiationNode, label, configurationName)


@export
class ConcurrentBlockStatement(VHDLModel_ConcurrentBlockStatement, DOMMixin):
    def __init__(
        self,
        node: Iir,
        label: str,
        #        portItems: Iterable[PortInterfaceItem] = None,
        declaredItems: Iterable = None,
        bodyItems: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(label, None, declaredItems, bodyItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, blockNode: Iir, label: str) -> "ConcurrentBlockStatement":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
        )

        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(blockNode), "block", label
        )
        bodyItems = GetStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(blockNode), "block", label
        )

        return cls(blockNode, label, declaredItems, bodyItems)
