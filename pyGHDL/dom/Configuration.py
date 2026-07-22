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
# Package module:   DOM: Configurations, block/component configurations, binding indications.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2026 Tristan Gingold
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
from typing import List, Generator, Union

from pyTooling.Decorators import export

from pyVHDLModel.Symbol import Symbol, PossibleReference
from pyVHDLModel.Name import Name
from pyVHDLModel.Association import GenericAssociationItem, PortAssociationItem
from pyVHDLModel.Configuration import EntityAspect as VHDLModel_EntityAspect
from pyVHDLModel.Configuration import EntityAspectEntity as VHDLModel_EntityAspectEntity
from pyVHDLModel.Configuration import EntityAspectConfiguration as VHDLModel_EntityAspectConfiguration
from pyVHDLModel.Configuration import EntityAspectOpen as VHDLModel_EntityAspectOpen
from pyVHDLModel.Configuration import BindingIndication as VHDLModel_BindingIndication
from pyVHDLModel.Configuration import AllInstantiationList as VHDLModel_AllInstantiationList
from pyVHDLModel.Configuration import OthersInstantiationList as VHDLModel_OthersInstantiationList
from pyVHDLModel.Configuration import ComponentConfiguration as VHDLModel_ComponentConfiguration
from pyVHDLModel.Configuration import BlockConfiguration as VHDLModel_BlockConfiguration

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.libghdl import utils
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom._Utils import GetIirKindOfNode
from pyGHDL.dom.Symbol import EntitySymbol, ArchitectureSymbol, ConfigurationSymbol, ComponentInstantiationSymbol


@export
class EntityAspectEntity(VHDLModel_EntityAspectEntity, DOMMixin):
    def __init__(self, node: Iir, entity: EntitySymbol, architecture: ArchitectureSymbol = None) -> None:
        super().__init__(entity, architecture)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityAspectNode: Iir) -> "EntityAspectEntity":
        from pyGHDL.dom._Translate import GetName

        entityNameNode = nodes.Get_Entity_Name(entityAspectNode)
        entity = EntitySymbol(entityNameNode, GetName(entityNameNode))

        architectureNode = nodes.Get_Architecture(entityAspectNode)
        architecture = (
            None if architectureNode == nodes.Null_Iir else ArchitectureSymbol(architectureNode, GetName(architectureNode))
        )

        return cls(entityAspectNode, entity, architecture)


@export
class EntityAspectConfiguration(VHDLModel_EntityAspectConfiguration, DOMMixin):
    def __init__(self, node: Iir, configuration: ConfigurationSymbol) -> None:
        super().__init__(configuration)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityAspectNode: Iir) -> "EntityAspectConfiguration":
        from pyGHDL.dom._Translate import GetName

        configurationNameNode = nodes.Get_Configuration_Name(entityAspectNode)
        configuration = ConfigurationSymbol(configurationNameNode, GetName(configurationNameNode))

        return cls(entityAspectNode, configuration)


@export
class EntityAspectOpen(VHDLModel_EntityAspectOpen, DOMMixin):
    def __init__(self, node: Iir) -> None:
        super().__init__()
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, entityAspectNode: Iir) -> "EntityAspectOpen":
        return cls(entityAspectNode)


def GetEntityAspectFromNode(entityAspectNode: Iir) -> VHDLModel_EntityAspect:
    """Translates an entity aspect (IIR node) to the matching pyVHDLModel.Configuration.EntityAspect subclass."""
    kind = GetIirKindOfNode(entityAspectNode)
    if kind == nodes.Iir_Kind.Entity_Aspect_Entity:
        return EntityAspectEntity.parse(entityAspectNode)
    elif kind == nodes.Iir_Kind.Entity_Aspect_Configuration:
        return EntityAspectConfiguration.parse(entityAspectNode)
    elif kind == nodes.Iir_Kind.Entity_Aspect_Open:
        return EntityAspectOpen.parse(entityAspectNode)
    else:
        position = Position.parse(entityAspectNode)
        raise DOMException(f"Unknown entity aspect kind '{kind.name}' at {position}.")


@export
class BindingIndication(VHDLModel_BindingIndication, DOMMixin):
    def __init__(
        self,
        node: Iir,
        entityAspect: VHDLModel_EntityAspect = None,
        genericAssociationItems: List[GenericAssociationItem] = None,
        portAssociationItems: List[PortAssociationItem] = None,
    ) -> None:
        super().__init__(entityAspect, genericAssociationItems, portAssociationItems)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, bindingIndicationNode: Iir) -> "BindingIndication":
        from pyGHDL.dom._Translate import GetGenericMapAspect, GetPortMapAspect

        entityAspectNode = nodes.Get_Entity_Aspect(bindingIndicationNode)
        entityAspect = None if entityAspectNode == nodes.Null_Iir else GetEntityAspectFromNode(entityAspectNode)

        genericAssociationItems = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(bindingIndicationNode))
        portAssociationItems = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(bindingIndicationNode))

        return cls(bindingIndicationNode, entityAspect, genericAssociationItems, portAssociationItems)


@export
class AllInstantiationList(VHDLModel_AllInstantiationList, DOMMixin):
    def __init__(self, node: Iir) -> None:
        super().__init__()
        DOMMixin.__init__(self, node)


@export
class OthersInstantiationList(VHDLModel_OthersInstantiationList, DOMMixin):
    def __init__(self, node: Iir) -> None:
        super().__init__()
        DOMMixin.__init__(self, node)


@export
class ComponentConfiguration(VHDLModel_ComponentConfiguration, DOMMixin):
    """
    .. note::

       Also used for configuration specifications (``for U1 : comp use entity ...;`` declared
       directly in an architecture's declarative part) - GHDL represents both with the identical
       field structure (``Instantiation_List``, ``Component_Name``, ``Binding_Indication``).
    """

    def __init__(
        self,
        node: Iir,
        instantiationList: Union[List[Name], "AllInstantiationList", "OthersInstantiationList"],
        componentName: ComponentInstantiationSymbol,
        bindingIndication: BindingIndication = None,
    ) -> None:
        super().__init__(instantiationList, componentName, bindingIndication)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "ComponentConfiguration":
        from pyGHDL.dom._Translate import GetName

        instList = nodes.Get_Instantiation_List(node)
        if instList == nodes.Iir_Flist_All:
            instantiationList = AllInstantiationList(node)
        elif instList == nodes.Iir_Flist_Others:
            instantiationList = OthersInstantiationList(node)
        else:
            instantiationList = []
            for labelNode in utils.flist_iter(instList):
                instantiationList.append(GetName(labelNode))

        componentNameNode = nodes.Get_Component_Name(node)
        componentName = ComponentInstantiationSymbol(componentNameNode, GetName(componentNameNode))

        bindingIndicationNode = nodes.Get_Binding_Indication(node)
        bindingIndication = (
            None if bindingIndicationNode == nodes.Null_Iir else BindingIndication.parse(bindingIndicationNode)
        )

        return cls(node, instantiationList, componentName, bindingIndication)


@export
class BlockConfiguration(VHDLModel_BlockConfiguration, DOMMixin):
    def __init__(
        self,
        node: Iir,
        blockSpecification: Symbol,
        items: List[Union["BlockConfiguration", ComponentConfiguration]] = None,
    ) -> None:
        super().__init__(blockSpecification, items)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "BlockConfiguration":
        from pyGHDL.dom._Translate import GetName
        from pyGHDL.dom.Symbol import Symbol as DOMSymbol

        blockSpecificationNode = nodes.Get_Block_Specification(node)
        blockSpecification = DOMSymbol(
            blockSpecificationNode,
            GetName(blockSpecificationNode),
            PossibleReference.Architecture | PossibleReference.Label,
        )

        items = list(GetConfigurationItemsFromChainedNodes(nodes.Get_Configuration_Item_Chain(node)))

        return cls(node, blockSpecification, items)


def GetConfigurationItemsFromChainedNodes(
    nodeChain: Iir,
) -> Generator[Union[BlockConfiguration, ComponentConfiguration], None, None]:
    """Translates a chain of configuration items (component/block configurations) to pyVHDLModel objects."""
    for item in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Component_Configuration:
            yield ComponentConfiguration.parse(item)
        elif kind == nodes.Iir_Kind.Block_Configuration:
            yield BlockConfiguration.parse(item)
        else:
            position = Position.parse(item)
            raise DOMException(f"Unknown configuration item kind '{kind.name}' at {position}.")
