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
# Package module:   DOM: Interface items (e.g. generic or port)
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
from typing import List, Iterator

from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel.SyntaxModel import (
    SimpleSubtypeSymbol as VHDLModel_SimpleSubtypeSymbol,
    ConstrainedScalarSubtypeSymbol as VHDLModel_ConstrainedScalarSubtypeSymbol,
    ConstrainedCompositeSubtypeSymbol as VHDLModel_ConstrainedCompositeSubtypeSymbol,
    SimpleObjectOrFunctionCallSymbol as VHDLModel_SimpleObjectOrFunctionCallSymbol,
    IndexedObjectOrFunctionCallSymbol as VHDLModel_IndexedObjectOrFunctionCallSymbol,
    ConstraintUnion,
    Name,
    LibraryReferenceSymbol as VHDLModel_LibraryReferenceSymbol,
    PackageReferenceSymbol as VHDLModel_PackageReferenceSymbol,
    PackageMembersReferenceSymbol as VHDLModel_PackageMembersReferenceSymbol,
    AllPackageMembersReferenceSymbol as VHDLModel_AllPackageMembersReferenceSymbol,
    ContextReferenceSymbol as VHDLModel_ContextReferenceSymbol,
    EntityInstantiationSymbol as VHDLModel_EntityInstantiationSymbol,
    ComponentInstantiationSymbol as VHDLModel_ComponentInstantiationSymbol,
    ConfigurationInstantiationSymbol as VHDLModel_ConfigurationInstantiationSymbol,
    EntitySymbol as VHDLModel_EntitySymbol,
    ArchitectureSymbol as VHDLModel_ArchitectureSymbol,
    PackageSymbol as VHDLModel_PackageSymbol,
)
from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin
from pyGHDL.dom.Range import Range


@export
class LibraryReferenceSymbol(VHDLModel_LibraryReferenceSymbol, DOMMixin):
    @InheritDocString(VHDLModel_LibraryReferenceSymbol)
    def __init__(self, identifierNode: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageReferenceSymbol(VHDLModel_PackageReferenceSymbol, DOMMixin):
    @InheritDocString(VHDLModel_PackageReferenceSymbol)
    def __init__(self, identifierNode: Iir, identifier: str, prefix: LibraryReferenceSymbol):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageMembersReferenceSymbol(VHDLModel_PackageMembersReferenceSymbol, DOMMixin):
    @InheritDocString(VHDLModel_PackageMembersReferenceSymbol)
    def __init__(self, identifierNode: Iir, identifier: str, prefix: PackageReferenceSymbol):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class AllPackageMembersReferenceSymbol(VHDLModel_AllPackageMembersReferenceSymbol, DOMMixin):
    @InheritDocString(VHDLModel_AllPackageMembersReferenceSymbol)
    def __init__(self, identifierNode: Iir, prefix: PackageReferenceSymbol):
        super().__init__(prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class ContextReferenceSymbol(VHDLModel_ContextReferenceSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ContextReferenceSymbol)
    def __init__(self, identifierNode: Iir, identifier: str, prefix: LibraryReferenceSymbol):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class EntityInstantiationSymbol(VHDLModel_EntityInstantiationSymbol, DOMMixin):
    @InheritDocString(VHDLModel_EntityInstantiationSymbol)
    def __init__(self, identifierNode: Iir, identifier: str, prefix: LibraryReferenceSymbol):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class ComponentInstantiationSymbol(VHDLModel_ComponentInstantiationSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ComponentInstantiationSymbol)
    def __init__(self, identifierNode: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, identifierNode)


@export
class ConfigurationInstantiationSymbol(VHDLModel_ConfigurationInstantiationSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ConfigurationInstantiationSymbol)
    def __init__(self, identifierNode: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, identifierNode)


@export
class EntitySymbol(VHDLModel_EntitySymbol, DOMMixin):
    @InheritDocString(VHDLModel_EntitySymbol)
    def __init__(self, identifierNode: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, identifierNode)


@export
class ArchitectureSymbol(VHDLModel_ArchitectureSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ArchitectureSymbol)
    def __init__(self, identifierNode: Iir, identifier: str, prefix: EntitySymbol):
        super().__init__(identifier, prefix)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageSymbol(VHDLModel_PackageSymbol, DOMMixin):
    @InheritDocString(VHDLModel_PackageSymbol)
    def __init__(self, identifierNode: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, identifierNode)

# TODO: ||||                 ||||
# TODO: VVVV   old symbols   VVVV

@export
class SimpleSubtypeSymbol(VHDLModel_SimpleSubtypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subtypeName: Name):
        if isinstance(subtypeName, (List, Iterator)):
            subtypeName = ".".join(subtypeName)

        super().__init__(subtypeName=subtypeName)
        DOMMixin.__init__(self, node)


@export
class ConstrainedScalarSubtypeSymbol(VHDLModel_ConstrainedScalarSubtypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subtypeName: Name, rng: Range = None):
        super().__init__(subtypeName, rng)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class ConstrainedCompositeSubtypeSymbol(VHDLModel_ConstrainedCompositeSubtypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subtypeName: Name, constraints: List[ConstraintUnion] = None):
        super().__init__(subtypeName, constraints)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class SimpleObjectOrFunctionCallSymbol(VHDLModel_SimpleObjectOrFunctionCallSymbol, DOMMixin):
    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)
        return cls(name)


@export
class IndexedObjectOrFunctionCallSymbol(VHDLModel_IndexedObjectOrFunctionCallSymbol, DOMMixin):
    def __init__(self, node: Iir, name: Name):
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)

        return cls(node, name)
