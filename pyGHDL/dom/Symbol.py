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
from typing import List, Iterator, Iterable

from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel import Name
from pyVHDLModel.Base import ExpressionUnion
from pyVHDLModel.Symbol import LibraryReferenceSymbol as VHDLModel_LibraryReferenceSymbol
from pyVHDLModel.Symbol import PackageReferenceSymbol as VHDLModel_PackageReferenceSymbol
from pyVHDLModel.Symbol import PackageMembersReferenceSymbol as VHDLModel_PackageMembersReferenceSymbol
from pyVHDLModel.Symbol import AllPackageMembersReferenceSymbol as VHDLModel_AllPackageMembersReferenceSymbol
from pyVHDLModel.Symbol import ContextReferenceSymbol as VHDLModel_ContextReferenceSymbol
from pyVHDLModel.Symbol import EntitySymbol as VHDLModel_EntitySymbol
from pyVHDLModel.Symbol import ArchitectureSymbol as VHDLModel_ArchitectureSymbol
from pyVHDLModel.Symbol import PackageSymbol as VHDLModel_PackageSymbol
from pyVHDLModel.Symbol import EntityInstantiationSymbol as VHDLModel_EntityInstantiationSymbol
from pyVHDLModel.Symbol import ComponentInstantiationSymbol as VHDLModel_ComponentInstantiationSymbol
from pyVHDLModel.Symbol import ConfigurationInstantiationSymbol as VHDLModel_ConfigurationInstantiationSymbol
from pyVHDLModel.Symbol import SimpleSubtypeSymbol as VHDLModel_SimpleSubtypeSymbol
from pyVHDLModel.Symbol import ConstrainedScalarSubtypeSymbol as VHDLModel_ConstrainedScalarSubtypeSymbol
from pyVHDLModel.Symbol import ConstrainedCompositeSubtypeSymbol as VHDLModel_ConstrainedCompositeSubtypeSymbol
from pyVHDLModel.Symbol import SimpleObjectOrFunctionCallSymbol as VHDLModel_SimpleObjectOrFunctionCallSymbol
from pyVHDLModel.Symbol import IndexedObjectOrFunctionCallSymbol as VHDLModel_IndexedObjectOrFunctionCallSymbol

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
    def __init__(self, node: Iir, subtypeName: str):
        if isinstance(subtypeName, (List, Iterator)):
            subtypeName = ".".join(subtypeName)

        super().__init__(subtypeName)
        DOMMixin.__init__(self, node)


@export
class ConstrainedScalarSubtypeSymbol(VHDLModel_ConstrainedScalarSubtypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subtypeName: Name, rng: Range = None):
        super().__init__(subtypeName)  # , rng)  # XXX: hacked
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class ConstrainedCompositeSubtypeSymbol(VHDLModel_ConstrainedCompositeSubtypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subtypeName: Name, constraints: List = None):
        super().__init__(subtypeName)  # , constraints)  # XXX: hacked
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class SimpleObjectOrFunctionCallSymbol(VHDLModel_SimpleObjectOrFunctionCallSymbol, DOMMixin):
    def __init__(self, node: Iir, identifier: str):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)

        return cls(node, str(name))  # XXX: hacked


@export
class IndexedObjectOrFunctionCallSymbol(VHDLModel_IndexedObjectOrFunctionCallSymbol, DOMMixin):
    def __init__(self, node: Iir, prefix: Name, indices: Iterable[ExpressionUnion]):
        super().__init__(prefix, indices)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)

        return cls(node, name, [])
