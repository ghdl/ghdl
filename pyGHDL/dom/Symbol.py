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

from pydecor import export

from pyVHDLModel.VHDLModel import (
    EntitySymbol as VHDLModel_EntitySymbol,
    SimpleSubTypeSymbol as VHDLModel_SimpleSubTypeSymbol,
    ConstrainedScalarSubTypeSymbol as VHDLModel_ConstrainedScalarSubTypeSymbol,
    ConstrainedCompositeSubTypeSymbol as VHDLModel_ConstrainedCompositeSubTypeSymbol,
    EnumerationLiteralSymbol as VHDLModel_EnumerationLiteralSymbol,
    SimpleObjectOrFunctionCallSymbol as VHDLModel_SimpleObjectOrFunctionCallSymbol,
    IndexedObjectOrFunctionCallSymbol as VHDLModel_IndexedObjectOrFunctionCallSymbol,
    Constraint,
    Name,
)
from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin
from pyGHDL.dom.Range import Range


__all__ = []


@export
class EntitySymbol(VHDLModel_EntitySymbol, DOMMixin):
    def __init__(self, node: Iir, entityName: Name):
        super().__init__(entityName)
        DOMMixin.__init__(self, node)


@export
class EnumerationLiteralSymbol(VHDLModel_EnumerationLiteralSymbol, DOMMixin):
    def __init__(self, node: Iir, literalName: Name):
        super().__init__(literalName)
        DOMMixin.__init__(self, node)


@export
class SimpleSubTypeSymbol(VHDLModel_SimpleSubTypeSymbol, DOMMixin):
    def __init__(self, node: Iir, subTypeName: Name):
        if isinstance(subTypeName, (List, Iterator)):
            subTypeName = ".".join(subTypeName)

        super().__init__(subTypeName=subTypeName)
        DOMMixin.__init__(self, node)


@export
class ConstrainedScalarSubTypeSymbol(
    VHDLModel_ConstrainedScalarSubTypeSymbol, DOMMixin
):
    def __init__(self, node: Iir, subTypeName: Name, rng: Range = None):
        super().__init__(subTypeName, rng)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class ConstrainedCompositeSubTypeSymbol(
    VHDLModel_ConstrainedCompositeSubTypeSymbol, DOMMixin
):
    def __init__(
        self, node: Iir, subTypeName: Name, constraints: List[Constraint] = None
    ):
        super().__init__(subTypeName, constraints)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class SimpleObjectOrFunctionCallSymbol(
    VHDLModel_SimpleObjectOrFunctionCallSymbol, DOMMixin
):
    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)
        return cls(name)


@export
class IndexedObjectOrFunctionCallSymbol(
    VHDLModel_IndexedObjectOrFunctionCallSymbol, DOMMixin
):
    def __init__(self, node: Iir, name: Name):
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetNameFromNode

        name = GetNameFromNode(node)

        return cls(node, name)
