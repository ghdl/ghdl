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
# Package module:   DOM: Elements not covered by the VHDL standard.
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
.. todo::
   Add a module documentation.
"""

from pyTooling.Decorators import export

from pyVHDLModel.Declaration import Alias as VHDLModel_Alias
from pyVHDLModel.Name import Name
from pyVHDLModel.Symbol import Symbol

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode, GetIirKindOfNode


@export
class Alias(VHDLModel_Alias, DOMMixin):
    def __init__(
        self,
        node: Iir,
        aliasName: str,
        name: Name,
        subtype: Symbol = None,
        documentation: str = None,
    ) -> None:
        super().__init__(aliasName, name, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, aliasNode: Iir):
        from pyGHDL.dom._Translate import GetName, GetSubtypeIndicationFromNode

        aliasName = GetNameOfNode(aliasNode)
        documentation = GetDocumentationOfNode(aliasNode)

        nameNode = nodes.Get_Name(aliasNode)
        if GetIirKindOfNode(nameNode) == nodes.Iir_Kind.Signature:
            # FIXME: the parameter/return type marks of the signature (used to disambiguate between
            #        overloaded subprograms/operators, e.g. 'add[integer, integer return integer]')
            #        are not captured - only the aliased name itself (the signature's prefix).
            name = GetName(nodes.Get_Signature_Prefix(nameNode))
        else:
            name = GetName(nameNode)

        subtypeIndicationNode = nodes.Get_Subtype_Indication(aliasNode)
        subtype = (
            None
            if subtypeIndicationNode == nodes.Null_Iir
            else GetSubtypeIndicationFromNode(aliasNode, "alias", aliasName)
        )

        return cls(aliasNode, aliasName, name, subtype, documentation)
