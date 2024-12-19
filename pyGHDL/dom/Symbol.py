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
"""
This module implements derived symbol classes from :mod:`pyVHDLModel.Symbol`.
"""
from typing import List

from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel.Name import Name
from pyVHDLModel.Symbol import LibraryReferenceSymbol as VHDLModel_LibraryReferenceSymbol
from pyVHDLModel.Symbol import PackageReferenceSymbol as VHDLModel_PackageReferenceSymbol
from pyVHDLModel.Symbol import PackageMemberReferenceSymbol as VHDLModel_PackageMemberReferenceSymbol
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
    """
    Represents a reference (name) to a library.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.LibraryReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          library ieee;
          --      ^^^^
    """

    @InheritDocString(VHDLModel_LibraryReferenceSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageReferenceSymbol(VHDLModel_PackageReferenceSymbol, DOMMixin):
    """
    Represents a reference (name) to a package.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.PackageReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          use ieee.numeric_std;
          --  ^^^^^^^^^^^^^^^^
    """

    @InheritDocString(VHDLModel_PackageReferenceSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ContextReferenceSymbol(VHDLModel_ContextReferenceSymbol, DOMMixin):
    """
    Represents a reference (name) to a context.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ContextReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          context ieee.ieee_std_context;
          --      ^^^^^^^^^^^^^^^^^^^^^
    """

    @InheritDocString(VHDLModel_ContextReferenceSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageMemberReferenceSymbol(VHDLModel_PackageMemberReferenceSymbol, DOMMixin):
    """
    Represents a reference (name) to a package member.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.PackageMemberReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          use ieee.numeric_std.unsigned;
          --  ^^^^^^^^^^^^^^^^^^^^^^^^^
    """

    @InheritDocString(VHDLModel_PackageMemberReferenceSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class AllPackageMembersReferenceSymbol(VHDLModel_AllPackageMembersReferenceSymbol, DOMMixin):
    """
    Represents a reference (name) to all members in a package.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.AllPackageMembersReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          use ieee.numeric_std.all;
          --  ^^^^^^^^^^^^^^^^^^^^
    """

    @InheritDocString(VHDLModel_AllPackageMembersReferenceSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class EntityInstantiationSymbol(VHDLModel_EntityInstantiationSymbol, DOMMixin):
    """
    Represents a reference (name) to an entity in a direct entity instantiation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.EntityInstantiationSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          inst : entity work.Counter;
          --            ^^^^^^^^^^^^
    """

    @InheritDocString(VHDLModel_EntityInstantiationSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ComponentInstantiationSymbol(VHDLModel_ComponentInstantiationSymbol, DOMMixin):
    """
    Represents a reference (name) to a component in a component instantiation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ComponentInstantiationSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          inst : component Counter;
          --               ^^^^^^^
    """

    @InheritDocString(VHDLModel_ComponentInstantiationSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ConfigurationInstantiationSymbol(VHDLModel_ConfigurationInstantiationSymbol, DOMMixin):
    """
    Represents a reference (name) to a configuration in a configuration instantiation.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ConfigurationInstantiationSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          inst : configuration Counter;
          --                   ^^^^^^^
    """

    @InheritDocString(VHDLModel_ConfigurationInstantiationSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class EntitySymbol(VHDLModel_EntitySymbol, DOMMixin):
    """
    Represents a reference (name) to an entity in an architecture declaration.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.EntitySymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          architecture rtl of Counter is
          --                  ^^^^^^^
          begin
          end architecture;
    """

    @InheritDocString(VHDLModel_EntitySymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ArchitectureSymbol(VHDLModel_ArchitectureSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ArchitectureSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class PackageSymbol(VHDLModel_PackageSymbol, DOMMixin):
    """
    Represents a reference (name) to a package in a package body declaration.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.PackageSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          package body utilities is
          --           ^^^^^^^^^
          end package body;
    """

    @InheritDocString(VHDLModel_PackageSymbol)
    def __init__(self, identifierNode: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


# TODO: ||||                 ||||
# TODO: VVVV   old symbols   VVVV


@export
class SimpleSubtypeSymbol(VHDLModel_SimpleSubtypeSymbol, DOMMixin):
    @InheritDocString(VHDLModel_SimpleSubtypeSymbol)
    def __init__(self, node: Iir, subtypeName: Name) -> None:
        super().__init__(subtypeName)
        DOMMixin.__init__(self, node)


@export
class ConstrainedScalarSubtypeSymbol(VHDLModel_ConstrainedScalarSubtypeSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ConstrainedScalarSubtypeSymbol)
    def __init__(self, node: Iir, subtypeName: Name, rng: Range = None) -> None:
        super().__init__(subtypeName)  # , rng)  # XXX: hacked
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class ConstrainedCompositeSubtypeSymbol(VHDLModel_ConstrainedCompositeSubtypeSymbol, DOMMixin):
    @InheritDocString(VHDLModel_ConstrainedCompositeSubtypeSymbol)
    def __init__(self, node: Iir, subtypeName: Name, constraints: List = None) -> None:
        super().__init__(subtypeName)  # , constraints)  # XXX: hacked
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
class SimpleObjectOrFunctionCallSymbol(VHDLModel_SimpleObjectOrFunctionCallSymbol, DOMMixin):
    @InheritDocString(VHDLModel_SimpleObjectOrFunctionCallSymbol)
    def __init__(self, node: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetName

        name = GetName(node)

        return cls(node, name)


@export
class IndexedObjectOrFunctionCallSymbol(VHDLModel_IndexedObjectOrFunctionCallSymbol, DOMMixin):
    @InheritDocString(VHDLModel_IndexedObjectOrFunctionCallSymbol)
    def __init__(self, node: Iir, name: Name) -> None:
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetName

        name = GetName(node)

        return cls(node, name)
