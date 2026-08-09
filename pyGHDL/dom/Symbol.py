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

from typing import List, Mapping

from pyGHDL.dom.Name import SimpleName
from pyTooling.Decorators import export, InheritDocString

from pyVHDLModel.Name import Name
from pyVHDLModel.Symbol import Symbol as VHDLModel_Symbol
from pyVHDLModel.Symbol import PossibleReference
from pyVHDLModel.Symbol import LibraryReferenceSymbol as VHDLModel_LibraryReferenceSymbol
from pyVHDLModel.Symbol import PackageReferenceSymbol as VHDLModel_PackageReferenceSymbol
from pyVHDLModel.Symbol import ModeViewSymbol as VHDLModel_ModeViewSymbol
from pyVHDLModel.Symbol import SubprogramReferenceSymbol as VHDLModel_SubprogramReferenceSymbol
from pyVHDLModel.Symbol import ConfigurationSymbol as VHDLModel_ConfigurationSymbol
from pyVHDLModel.Symbol import SignalSymbol as VHDLModel_SignalSymbol
from pyVHDLModel.Symbol import VariableSymbol as VHDLModel_VariableSymbol
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
from pyVHDLModel.Symbol import ConstrainedArraySubtypeSymbol as VHDLModel_ConstrainedArraySubtypeSymbol
from pyVHDLModel.Symbol import ConstrainedRecordSubtypeSymbol as VHDLModel_ConstrainedRecordSubtypeSymbol
from pyVHDLModel.Symbol import RecordElementSymbol as VHDLModel_RecordElementSymbol
from pyVHDLModel.Symbol import RangeAttributeSymbol as VHDLModel_RangeAttributeSymbol
from pyVHDLModel.Symbol import SimpleObjectOrFunctionCallSymbol as VHDLModel_SimpleObjectOrFunctionCallSymbol
from pyVHDLModel.Symbol import IndexedObjectOrFunctionCallSymbol as VHDLModel_IndexedObjectOrFunctionCallSymbol

from pyGHDL.libghdl._types import Iir
from pyGHDL.dom import DOMMixin
from pyVHDLModel.Base import Range


@export
class Symbol(VHDLModel_Symbol, DOMMixin):
    """
    Generic reference (name) to a language entity where no single, fixed
    :class:`~pyVHDLModel.Symbol.PossibleReference` value fits - e.g. an alias's target, which may or may not
    be restricted to an object depending on whether a subtype indication is present.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.Symbol`.
    """

    def __init__(self, identifierNode: Iir, name: Name, possibleReferences: PossibleReference) -> None:
        """
        Initializes a symbol.

        :param identifierNode:     The IIR node of the name this symbol was translated from.
        :param name:               The name referencing the language entity this symbol stands for.
        :param possibleReferences: An enumeration to filter possible references.
        """
        super().__init__(name, possibleReferences)
        DOMMixin.__init__(self, identifierNode)


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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to a library.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the library.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to a package.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the package.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ModeViewSymbol(VHDLModel_ModeViewSymbol, DOMMixin):
    """
    Represents a reference (name) to a mode view declaration.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ModeViewSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          port (p : view MyView);
          --          ^^^^^^
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference to a mode view (VHDL-2019).

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the mode view.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class SubprogramReferenceSymbol(VHDLModel_SubprogramReferenceSymbol, DOMMixin):
    """
    Represents a reference (name) to a subprogram (procedure or function).

    This class implements a :mod:`pyGHDL.dom` object derived from
    :class:`pyVHDLModel.Symbol.SubprogramReferenceSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          function f is new g generic map (...);
          --                  ^
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference to a subprogram.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the subprogram.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class ConfigurationSymbol(VHDLModel_ConfigurationSymbol, DOMMixin):
    """
    Represents a reference (name) to a configuration declaration, e.g. in an entity aspect of a
    binding indication.

    This class implements a :mod:`pyGHDL.dom` object derived from
    :class:`pyVHDLModel.Symbol.ConfigurationSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          for U1 : comp use configuration work.cfg;
          --                              ^^^^^^^
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference to a configuration.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the configuration.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class SignalSymbol(VHDLModel_SignalSymbol, DOMMixin):
    """
    Represents a reference (name) to a signal, e.g. the target of a signal assignment.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.SignalSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          s <= '1';
          --^
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a signal symbol.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the signal.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
class VariableSymbol(VHDLModel_VariableSymbol, DOMMixin):
    """
    Represents a reference (name) to a variable, e.g. the target of a variable assignment.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.VariableSymbol`.

    .. admonition:: Example

       .. code-block:: VHDL

          v := '1';
          --^
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a variable symbol.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the variable.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to a context.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the context.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to a package member.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the package member.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to all package members.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the package whose members are all referenced.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to an entity in a direct entity instantiation.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the instantiated entity.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to an entity in a component instantiation.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the instantiated component.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to an entity in a configuration instantiation.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the instantiated configuration.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to an entity in an architecture declaration.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the entity the architecture belongs to.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


@export
@InheritDocString(VHDLModel_ArchitectureSymbol, merge=True)
class ArchitectureSymbol(VHDLModel_ArchitectureSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ArchitectureSymbol`.
    """

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes an architecture symbol.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the architecture.
        """
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

    def __init__(self, identifierNode: Iir, name: Name) -> None:
        """
        Initializes a reference (name) to a package in a package body declaration.

        :param identifierNode: The IIR node of the name this symbol was translated from.
        :param name:           The name referencing the package the package body belongs to.
        """
        super().__init__(name)
        DOMMixin.__init__(self, identifierNode)


# TODO: ||||                 ||||
# TODO: VVVV   old symbols   VVVV


@export
@InheritDocString(VHDLModel_SimpleSubtypeSymbol, merge=True)
class SimpleSubtypeSymbol(VHDLModel_SimpleSubtypeSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.SimpleSubtypeSymbol`.
    """

    def __init__(self, node: Iir, subtypeName: Name) -> None:
        """
        Initializes a simple subtype symbol.

        :param node:        The IIR node this object was translated from.
        :param subtypeName: The name of the referenced subtype.
        """
        super().__init__(subtypeName)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ConstrainedScalarSubtypeSymbol, merge=True)
class ConstrainedScalarSubtypeSymbol(VHDLModel_ConstrainedScalarSubtypeSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ConstrainedScalarSubtypeSymbol`.
    """

    def __init__(self, node: Iir, subtypeName: Name, rng: Range) -> None:
        """
        Initializes a reference to a scalar subtype narrowed by a range.

        :param node:        The IIR node this object was translated from.
        :param subtypeName: The name of the referenced subtype.
        :param rng:         The range constraining the scalar subtype.
        """
        super().__init__(subtypeName, rng)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_ConstrainedArraySubtypeSymbol, merge=True)
class ConstrainedArraySubtypeSymbol(VHDLModel_ConstrainedArraySubtypeSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ConstrainedArraySubtypeSymbol`.
    """

    def __init__(self, node: Iir, subtypeName: Name, constraints: List) -> None:
        """
        Initializes a reference to an array subtype narrowed by index ranges.

        :param node:        The IIR node this object was translated from.
        :param subtypeName: The name of the referenced subtype.
        :param constraints: List of all index ranges, one per dimension.
        """
        super().__init__(subtypeName, constraints)
        DOMMixin.__init__(self, node)

    # @classmethod
    # def parse(cls, node: Iir):
    #     pass


@export
@InheritDocString(VHDLModel_ConstrainedRecordSubtypeSymbol, merge=True)
class ConstrainedRecordSubtypeSymbol(VHDLModel_ConstrainedRecordSubtypeSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.ConstrainedRecordSubtypeSymbol`.
    """

    def __init__(self, node: Iir, subtypeName: Name, constraints: Mapping) -> None:
        """
        Initializes a reference to a record subtype with constrained elements.

        :param node:        The IIR node this object was translated from.
        :param subtypeName: The name of the referenced subtype.
        :param constraints: Dictionary of the constraint per constrained record element.
        """
        super().__init__(subtypeName, constraints)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        pass


@export
@InheritDocString(VHDLModel_RecordElementSymbol, merge=True)
class RecordElementSymbol(VHDLModel_RecordElementSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.RecordElementSymbol`.
    """

    def __init__(self, node: Iir, name: SimpleName) -> None:
        """
        Initializes a reference to a record element.

        :param node: The IIR node this object was translated from.
        :param name: The name referencing the record element.
        """
        super().__init__(name)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_RangeAttributeSymbol, merge=True)
class RangeAttributeSymbol(VHDLModel_RangeAttributeSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.RangeAttributeSymbol`.
    """

    def __init__(self, node: Iir, name: Name) -> None:
        """
        Initialize a range attribute symbol.

        :param node: The IIR node this object was translated from.
        :param name: The attribute name referencing the range.
        """
        super().__init__(name)
        DOMMixin.__init__(self, node)


@export
@InheritDocString(VHDLModel_SimpleObjectOrFunctionCallSymbol, merge=True)
class SimpleObjectOrFunctionCallSymbol(VHDLModel_SimpleObjectOrFunctionCallSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.SimpleObjectOrFunctionCallSymbol`.
    """

    def __init__(self, node: Iir, name: Name) -> None:
        """
        Initializes a reference that is either an object or a parameterless function call.

        :param node: The IIR node this object was translated from.
        :param name: The name referencing the object or parameterless function.
        """
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetName

        name = GetName(node)

        return cls(node, name)


@export
@InheritDocString(VHDLModel_IndexedObjectOrFunctionCallSymbol, merge=True)
class IndexedObjectOrFunctionCallSymbol(VHDLModel_IndexedObjectOrFunctionCallSymbol, DOMMixin):
    """
    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Symbol.IndexedObjectOrFunctionCallSymbol`.
    """

    def __init__(self, node: Iir, name: Name) -> None:
        """
        Initializes a reference that is either an indexed object, a function call or a type conversion.

        :param node: The IIR node this object was translated from.
        :param name: The name referencing the object, function or type being converted.
        """
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir):
        from pyGHDL.dom._Translate import GetName

        name = GetName(node)

        return cls(node, name)
