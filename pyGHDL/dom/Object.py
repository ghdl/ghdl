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
This module implements derived object classes from :mod:`pyVHDLModel.Object`.
"""
from typing import Union, List, Iterable

from pyTooling.Decorators import export

from pyVHDLModel.Base import ExpressionUnion
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Object import Constant as VHDLModel_Constant
from pyVHDLModel.Object import DeferredConstant as VHDLModel_DeferredConstant
from pyVHDLModel.Object import Variable as VHDLModel_Variable
from pyVHDLModel.Object import SharedVariable as VHDLModel_SharedVariable
from pyVHDLModel.Object import Signal as VHDLModel_Signal
from pyVHDLModel.Object import File as VHDLModel_File

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetDocumentationOfNode


@export
class Constant(VHDLModel_Constant, DOMMixin):
    """
    Represents a *constant*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.Constant`.

    .. admonition:: Example

       .. code-block:: VHDL

          constant BITS : positive := 8;
    """

    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: Symbol,
        defaultExpression: ExpressionUnion,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(
        cls, constantNode: Iir, furtherIdentifiers: Iterable[str] = None
    ) -> Union["Constant", "DeferredConstant"]:
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(constantNode)
        documentation = GetDocumentationOfNode(constantNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(constantNode, "constant", name)
        defaultValue = nodes.Get_Default_Value(constantNode)
        if defaultValue != nodes.Null_Iir:
            defaultExpression = GetExpressionFromNode(defaultValue)

            return cls(constantNode, identifiers, subtypeIndication, defaultExpression, documentation)
        else:
            return DeferredConstant(constantNode, identifiers, subtypeIndication, documentation)


@export
class DeferredConstant(VHDLModel_DeferredConstant, DOMMixin):
    """
    Represents a *deferred constant*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.DeferredConstant`.

    .. admonition:: Example

       .. code-block:: VHDL

          constant BITS : positive;
    """

    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        super().__init__(identifiers, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, constantNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "DeferredConstant":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(constantNode)
        documentation = GetDocumentationOfNode(constantNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(constantNode, "deferred constant", name)

        return cls(constantNode, identifiers, subtypeIndication, documentation)


@export
class Variable(VHDLModel_Variable, DOMMixin):
    """
    Represents a *variable*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.Variable`.

    .. admonition:: Example

       .. code-block:: VHDL

          variable result : natural := 0;
    """

    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: Symbol,
        defaultExpression: ExpressionUnion,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, variableNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "Variable":
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(variableNode)
        documentation = GetDocumentationOfNode(variableNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(variableNode, "variable", name)
        defaultValue = nodes.Get_Default_Value(variableNode)
        defaultExpression = None
        if defaultValue != nodes.Null_Iir:
            defaultExpression = GetExpressionFromNode(defaultValue)

        return cls(variableNode, identifiers, subtypeIndication, defaultExpression, documentation)


@export
class SharedVariable(VHDLModel_SharedVariable, DOMMixin):
    """
    Represents a *shared variable*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.SharedVariable`.

    .. admonition:: Example

       .. code-block:: VHDL

          shared variable counter : counter_pt;
    """

    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        super().__init__(identifiers, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, variableNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "SharedVariable":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(variableNode)
        documentation = GetDocumentationOfNode(variableNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(variableNode, "variable", name)

        return cls(variableNode, identifiers, subtypeIndication, documentation)


@export
class Signal(VHDLModel_Signal, DOMMixin):
    """
    Represents a *signal*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.Signal`.

    .. admonition:: Example

       .. code-block:: VHDL

          signal counter : unsigned := (others => '0');
    """

    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: Symbol,
        defaultExpression: ExpressionUnion,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, signalNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "Signal":
        from pyGHDL.dom._Translate import (
            GetSubtypeIndicationFromNode,
            GetExpressionFromNode,
        )

        name = GetNameOfNode(signalNode)
        documentation = GetDocumentationOfNode(signalNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(signalNode, "signal", name)
        default = nodes.Get_Default_Value(signalNode)
        defaultExpression = GetExpressionFromNode(default) if default else None

        return cls(signalNode, identifiers, subtypeIndication, defaultExpression, documentation)


@export
class File(VHDLModel_File, DOMMixin):
    """
    Represents a *file*.

    This class implements a :mod:`pyGHDL.dom` object derived from :class:`pyVHDLModel.Object.File`.

    .. admonition:: Example

       .. code-block:: VHDL

          file INPUT : TEXT;
    """

    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        super().__init__(identifiers, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, fileNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "File":
        from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode

        name = GetNameOfNode(fileNode)
        documentation = GetDocumentationOfNode(fileNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(fileNode, "file", name)

        # FIXME: handle file open stuff

        return cls(fileNode, identifiers, subtypeIndication, documentation)
