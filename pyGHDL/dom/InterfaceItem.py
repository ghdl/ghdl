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
from typing import List, Iterable

from pyTooling.Decorators import export

from pyVHDLModel.Base import Mode, ExpressionUnion
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Interface import GenericConstantInterfaceItem as VHDLModel_GenericConstantInterfaceItem
from pyVHDLModel.Interface import GenericTypeInterfaceItem as VHDLModel_GenericTypeInterfaceItem
from pyVHDLModel.Interface import GenericProcedureInterfaceItem as VHDLModel_GenericProcedureInterfaceItem
from pyVHDLModel.Interface import GenericFunctionInterfaceItem as VHDLModel_GenericFunctionInterfaceItem
from pyVHDLModel.Interface import GenericPackageInterfaceItem as VHDLModel_GenericPackageInterfaceItem
from pyVHDLModel.Interface import PortSignalInterfaceItem as VHDLModel_PortSignalInterfaceItem
from pyVHDLModel.Interface import ParameterConstantInterfaceItem as VHDLModel_ParameterConstantInterfaceItem
from pyVHDLModel.Interface import ParameterVariableInterfaceItem as VHDLModel_ParameterVariableInterfaceItem
from pyVHDLModel.Interface import ParameterSignalInterfaceItem as VHDLModel_ParameterSignalInterfaceItem
from pyVHDLModel.Interface import ParameterFileInterfaceItem as VHDLModel_ParameterFileInterfaceItem

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetModeOfNode, GetDocumentationOfNode
from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode, GetExpressionFromNode


@export
class GenericConstantInterfaceItem(VHDLModel_GenericConstantInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "GenericConstantInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(genericNode)
        subtypeIndication = GetSubtypeIndicationFromNode(genericNode, "generic", name)
        default = nodes.Get_Default_Value(genericNode)
        value = GetExpressionFromNode(default) if default else None

        return cls(genericNode, identifiers, mode, subtypeIndication, value, documentation)


@export
class GenericTypeInterfaceItem(VHDLModel_GenericTypeInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifier: str, documentation: str = None) -> None:
        super().__init__(identifier, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericTypeInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        return cls(genericNode, name, documentation)


@export
class GenericPackageInterfaceItem(VHDLModel_GenericPackageInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, name: str, documentation: str = None) -> None:
        super().__init__(name, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericPackageInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        return cls(genericNode, name, documentation)


@export
class GenericProcedureInterfaceItem(VHDLModel_GenericProcedureInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifier: str, documentation: str = None) -> None:
        super().__init__(identifier, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericProcedureInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        return cls(genericNode, name, documentation)


@export
class GenericFunctionInterfaceItem(VHDLModel_GenericFunctionInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifier: str, documentation: str = None) -> None:
        super().__init__(identifier, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericFunctionInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        return cls(genericNode, name, documentation)


@export
class PortSignalInterfaceItem(VHDLModel_PortSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, portNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "PortSignalInterfaceItem":
        name = GetNameOfNode(portNode)
        documentation = GetDocumentationOfNode(portNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(portNode)
        subtypeIndication = GetSubtypeIndicationFromNode(portNode, "port", name)

        defaultValue = nodes.Get_Default_Value(portNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(portNode, identifiers, mode, subtypeIndication, value, documentation)


@export
class ParameterConstantInterfaceItem(VHDLModel_ParameterConstantInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "ParameterConstantInterfaceItem":
        name = GetNameOfNode(parameterNode)
        documentation = GetDocumentationOfNode(parameterNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(parameterNode, identifiers, mode, subtypeIndication, value, documentation)


@export
class ParameterVariableInterfaceItem(VHDLModel_ParameterVariableInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "ParameterVariableInterfaceItem":
        name = GetNameOfNode(parameterNode)
        documentation = GetDocumentationOfNode(parameterNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(parameterNode, identifiers, mode, subtypeIndication, value, documentation)


@export
class ParameterSignalInterfaceItem(VHDLModel_ParameterSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "ParameterSignalInterfaceItem":
        name = GetNameOfNode(parameterNode)
        documentation = GetDocumentationOfNode(parameterNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(parameterNode, identifiers, mode, subtypeIndication, value, documentation)


@export
class ParameterFileInterfaceItem(VHDLModel_ParameterFileInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        super().__init__(identifiers, subtype, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "ParameterFileInterfaceItem":
        name = GetNameOfNode(parameterNode)
        documentation = GetDocumentationOfNode(parameterNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        return cls(parameterNode, identifiers, subtypeIndication, documentation)
