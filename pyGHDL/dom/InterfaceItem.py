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
from typing import List

from pyTooling.Decorators import export

from pyVHDLModel.SyntaxModel import (
    GenericConstantInterfaceItem as VHDLModel_GenericConstantInterfaceItem,
    GenericTypeInterfaceItem as VHDLModel_GenericTypeInterfaceItem,
    GenericPackageInterfaceItem as VHDLModel_GenericPackageInterfaceItem,
    GenericProcedureInterfaceItem as VHDLModel_GenericProcedureInterfaceItem,
    GenericFunctionInterfaceItem as VHDLModel_GenericFunctionInterfaceItem,
    PortSignalInterfaceItem as VHDLModel_PortSignalInterfaceItem,
    ParameterConstantInterfaceItem as VHDLModel_ParameterConstantInterfaceItem,
    ParameterVariableInterfaceItem as VHDLModel_ParameterVariableInterfaceItem,
    ParameterSignalInterfaceItem as VHDLModel_ParameterSignalInterfaceItem,
    ParameterFileInterfaceItem as VHDLModel_ParameterFileInterfaceItem,
    Mode,
    SubtypeOrSymbol,
    ExpressionUnion,
)

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetModeOfNode
from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode, GetExpressionFromNode


__all__ = []


@export
class GenericConstantInterfaceItem(VHDLModel_GenericConstantInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion,
    ):
        super().__init__(identifiers, mode, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericConstantInterfaceItem":
        name = GetNameOfNode(genericNode)
        mode = GetModeOfNode(genericNode)
        subtypeIndication = GetSubtypeIndicationFromNode(genericNode, "generic", name)
        default = nodes.Get_Default_Value(genericNode)
        value = GetExpressionFromNode(default) if default else None

        return cls(
            genericNode,
            [
                name,
            ],
            mode,
            subtypeIndication,
            value,
        )


@export
class GenericTypeInterfaceItem(VHDLModel_GenericTypeInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericTypeInterfaceItem":
        name = GetNameOfNode(genericNode)

        return cls(genericNode, name)


@export
class GenericPackageInterfaceItem(VHDLModel_GenericPackageInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        name: str,
    ):
        super().__init__(name)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericPackageInterfaceItem":
        name = GetNameOfNode(genericNode)

        return cls(genericNode, name)


@export
class GenericProcedureInterfaceItem(VHDLModel_GenericProcedureInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericProcedureInterfaceItem":
        name = GetNameOfNode(genericNode)

        return cls(genericNode, name)


@export
class GenericFunctionInterfaceItem(VHDLModel_GenericFunctionInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifier: str,
    ):
        super().__init__(identifier)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericFunctionInterfaceItem":
        name = GetNameOfNode(genericNode)

        return cls(genericNode, name)


@export
class PortSignalInterfaceItem(VHDLModel_PortSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion = None,
    ):
        super().__init__(identifiers, mode, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, portNode: Iir) -> "PortSignalInterfaceItem":
        name = GetNameOfNode(portNode)
        mode = GetModeOfNode(portNode)
        subtypeIndication = GetSubtypeIndicationFromNode(portNode, "port", name)

        defaultValue = nodes.Get_Default_Value(portNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(
            portNode,
            [
                name,
            ],
            mode,
            subtypeIndication,
            value,
        )


@export
class ParameterConstantInterfaceItem(VHDLModel_ParameterConstantInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion = None,
    ):
        super().__init__(identifiers, mode, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir) -> "ParameterConstantInterfaceItem":
        name = GetNameOfNode(parameterNode)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(
            parameterNode,
            [
                name,
            ],
            mode,
            subtypeIndication,
            value,
        )


@export
class ParameterVariableInterfaceItem(VHDLModel_ParameterVariableInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion = None,
    ):
        super().__init__(identifiers, mode, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir) -> "ParameterVariableInterfaceItem":
        name = GetNameOfNode(parameterNode)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(
            parameterNode,
            [
                name,
            ],
            mode,
            subtypeIndication,
            value,
        )


@export
class ParameterSignalInterfaceItem(VHDLModel_ParameterSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: SubtypeOrSymbol,
        defaultExpression: ExpressionUnion = None,
    ):
        super().__init__(identifiers, mode, subtype, defaultExpression)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir) -> "ParameterSignalInterfaceItem":
        name = GetNameOfNode(parameterNode)
        mode = GetModeOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        defaultValue = nodes.Get_Default_Value(parameterNode)
        value = GetExpressionFromNode(defaultValue) if defaultValue != nodes.Null_Iir else None

        return cls(
            parameterNode,
            [
                name,
            ],
            mode,
            subtypeIndication,
            value,
        )


@export
class ParameterFileInterfaceItem(VHDLModel_ParameterFileInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        subtype: SubtypeOrSymbol,
    ):
        super().__init__(identifiers, subtype)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir) -> "ParameterFileInterfaceItem":
        name = GetNameOfNode(parameterNode)
        subtypeIndication = GetSubtypeIndicationFromNode(parameterNode, "parameter", name)

        return cls(
            parameterNode,
            [
                name,
            ],
            subtypeIndication,
        )
