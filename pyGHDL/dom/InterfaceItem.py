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
This module implements derived interface item classes from :mod:`pyVHDLModel.Interface`.
"""

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
from pyVHDLModel.Interface import PortSimpleSignalInterfaceItem as VHDLModel_PortSimpleSignalInterfaceItem
from pyVHDLModel.Interface import PortViewSignalInterfaceItem as VHDLModel_PortViewSignalInterfaceItem
from pyVHDLModel.Interface import ParameterConstantInterfaceItem as VHDLModel_ParameterConstantInterfaceItem
from pyVHDLModel.Interface import ParameterVariableInterfaceItem as VHDLModel_ParameterVariableInterfaceItem
from pyVHDLModel.Interface import ParameterSignalInterfaceItem as VHDLModel_ParameterSignalInterfaceItem
from pyVHDLModel.Interface import ParameterSimpleSignalInterfaceItem as VHDLModel_ParameterSimpleSignalInterfaceItem
from pyVHDLModel.Interface import ParameterViewSignalInterfaceItem as VHDLModel_ParameterViewSignalInterfaceItem
from pyVHDLModel.Interface import ParameterFileInterfaceItem as VHDLModel_ParameterFileInterfaceItem
from pyVHDLModel.Interface import ModeViewDeclaration as VHDLModel_ModeViewDeclaration
from pyVHDLModel.Interface import SimpleModeViewElement as VHDLModel_SimpleModeViewElement
from pyVHDLModel.Interface import CompositeModeViewElement as VHDLModel_CompositeModeViewElement

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetModeOfNode, GetDocumentationOfNode
from pyGHDL.dom._Translate import GetSubtypeIndicationFromNode, GetExpressionFromNode, GetName
from pyGHDL.dom.Symbol import ModeViewSymbol, SimpleSubtypeSymbol


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
        """
        Initializes a constant in a generic clause.

        :param node:              The IIR node this object was translated from.
        :param identifiers:       A list of identifiers.
        :param mode:              The interface item's mode.
        :param subtype:           Reference to the object's subtype.
        :param defaultExpression: The default value, or ``None`` if none was given.
        :param documentation:     The documentation comment associated with this declaration.
        """
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
        """
        Initializes a type in a generic clause.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The generic type's identifier.
        :param documentation: The documentation comment associated with this declaration.
        """
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
        """
        Initializes a package in a generic clause.

        :param node:          The IIR node this object was translated from.
        :param name:          The generic package's identifier.
        :param documentation: The documentation comment associated with this declaration.
        """
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
        """
        Initializes a procedure in a generic clause.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The generic procedure's identifier.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericProcedureInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        return cls(genericNode, name, documentation)


@export
class GenericFunctionInterfaceItem(VHDLModel_GenericFunctionInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifier: str, returnType: Symbol, documentation: str = None) -> None:
        """
        Initializes a function in a generic clause.

        :param node:          The IIR node this object was translated from.
        :param identifier:    The generic function's identifier.
        :param returnType:    Reference to the subtype of the function's return value.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, returnType, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, genericNode: Iir) -> "GenericFunctionInterfaceItem":
        name = GetNameOfNode(genericNode)
        documentation = GetDocumentationOfNode(genericNode)

        returnType = nodes.Get_Return_Type_Mark(genericNode)
        returnTypeName = GetName(returnType)
        returnTypeSymbol = SimpleSubtypeSymbol(returnType, returnTypeName)

        return cls(genericNode, name, returnTypeSymbol, documentation)


@export
class PortSimpleSignalInterfaceItem(VHDLModel_PortSimpleSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        """
        Initializes a port declared with a simple mode.

        :param node:              The IIR node this object was translated from.
        :param identifiers:       A list of identifiers.
        :param mode:              The interface item's mode.
        :param subtype:           Reference to the object's subtype.
        :param defaultExpression: The default value, or ``None`` if none was given.
        :param documentation:     The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, portNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "PortSimpleSignalInterfaceItem":
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
class PortViewSignalInterfaceItem(VHDLModel_PortViewSignalInterfaceItem, DOMMixin):
    """
    .. admonition:: Example

       .. code-block:: VHDL

          port (p : view MyView);

    .. note::

       ``Get_Subtype_Indication`` on an ``Interface_View_Declaration`` node is only populated after semantic
       analysis has resolved the referenced mode view; since translation here is parse-only, ``Subtype``
       stays ``None`` - the type is only implied by the referenced mode view.
    """

    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        modeViewIndication: ModeViewSymbol,
        documentation: str = None,
    ) -> None:
        """
        Initializes a port declared with a mode view (VHDL-2019).

        :param node:               The IIR node this object was translated from.
        :param identifiers:        A list of identifiers.
        :param modeViewIndication: Reference to the mode view applied to this port.
        :param documentation:      The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, modeViewIndication, documentation=documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, portNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "PortViewSignalInterfaceItem":
        name = GetNameOfNode(portNode)
        documentation = GetDocumentationOfNode(portNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)

        modeViewIndicationNode = nodes.Get_Mode_View_Indication(portNode)
        modeViewNameNode = nodes.Get_Name(modeViewIndicationNode)
        modeViewIndication = ModeViewSymbol(modeViewNameNode, GetName(modeViewNameNode))

        return cls(portNode, identifiers, modeViewIndication, documentation)


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
        """
        Initializes a constant parameter of a subprogram.

        :param node:              The IIR node this object was translated from.
        :param identifiers:       A list of identifiers.
        :param mode:              The interface item's mode.
        :param subtype:           Reference to the object's subtype.
        :param defaultExpression: The default value, or ``None`` if none was given.
        :param documentation:     The documentation comment associated with this declaration.
        """
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
        """
        Initializes a variable parameter of a subprogram.

        :param node:              The IIR node this object was translated from.
        :param identifiers:       A list of identifiers.
        :param mode:              The interface item's mode.
        :param subtype:           Reference to the object's subtype.
        :param defaultExpression: The default value, or ``None`` if none was given.
        :param documentation:     The documentation comment associated with this declaration.
        """
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
class ParameterSimpleSignalInterfaceItem(VHDLModel_ParameterSimpleSignalInterfaceItem, DOMMixin):
    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        mode: Mode,
        subtype: Symbol,
        defaultExpression: ExpressionUnion = None,
        documentation: str = None,
    ) -> None:
        """
        Initializes a signal parameter declared with a simple mode.

        :param node:              The IIR node this object was translated from.
        :param identifiers:       A list of identifiers.
        :param mode:              The interface item's mode.
        :param subtype:           Reference to the object's subtype.
        :param defaultExpression: The default value, or ``None`` if none was given.
        :param documentation:     The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, mode, subtype, defaultExpression, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(
        cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None
    ) -> "ParameterSimpleSignalInterfaceItem":
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
class ParameterViewSignalInterfaceItem(VHDLModel_ParameterViewSignalInterfaceItem, DOMMixin):
    """
    .. admonition:: Example

       .. code-block:: VHDL

          procedure proc(signal s : view MyView);

    .. note::

       See :class:`PortViewSignalInterfaceItem` for why ``Subtype`` stays ``None`` here.
    """

    def __init__(
        self,
        node: Iir,
        identifiers: List[str],
        modeViewIndication: ModeViewSymbol,
        documentation: str = None,
    ) -> None:
        """
        Initializes a signal parameter declared with a mode view (VHDL-2019).

        :param node:               The IIR node this object was translated from.
        :param identifiers:        A list of identifiers.
        :param modeViewIndication: Reference to the mode view applied to this parameter.
        :param documentation:      The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, modeViewIndication, documentation=documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, parameterNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "ParameterViewSignalInterfaceItem":
        name = GetNameOfNode(parameterNode)
        documentation = GetDocumentationOfNode(parameterNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)

        modeViewIndicationNode = nodes.Get_Mode_View_Indication(parameterNode)
        modeViewNameNode = nodes.Get_Name(modeViewIndicationNode)
        modeViewIndication = ModeViewSymbol(modeViewNameNode, GetName(modeViewNameNode))

        return cls(parameterNode, identifiers, modeViewIndication, documentation)


@export
class ParameterFileInterfaceItem(VHDLModel_ParameterFileInterfaceItem, DOMMixin):
    def __init__(self, node: Iir, identifiers: List[str], subtype: Symbol, documentation: str = None) -> None:
        """
        Initializes a file parameter of a subprogram.

        :param node:          The IIR node this object was translated from.
        :param identifiers:   A list of identifiers.
        :param subtype:       Reference to the object's subtype.
        :param documentation: The documentation comment associated with this declaration.
        """
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


@export
class SimpleModeViewElement(VHDLModel_SimpleModeViewElement, DOMMixin):
    """
    .. admonition:: Example

       .. code-block:: VHDL

          view MyView of RecordType is
            a, b : out;
          end view;
    """

    def __init__(self, node: Iir, identifiers: List[str], mode: Mode, documentation: str = None) -> None:
        """
        Initializes a simple mode view element.

        :param node:          The IIR node this object was translated from.
        :param identifiers:   A list of identifiers.
        :param mode:          The element's mode.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, mode, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "SimpleModeViewElement":
        name = GetNameOfNode(elementNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)
        mode = GetModeOfNode(elementNode)
        documentation = GetDocumentationOfNode(elementNode)

        return cls(elementNode, identifiers, mode, documentation)


@export
class CompositeModeViewElement(VHDLModel_CompositeModeViewElement, DOMMixin):
    """
    .. admonition:: Example

       .. code-block:: VHDL

          view OuterView of OuterRecord is
            b : view InnerView;
          end view;
    """

    def __init__(
        self, node: Iir, identifiers: List[str], modeViewName: ModeViewSymbol, documentation: str = None
    ) -> None:
        """
        Initializes a composite mode view element.

        :param node:          The IIR node this object was translated from.
        :param identifiers:   A list of identifiers.
        :param modeViewName:  Reference to the mode view applied to this element.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifiers, modeViewName, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, elementNode: Iir, furtherIdentifiers: Iterable[str] = None) -> "CompositeModeViewElement":
        name = GetNameOfNode(elementNode)
        identifiers = [name]
        if furtherIdentifiers is not None:
            identifiers.extend(furtherIdentifiers)

        modeViewNameNode = nodes.Get_Mode_View_Name(elementNode)
        modeViewName = ModeViewSymbol(modeViewNameNode, GetName(modeViewNameNode))
        documentation = GetDocumentationOfNode(elementNode)

        return cls(elementNode, identifiers, modeViewName, documentation)


@export
class ModeViewDeclaration(VHDLModel_ModeViewDeclaration, DOMMixin):
    """
    .. admonition:: Example

       .. code-block:: VHDL

          view MyView of RecordType is
            a : out;
            b : in;
          end view;
    """

    def __init__(
        self,
        node: Iir,
        identifier: str,
        subtype: Symbol,
        elements: List = None,
        documentation: str = None,
    ) -> None:
        """
        Initializes a mode view declaration (VHDL-2019).

        :param node:          The IIR node this object was translated from.
        :param identifier:    The mode view's identifier.
        :param subtype:       Reference to the subtype this mode view applies to.
        :param elements:      List of all mode view elements, in declaration order.
        :param documentation: The documentation comment associated with this declaration.
        """
        super().__init__(identifier, subtype, elements, documentation)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, modeViewNode: Iir) -> "ModeViewDeclaration":
        from pyGHDL.dom._Translate import GetModeViewElementsFromChainedNodes

        name = GetNameOfNode(modeViewNode)
        documentation = GetDocumentationOfNode(modeViewNode)
        subtypeIndication = GetSubtypeIndicationFromNode(modeViewNode, "mode view", name)
        elements = GetModeViewElementsFromChainedNodes(nodes.Get_Elements_Definition_Chain(modeViewNode))

        return cls(modeViewNode, name, subtypeIndication, elements, documentation)
