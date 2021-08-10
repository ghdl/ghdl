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
# Package module:   DOM: Concurrent statements.
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
from typing import Iterable

from pydecor import export

from pyVHDLModel.SyntaxModel import (
    ComponentInstantiation as VHDLModel_ComponentInstantiation,
    EntityInstantiation as VHDLModel_EntityInstantiation,
    ConfigurationInstantiation as VHDLModel_ConfigurationInstantiation,
    ConcurrentBlockStatement as VHDLModel_ConcurrentBlockStatement,
    ProcessStatement as VHDLModel_ProcessStatement,
    IfGenerateBranch as VHDLModel_IfGenerateBranch,
    ElsifGenerateBranch as VHDLModel_ElsifGenerateBranch,
    ElseGenerateBranch as VHDLModel_ElseGenerateBranch,
    IfGenerateStatement as VHDLModel_IfGenerateStatement,
    CaseGenerateStatement as VHDLModel_CaseGenerateStatement,
    ForGenerateStatement as VHDLModel_ForGenerateStatement,
    WaveformElement as VHDLModel_WaveformElement,
    ConcurrentSimpleSignalAssignment as VHDLModel_ConcurrentSimpleSignalAssignment,
    Name,
    ConcurrentStatement,
    SequentialStatement,
    Expression,
)

from pyGHDL.libghdl import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin
from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode


@export
class ComponentInstantiation(VHDLModel_ComponentInstantiation, DOMMixin):
    def __init__(self, instantiationNode: Iir, label: str, componentName: Name):
        super().__init__(label, componentName)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(
        cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str
    ) -> "ComponentInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        componentName = GetNameFromNode(instantiatedUnit)

        # TODO: get mapped generics
        # TODO: get mapped ports

        return cls(instantiationNode, label, componentName)


@export
class EntityInstantiation(VHDLModel_EntityInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        entityName: Name,
        architectureName: Name = None,
    ):
        super().__init__(label, entityName, architectureName)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(
        cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str
    ) -> "EntityInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        entityId = nodes.Get_Entity_Name(instantiatedUnit)
        entityName = GetNameFromNode(entityId)

        architectureName = None
        architectureId = nodes.Get_Architecture(instantiatedUnit)
        if architectureId != nodes.Null_Iir:
            architectureName = GetNameOfNode(architectureId)

        # TODO: get mapped generics
        # TODO: get mapped ports

        return cls(instantiationNode, label, entityName, architectureName)


@export
class ConfigurationInstantiation(VHDLModel_ConfigurationInstantiation, DOMMixin):
    def __init__(self, instantiationNode: Iir, label: str, configurationName: Name):
        super().__init__(label, configurationName)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(
        cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str
    ) -> "ConfigurationInstantiation":
        from pyGHDL.dom._Translate import GetNameFromNode

        configurationId = nodes.Get_Configuration_Name(instantiatedUnit)
        configurationName = GetNameFromNode(configurationId)

        # TODO: get mapped generics
        # TODO: get mapped ports

        return cls(instantiationNode, label, configurationName)


@export
class ConcurrentBlockStatement(VHDLModel_ConcurrentBlockStatement, DOMMixin):
    def __init__(
        self,
        blockNode: Iir,
        label: str,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
    ):
        super().__init__(label, None, declaredItems, statements)
        DOMMixin.__init__(self, blockNode)

    @classmethod
    def parse(cls, blockNode: Iir, label: str) -> "ConcurrentBlockStatement":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
        )

        declaredItems = GetDeclaredItemsFromChainedNodes(
            nodes.Get_Declaration_Chain(blockNode), "block", label
        )
        statements = GetStatementsFromChainedNodes(
            nodes.Get_Concurrent_Statement_Chain(blockNode), "block", label
        )

        return cls(blockNode, label, declaredItems, statements)


@export
class ProcessStatement(VHDLModel_ProcessStatement, DOMMixin):
    def __init__(
        self,
        processNode: Iir,
        label: str = None,
        declaredItems: Iterable = None,
        statements: Iterable[SequentialStatement] = None,
        sensitivityList: Iterable[Name] = None,
    ):
        super().__init__(label, declaredItems, statements, sensitivityList)
        DOMMixin.__init__(self, processNode)

    @classmethod
    def parse(
        cls, processNode: Iir, label: str, hasSensitivityList: bool
    ) -> "ProcessStatement":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
        )

        # TODO: get sensitivity list
        # TODO: get declared items
        # TODO: get sequential statements

        declaredItems = None
        statements = None
        sensitivityList = None

        if hasSensitivityList:
            pass

        return cls(processNode, label, declaredItems, statements, sensitivityList)


@export
class IfGenerateBranch(VHDLModel_IfGenerateBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: Expression,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(condition, declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir) -> "IfGenerateBranch":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(nodes.Get_Condition(generateNode))
        body = nodes.Get_Generate_Statement_Body(generateNode)

        alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            declarationChain, "if-generate branch", alternativeLabel
        )

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetStatementsFromChainedNodes(
            statementChain, "if-generate branch", alternativeLabel
        )

        return cls(body, condition, declaredItems, statements, alternativeLabel)


@export
class ElsifGenerateBranch(VHDLModel_ElsifGenerateBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: Expression,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(condition, declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir) -> "ElsifGenerateBranch":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(nodes.Get_Condition(generateNode))
        body = nodes.Get_Generate_Statement_Body(generateNode)

        alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            declarationChain, "if-generate branch", alternativeLabel
        )

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetStatementsFromChainedNodes(
            statementChain, "if-generate branch", alternativeLabel
        )

        return cls(body, condition, declaredItems, statements, alternativeLabel)


@export
class ElseGenerateBranch(VHDLModel_ElseGenerateBranch):
    def __init__(
        self,
        branchNode: Iir,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir) -> "ElseGenerateBranch":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        body = nodes.Get_Generate_Statement_Body(generateNode)

        alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(
            declarationChain, "if-generate branch", alternativeLabel
        )

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetStatementsFromChainedNodes(
            statementChain, "if-generate branch", alternativeLabel
        )

        return cls(body, declaredItems, statements, alternativeLabel)


@export
class IfGenerateStatement(VHDLModel_IfGenerateStatement, DOMMixin):
    def __init__(self, generateNode: Iir, label: str, ifBranch: IfGenerateBranch):
        super().__init__(label, ifBranch)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "IfGenerateStatement":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        # TODO: get branches
        # TODO: get declared items
        # TODO: get concurrent statements

        print("if branch", generateNode, GetIirKindOfNode(generateNode))
        ifBranch = IfGenerateBranch.parse(generateNode)

        #        Python 3.8 syntax
        #        elseClause = generateNode
        #        while (elseClause := nodes.Get_Generate_Else_Clause(elseClause)) != nodes.Null_Iir:
        elseClause = nodes.Get_Generate_Else_Clause(generateNode)
        while elseClause != nodes.Null_Iir:
            print("els(if) branch", elseClause, GetIirKindOfNode(elseClause))
            ifBranch = ElsifGenerateBranch.parse(generateNode)

            elseClause = nodes.Get_Generate_Else_Clause(elseClause)

        return cls(generateNode, label, ifBranch)


@export
class CaseGenerateStatement(VHDLModel_CaseGenerateStatement, DOMMixin):
    def __init__(self, generateNode: Iir, label: str):
        super().__init__(label)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "CaseGenerateStatement":
        # TODO: get choices
        # TODO: get declared items
        # TODO: get concurrent statements

        return cls(generateNode, label)


@export
class ForGenerateStatement(VHDLModel_ForGenerateStatement, DOMMixin):
    def __init__(self, generateNode: Iir, label: str):
        super().__init__(label)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "ForGenerateStatement":
        # TODO: get index and range
        # TODO: get declared items
        # TODO: get concurrent statements

        return cls(generateNode, label)


@export
class WaveformElement(
    VHDLModel_WaveformElement, DOMMixin
):
    def __init__(
        self,
        waveformNode: Iir,
        expression: Expression,
        after: Expression
    ):
        super().__init__(expression, after)
        DOMMixin.__init__(self, waveformNode)

@export
class ConcurrentSimpleSignalAssignment(
    VHDLModel_ConcurrentSimpleSignalAssignment, DOMMixin
):
    def __init__(
        self,
        assignmentNode: Iir,
        label: str,
        target: Name,
        waveform: Iterable[WaveformElement],
    ):
        super().__init__(label, target, waveform)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(
        cls, assignmentNode: Iir, label: str
    ) -> "ConcurrentSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetNameFromNode

        target = nodes.Get_Target(assignmentNode)
        targetName = GetNameFromNode(target)

        waveform = nodes.Get_Waveform_Chain(assignmentNode)

        # TODO: translate waveforms to series of "expressions".
        expression = [None]

        return cls(assignmentNode, label, targetName, expression)
