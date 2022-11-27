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

from pyTooling.Decorators import export

from pyGHDL.dom.Range import Range
from pyVHDLModel.SyntaxModel import (
    GenericAssociationItem as VHDLModel_GenericAssociationItem,
    PortAssociationItem as VHDLModel_PortAssociationItem,
    ParameterAssociationItem as VHDLModel_ParameterAssociationItem,
    ComponentInstantiation as VHDLModel_ComponentInstantiation,
    EntityInstantiation as VHDLModel_EntityInstantiation,
    ConfigurationInstantiation as VHDLModel_ConfigurationInstantiation,
    ConcurrentBlockStatement as VHDLModel_ConcurrentBlockStatement,
    ProcessStatement as VHDLModel_ProcessStatement,
    IfGenerateBranch as VHDLModel_IfGenerateBranch,
    ElsifGenerateBranch as VHDLModel_ElsifGenerateBranch,
    ElseGenerateBranch as VHDLModel_ElseGenerateBranch,
    IfGenerateStatement as VHDLModel_IfGenerateStatement,
    IndexedGenerateChoice as VHDLModel_IndexedGenerateChoice,
    RangedGenerateChoice as VHDLModel_RangedGenerateChoice,
    OthersGenerateCase as VHDLModel_OthersGenerateCase,
    GenerateCase as VHDLModel_GenerateCase,
    CaseGenerateStatement as VHDLModel_CaseGenerateStatement,
    ForGenerateStatement as VHDLModel_ForGenerateStatement,
    WaveformElement as VHDLModel_WaveformElement,
    ConcurrentSimpleSignalAssignment as VHDLModel_ConcurrentSimpleSignalAssignment,
    ConcurrentProcedureCall as VHDLModel_ConcurrentProcedureCall,
    ConcurrentAssertStatement as VHDLModel_ConcurrentAssertStatement,
    Name,
    ConcurrentStatement,
    SequentialStatement,
    ExpressionUnion,
    ConcurrentChoice,
    ConcurrentCase,
    AssociationItem,
)

from pyGHDL.libghdl import Iir, utils
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom._Utils import GetNameOfNode


@export
class GenericAssociationItem(VHDLModel_GenericAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, actual: ExpressionUnion, formal: Name = None):
        super().__init__(actual, formal)
        DOMMixin.__init__(self, associationNode)


@export
class PortAssociationItem(VHDLModel_PortAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, actual: ExpressionUnion, formal: Name = None):
        super().__init__(actual, formal)
        DOMMixin.__init__(self, associationNode)


@export
class ParameterAssociationItem(VHDLModel_ParameterAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, actual: ExpressionUnion, formal: Name = None):
        super().__init__(actual, formal)
        DOMMixin.__init__(self, associationNode)


@export
class ComponentInstantiation(VHDLModel_ComponentInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        componentName: Name,
        genericAssociations: Iterable[AssociationItem] = None,
        portAssociations: Iterable[AssociationItem] = None,
    ):
        super().__init__(label, componentName, genericAssociations, portAssociations)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ComponentInstantiation":
        from pyGHDL.dom._Translate import (
            GetNameFromNode,
            GetGenericMapAspect,
            GetPortMapAspect,
        )

        componentName = GetNameFromNode(instantiatedUnit)
        genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociations = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(
            instantiationNode,
            label,
            componentName,
            genericAssociations,
            portAssociations,
        )


@export
class EntityInstantiation(VHDLModel_EntityInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        entityName: Name,
        architectureName: Name = None,
        genericAssociations: Iterable[AssociationItem] = None,
        portAssociations: Iterable[AssociationItem] = None,
    ):
        super().__init__(label, entityName, architectureName, genericAssociations, portAssociations)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "EntityInstantiation":
        from pyGHDL.dom._Translate import (
            GetNameFromNode,
            GetGenericMapAspect,
            GetPortMapAspect,
        )

        entityId = nodes.Get_Entity_Name(instantiatedUnit)
        entityName = GetNameFromNode(entityId)

        architectureName = None
        architectureId = nodes.Get_Architecture(instantiatedUnit)
        if architectureId != nodes.Null_Iir:
            architectureName = GetNameOfNode(architectureId)

        genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociations = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(
            instantiationNode,
            label,
            entityName,
            architectureName,
            genericAssociations,
            portAssociations,
        )


@export
class ConfigurationInstantiation(VHDLModel_ConfigurationInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        configurationName: Name,
        genericAssociations: Iterable[AssociationItem] = None,
        portAssociations: Iterable[AssociationItem] = None,
    ):
        super().__init__(label, configurationName, genericAssociations, portAssociations)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ConfigurationInstantiation":
        from pyGHDL.dom._Translate import (
            GetNameFromNode,
            GetGenericMapAspect,
            GetPortMapAspect,
        )

        configurationId = nodes.Get_Configuration_Name(instantiatedUnit)
        configurationName = GetNameFromNode(configurationId)

        genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociations = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(
            instantiationNode,
            label,
            configurationName,
            genericAssociations,
            portAssociations,
        )


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
            GetConcurrentStatementsFromChainedNodes,
        )

        #        genericAssociations = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        #        portAssociations = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(blockNode), "block", label)
        statements = GetConcurrentStatementsFromChainedNodes(
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
    def parse(cls, processNode: Iir, label: str, hasSensitivityList: bool) -> "ProcessStatement":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetSequentialStatementsFromChainedNodes,
        )

        sensitivityList = None
        if hasSensitivityList:
            sensitivityList = []
            for item in utils.list_iter(nodes.Get_Sensitivity_List(processNode)):
                sensitivityList.append(GetNameOfNode(item))

        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(processNode), "process", label)
        statements = GetSequentialStatementsFromChainedNodes(
            nodes.Get_Sequential_Statement_Chain(processNode), "process", label
        )

        return cls(processNode, label, declaredItems, statements, sensitivityList)


@export
class IfGenerateBranch(VHDLModel_IfGenerateBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
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
            GetConcurrentStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(nodes.Get_Condition(generateNode))
        body = nodes.Get_Generate_Statement_Body(generateNode)

        # TODO: alternative label
        # alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "if-generate branch", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "if-generate branch", alternativeLabel)

        return cls(generateNode, condition, declaredItems, statements, alternativeLabel)


@export
class ElsifGenerateBranch(VHDLModel_ElsifGenerateBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(condition, declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir, condition: Iir) -> "ElsifGenerateBranch":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(condition)
        body = nodes.Get_Generate_Statement_Body(generateNode)

        # TODO: alternative label
        # alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "elsif-generate branch", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "elsif-generate branch", alternativeLabel)

        return cls(generateNode, condition, declaredItems, statements, alternativeLabel)


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
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Generate_Statement_Body(generateNode)

        # TODO: alternative label
        # alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "else-generate branch", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "else-generate branch", alternativeLabel)

        return cls(generateNode, declaredItems, statements, alternativeLabel)


@export
class IfGenerateStatement(VHDLModel_IfGenerateStatement, DOMMixin):
    def __init__(
        self,
        generateNode: Iir,
        label: str,
        ifBranch: IfGenerateBranch,
        elsifBranches: Iterable[ElsifGenerateBranch] = None,
        elseBranch: ElseGenerateBranch = None,
    ):
        super().__init__(label, ifBranch, elsifBranches, elseBranch)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "IfGenerateStatement":
        ifBranch = IfGenerateBranch.parse(generateNode)
        elsifBranches = []
        elseBranch = None
        # WORKAROUND: Python 3.8 syntax
        # elseClause = generateNode
        # while (elseClause := nodes.Get_Generate_Else_Clause(elseClause)) != nodes.Null_Iir:
        elseClause = nodes.Get_Generate_Else_Clause(generateNode)
        while elseClause != nodes.Null_Iir:
            condition = nodes.Get_Condition(elseClause)
            if condition != nodes.Null_Iir:
                elsifBranches.append(ElsifGenerateBranch.parse(elseClause, condition))
            else:
                elseBranch = ElseGenerateBranch.parse(elseClause)
                break

            elseClause = nodes.Get_Generate_Else_Clause(elseClause)

        return cls(generateNode, label, ifBranch, elsifBranches, elseBranch)


@export
class IndexedGenerateChoice(VHDLModel_IndexedGenerateChoice, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion):
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class RangedGenerateChoice(VHDLModel_RangedGenerateChoice, DOMMixin):
    def __init__(self, node: Iir, rng: Range):
        super().__init__(rng)
        DOMMixin.__init__(self, node)


@export
class GenerateCase(VHDLModel_GenerateCase, DOMMixin):
    def __init__(
        self,
        node: Iir,
        choices: Iterable[ConcurrentChoice],
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(choices, declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, caseNode: Iir, choices: Iterable[ConcurrentChoice]) -> "GenerateCase":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Associated_Block(caseNode)

        # TODO: alternative label
        # alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "generate case", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "generate case", alternativeLabel)

        return cls(caseNode, choices, declaredItems, statements, alternativeLabel)


@export
class OthersGenerateCase(VHDLModel_OthersGenerateCase, DOMMixin):
    def __init__(
        self,
        caseNode: Iir,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ):
        super().__init__(declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, caseNode)

    @classmethod
    def parse(cls, caseNode: Iir) -> "OthersGenerateCase":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Associated_Block(caseNode)

        # TODO: alternative label
        # alternativeLabelId = nodes.Get_Alternative_Label(body)
        alternativeLabel = ""

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "case-generate others", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "case-generate others", alternativeLabel)

        return cls(caseNode, declaredItems, statements, alternativeLabel)


@export
class CaseGenerateStatement(VHDLModel_CaseGenerateStatement, DOMMixin):
    def __init__(
        self,
        generateNode: Iir,
        label: str,
        expression: ExpressionUnion,
        cases: Iterable[ConcurrentCase],
    ):
        super().__init__(label, expression, cases)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "CaseGenerateStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetNameFromNode,
        )

        expression = GetExpressionFromNode(nodes.Get_Expression(generateNode))

        cases = []
        choices = None
        alternative = nodes.Get_Case_Statement_Alternative_Chain(generateNode)
        caseNode = alternative

        while alternative != nodes.Null_Iir:
            choiceKind = GetIirKindOfNode(alternative)
            sameAlternative = nodes.Get_Same_Alternative_Flag(alternative)

            if choiceKind in (
                nodes.Iir_Kind.Choice_By_Name,
                nodes.Iir_Kind.Choice_By_Expression,
            ):
                choiceExpression = GetExpressionFromNode(nodes.Get_Choice_Expression(alternative))

                choice = IndexedGenerateChoice(alternative, choiceExpression)
                if sameAlternative:
                    choices.append(choice)
                    alternative = nodes.Get_Chain(alternative)
                    continue
            elif choiceKind is nodes.Iir_Kind.Choice_By_Range:
                choiceRange = nodes.Get_Choice_Range(alternative)
                choiceRangeKind = GetIirKindOfNode(choiceRange)
                if choiceRangeKind == nodes.Iir_Kind.Range_Expression:
                    rng = GetRangeFromNode(choiceRange)
                elif choiceRangeKind in (
                    nodes.Iir_Kind.Attribute_Name,
                    nodes.Iir_Kind.Parenthesis_Name,
                ):
                    rng = GetNameFromNode(choiceRange)
                else:
                    pos = Position.parse(alternative)
                    raise DOMException(
                        f"Unknown choice range kind '{choiceRangeKind.name}' in case...generate statement at line {pos.Line}."
                    )

                choice = RangedGenerateChoice(alternative, rng)
                if sameAlternative:
                    choices.append(choice)
                    alternative = nodes.Get_Chain(alternative)
                    continue
            elif choiceKind is nodes.Iir_Kind.Choice_By_Others:
                if choices is not None:
                    cases.append(GenerateCase.parse(alternative, choices))
                    choices = None
                cases.append(OthersGenerateCase.parse(alternative))
                alternative = nodes.Get_Chain(alternative)
                caseNode = alternative
                continue
            else:
                pos = Position.parse(alternative)
                raise DOMException(
                    f"Unknown choice kind '{choiceKind.name}' in case...generate statement at line {pos.Line}."
                )

            if choices is not None:
                cases.append(GenerateCase.parse(caseNode, choices))

            caseNode = alternative
            choices = [
                choice,
            ]

            alternative = nodes.Get_Chain(alternative)

        if choices is not None:
            cases.append(GenerateCase.parse(caseNode, choices))

        return cls(generateNode, label, expression, cases)


@export
class ForGenerateStatement(VHDLModel_ForGenerateStatement, DOMMixin):
    def __init__(
        self,
        generateNode: Iir,
        label: str,
        loopIndex: str,
        rng: Range,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
    ):
        super().__init__(label, loopIndex, rng, declaredItems, statements)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "ForGenerateStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
            GetRangeFromNode,
            GetNameFromNode,
        )

        spec = nodes.Get_Parameter_Specification(generateNode)
        loopIndex = GetNameOfNode(spec)

        discreteRange = nodes.Get_Discrete_Range(spec)
        rangeKind = GetIirKindOfNode(discreteRange)
        if rangeKind == nodes.Iir_Kind.Range_Expression:
            rng = GetRangeFromNode(discreteRange)
        elif rangeKind in (
            nodes.Iir_Kind.Attribute_Name,
            nodes.Iir_Kind.Parenthesis_Name,
        ):
            rng = GetNameFromNode(discreteRange)
        else:
            pos = Position.parse(generateNode)
            raise DOMException(
                f"Unknown discete range kind '{rangeKind.name}' in for...generate statement at line {pos.Line}."
            )

        body = nodes.Get_Generate_Statement_Body(generateNode)
        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "for-generate", label)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "for-generate", label)

        return cls(generateNode, label, loopIndex, rng, declaredItems, statements)


@export
class WaveformElement(VHDLModel_WaveformElement, DOMMixin):
    def __init__(self, waveNode: Iir, expression: ExpressionUnion, after: ExpressionUnion):
        super().__init__(expression, after)
        DOMMixin.__init__(self, waveNode)

    @classmethod
    def parse(cls, waveNode: Iir):
        from pyGHDL.dom._Translate import GetExpressionFromNode

        value = GetExpressionFromNode(nodes.Get_We_Value(waveNode))

        timeNode = nodes.Get_Time(waveNode)
        if timeNode is nodes.Null_Iir:
            time = None
        else:
            time = GetExpressionFromNode(timeNode)

        return cls(waveNode, value, time)


@export
class ConcurrentSimpleSignalAssignment(VHDLModel_ConcurrentSimpleSignalAssignment, DOMMixin):
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
    def parse(cls, assignmentNode: Iir, label: str) -> "ConcurrentSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetNameFromNode

        target = nodes.Get_Target(assignmentNode)
        targetName = GetNameFromNode(target)

        waveform = []
        for wave in utils.chain_iter(nodes.Get_Waveform_Chain(assignmentNode)):
            waveform.append(WaveformElement.parse(wave))

        return cls(assignmentNode, label, targetName, waveform)


@export
class ConcurrentProcedureCall(VHDLModel_ConcurrentProcedureCall, DOMMixin):
    def __init__(
        self,
        callNode: Iir,
        label: str,
        procedureName: Name,
        parameterMappings: Iterable,
    ):
        super().__init__(label, procedureName, parameterMappings)
        DOMMixin.__init__(self, callNode)

    @classmethod
    def parse(cls, concurrentCallNode: Iir, label: str) -> "ConcurrentProcedureCall":
        from pyGHDL.dom._Translate import GetNameFromNode, GetParameterMapAspect

        callNode = nodes.Get_Procedure_Call(concurrentCallNode)

        prefix = nodes.Get_Prefix(callNode)
        procedureName = GetNameFromNode(prefix)
        parameterAssociations = GetParameterMapAspect(nodes.Get_Parameter_Association_Chain(callNode))

        return cls(concurrentCallNode, label, procedureName, parameterAssociations)


@export
class ConcurrentAssertStatement(VHDLModel_ConcurrentAssertStatement, DOMMixin):
    def __init__(
        self,
        assertNode: Iir,
        condition: ExpressionUnion,
        message: ExpressionUnion = None,
        severity: ExpressionUnion = None,
        label: str = None,
    ):
        super().__init__(condition, message, severity, label)
        DOMMixin.__init__(self, assertNode)

    @classmethod
    def parse(cls, assertNode: Iir, label: str) -> "ConcurrentAssertStatement":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        # FIXME: how to get the condition?
        # assertNode is a Psl_Assert_Directive
        condition = None  # GetExpressionFromNode(nodes.Get_Assertion_Condition(assertNode))
        messageNode = nodes.Get_Report_Expression(assertNode)
        message = None if messageNode is nodes.Null_Iir else GetExpressionFromNode(messageNode)
        severityNode = nodes.Get_Severity_Expression(assertNode)
        severity = None if severityNode is nodes.Null_Iir else GetExpressionFromNode(severityNode)

        return cls(assertNode, condition, message, severity, label)
