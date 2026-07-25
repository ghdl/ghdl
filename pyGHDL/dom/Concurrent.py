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
from typing import Iterable, List, Optional as Nullable

from pyTooling.Decorators import export

from pyVHDLModel.Base import ExpressionUnion, WaveformElement as VHDLModel_WaveformElement, ModelEntity
from pyVHDLModel.Common import (
    ConditionalWaveform as VHDLModel_ConditionalWaveform,
    SelectedWaveform as VHDLModel_SelectedWaveform,
    OthersSelectedWaveform as VHDLModel_OthersSelectedWaveform,
)
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Association import (
    AssociationItem,
    GenericAssociationItem as VHDLModel_GenericAssociationItem,
    PortAssociationItem as VHDLModel_PortAssociationItem,
    ParameterAssociationItem as VHDLModel_ParameterAssociationItem,
)
from pyVHDLModel.Sequential import SequentialStatement
from pyVHDLModel.Concurrent import (
    ComponentInstantiation as VHDLModel_ComponentInstantiation,
    EntityInstantiation as VHDLModel_EntityInstantiation,
    ConfigurationInstantiation as VHDLModel_ConfigurationInstantiation,
    ProcessStatement as VHDLModel_ProcessStatement,
    ConcurrentProcedureCall as VHDLModel_ConcurrentProcedureCall,
    ConcurrentBlockStatement as VHDLModel_ConcurrentBlockStatement,
    IfGenerateBranch as VHDLModel_IfGenerateBranch,
    ElsifGenerateBranch as VHDLModel_ElsifGenerateBranch,
    ElseGenerateBranch as VHDLModel_ElseGenerateBranch,
    IfGenerateStatement as VHDLModel_IfGenerateStatement,
    ConcurrentChoice,
    ConcurrentCase,
    CaseGenerateStatement as VHDLModel_CaseGenerateStatement,
    ForGenerateStatement as VHDLModel_ForGenerateStatement,
    ConcurrentSimpleSignalAssignment as VHDLModel_ConcurrentSimpleSignalAssignment,
    ConcurrentConditionalSignalAssignment as VHDLModel_ConcurrentConditionalSignalAssignment,
    ConcurrentSelectedSignalAssignment as VHDLModel_ConcurrentSelectedSignalAssignment,
    ConcurrentAssertStatement as VHDLModel_ConcurrentAssertStatement,
    ConcurrentStatement,
    GenerateCase as VHDLModel_GenerateCase,
    OthersGenerateCase as VHDLModel_OthersGenerateCase,
    IndexedGenerateChoice as VHDLModel_IndexedGenerateChoice,
    RangedGenerateChoice as VHDLModel_RangedGenerateChoice,
)

from pyGHDL.libghdl import Iir, utils, name_table
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, DOMException, Position
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Symbol import (
    ArchitectureSymbol,
    EntityInstantiationSymbol,
    ComponentInstantiationSymbol,
    ConfigurationInstantiationSymbol,
    SignalSymbol,
)


def GetAlternativeLabel(node: Iir) -> Nullable[str]:
    """
    Reads the optional alternative label of a generate-statement branch (``Get_Alternative_Label``
    on a ``Generate_Statement_Body``) or a case-generate alternative (on the node returned by
    ``Get_Associated_Block``), returning ``None`` if no label was given in the source rather than
    an empty string.
    """
    alternativeLabelId = nodes.Get_Alternative_Label(node)
    return None if alternativeLabelId == name_table.Null_Identifier else name_table.Get_Name_Ptr(alternativeLabelId)


@export
class GenericAssociationItem(VHDLModel_GenericAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, formal: Symbol, actual: ExpressionUnion) -> None:
        super().__init__(formal, actual)
        DOMMixin.__init__(self, associationNode)


@export
class PortAssociationItem(VHDLModel_PortAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, formal: Symbol, actual: ExpressionUnion) -> None:
        super().__init__(formal, actual)
        DOMMixin.__init__(self, associationNode)


@export
class ParameterAssociationItem(VHDLModel_ParameterAssociationItem, DOMMixin):
    def __init__(self, associationNode: Iir, formal: Symbol, actual: ExpressionUnion) -> None:
        super().__init__(formal, actual)
        DOMMixin.__init__(self, associationNode)


@export
class ComponentInstantiation(VHDLModel_ComponentInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        componentSymbol: ComponentInstantiationSymbol,
        genericAssociationItems: Iterable[AssociationItem] = None,
        portAssociationItems: Iterable[AssociationItem] = None,
    ) -> None:
        super().__init__(label, componentSymbol, genericAssociationItems, portAssociationItems)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ComponentInstantiation":
        from pyGHDL.dom._Translate import GetName, GetGenericMapAspect, GetPortMapAspect

        componentSymbol = ComponentInstantiationSymbol(instantiatedUnit, GetName(instantiatedUnit))
        genericAssociationItems = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociationItems = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(instantiationNode, label, componentSymbol, genericAssociationItems, portAssociationItems)


@export
class EntityInstantiation(VHDLModel_EntityInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        entitySymbol: EntityInstantiationSymbol,
        architectureSymbol: ArchitectureSymbol = None,  # TODO: merge both symbols ?
        genericAssociationItems: Iterable[AssociationItem] = None,
        portAssociationItems: Iterable[AssociationItem] = None,
    ) -> None:
        super().__init__(label, entitySymbol, architectureSymbol, genericAssociationItems, portAssociationItems)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "EntityInstantiation":
        from pyGHDL.dom._Translate import GetName, GetGenericMapAspect, GetPortMapAspect

        entityName = nodes.Get_Entity_Name(instantiatedUnit)
        entitySymbol = EntityInstantiationSymbol(entityName, GetName(entityName))

        architectureSymbol = None
        architectureId = nodes.Get_Architecture(instantiatedUnit)
        if architectureId != nodes.Null_Iir:
            architectureSymbol = ArchitectureSymbol(GetName(architectureId), entitySymbol)

        genericAssociationItems = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociationItems = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(
            instantiationNode, label, entitySymbol, architectureSymbol, genericAssociationItems, portAssociationItems
        )


@export
class ConfigurationInstantiation(VHDLModel_ConfigurationInstantiation, DOMMixin):
    def __init__(
        self,
        instantiationNode: Iir,
        label: str,
        configurationSymbol: ConfigurationInstantiationSymbol,
        genericAssociationItems: Iterable[AssociationItem] = None,
        portAssociationItems: Iterable[AssociationItem] = None,
    ) -> None:
        super().__init__(label, configurationSymbol, genericAssociationItems, portAssociationItems)
        DOMMixin.__init__(self, instantiationNode)

    @classmethod
    def parse(cls, instantiationNode: Iir, instantiatedUnit: Iir, label: str) -> "ConfigurationInstantiation":
        from pyGHDL.dom._Translate import GetName, GetGenericMapAspect, GetPortMapAspect

        configurationName = nodes.Get_Configuration_Name(instantiatedUnit)
        configurationSymbol = ConfigurationInstantiationSymbol(configurationName, GetName(configurationName))

        genericAssociationItems = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        portAssociationItems = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

        return cls(instantiationNode, label, configurationSymbol, genericAssociationItems, portAssociationItems)


@export
class ConcurrentBlockStatement(VHDLModel_ConcurrentBlockStatement, DOMMixin):
    def __init__(
        self,
        blockNode: Iir,
        label: str,
        declaredItems: Iterable = None,
        statements: Iterable["ConcurrentStatement"] = None,
    ) -> None:
        super().__init__(label, None, declaredItems, statements)
        DOMMixin.__init__(self, blockNode)

    @classmethod
    def parse(cls, blockNode: Iir, label: str) -> "ConcurrentBlockStatement":
        from pyGHDL.dom._Translate import GetDeclaredItemsFromChainedNodes, GetConcurrentStatementsFromChainedNodes

        #        genericAssociationItems = GetGenericMapAspect(nodes.Get_Generic_Map_Aspect_Chain(instantiationNode))
        #        portAssociationItems = GetPortMapAspect(nodes.Get_Port_Map_Aspect_Chain(instantiationNode))

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
        sensitivityList: Iterable[Symbol] = None,
    ) -> None:
        super().__init__(label, declaredItems, statements, sensitivityList)
        DOMMixin.__init__(self, processNode)

    @classmethod
    def parse(cls, processNode: Iir, label: str, hasSensitivityList: bool) -> "ProcessStatement":
        from pyGHDL.dom._Translate import (
            GetName,
            GetDeclaredItemsFromChainedNodes,
            GetSequentialStatementsFromChainedNodes,
        )

        sensitivityList = None
        if hasSensitivityList:
            sensitivityList = []
            for item in utils.list_iter(nodes.Get_Sensitivity_List(processNode)):
                sensitivityList.append(GetName(item))

        declaredItems = GetDeclaredItemsFromChainedNodes(nodes.Get_Declaration_Chain(processNode), "process", label)
        statements = GetSequentialStatementsFromChainedNodes(
            nodes.Get_Sequential_Statement_Chain(processNode), "process", label
        )

        return cls(processNode, label, declaredItems, statements, sensitivityList)


@export
class IfGenerateBranch(VHDLModel_IfGenerateBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ) -> None:
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

        alternativeLabel = GetAlternativeLabel(body)

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "if-generate branch", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "if-generate branch", alternativeLabel)

        return cls(generateNode, condition, declaredItems, statements, alternativeLabel)


@export
class ElsifGenerateBranch(VHDLModel_ElsifGenerateBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ) -> None:
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

        alternativeLabel = GetAlternativeLabel(body)

        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "elsif-generate branch", alternativeLabel)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "elsif-generate branch", alternativeLabel)

        return cls(generateNode, condition, declaredItems, statements, alternativeLabel)


@export
class ElseGenerateBranch(VHDLModel_ElseGenerateBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        declaredItems: Iterable = None,
        statements: Iterable[ConcurrentStatement] = None,
        alternativeLabel: str = None,
    ) -> None:
        super().__init__(declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir) -> "ElseGenerateBranch":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Generate_Statement_Body(generateNode)

        alternativeLabel = GetAlternativeLabel(body)

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
    ) -> None:
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
    def __init__(self, node: Iir, expression: ExpressionUnion) -> None:
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class RangedGenerateChoice(VHDLModel_RangedGenerateChoice, DOMMixin):
    def __init__(self, node: Iir, rng: Range) -> None:
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
    ) -> None:
        super().__init__(choices, declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, caseNode: Iir, choices: Iterable[ConcurrentChoice]) -> "GenerateCase":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Associated_Block(caseNode)

        alternativeLabel = GetAlternativeLabel(body)

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
    ) -> None:
        super().__init__(declaredItems, statements, alternativeLabel)
        DOMMixin.__init__(self, caseNode)

    @classmethod
    def parse(cls, caseNode: Iir) -> "OthersGenerateCase":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
        )

        body = nodes.Get_Associated_Block(caseNode)

        alternativeLabel = GetAlternativeLabel(body)

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
    ) -> None:
        super().__init__(label, expression, cases)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "CaseGenerateStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetName,
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
                    rng = GetName(choiceRange)
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
                    cases.append(GenerateCase.parse(caseNode, choices))
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
            choices = [choice]

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
        parent: Nullable[ModelEntity] = None,
    ) -> None:
        super().__init__(label, loopIndex, rng, declaredItems, statements, parent=parent)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "ForGenerateStatement":
        from pyGHDL.dom._Utils import GetNameOfNode
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetConcurrentStatementsFromChainedNodes,
            GetDiscreteRangeFromNode,
        )

        spec = nodes.Get_Parameter_Specification(generateNode)
        loopIndex = GetNameOfNode(spec)

        rng = GetDiscreteRangeFromNode(nodes.Get_Discrete_Range(spec), "for...generate statement")

        body = nodes.Get_Generate_Statement_Body(generateNode)
        declarationChain = nodes.Get_Declaration_Chain(body)
        declaredItems = GetDeclaredItemsFromChainedNodes(declarationChain, "for-generate", label)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetConcurrentStatementsFromChainedNodes(statementChain, "for-generate", label)

        return cls(generateNode, label, loopIndex, rng, declaredItems, statements)


@export
class WaveformElement(VHDLModel_WaveformElement, DOMMixin):
    def __init__(self, waveNode: Iir, expression: ExpressionUnion, after: ExpressionUnion) -> None:
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


def GetWaveformElementsFromChainedNodes(nodeChain: Iir) -> List[WaveformElement]:
    """Translates a chain of ``Waveform_Element`` nodes (used at multiple call sites: simple/
    conditional/selected signal assignments, both concurrent and sequential) into a list of
    :class:`WaveformElement`."""
    return [WaveformElement.parse(wave) for wave in utils.chain_iter(nodeChain)]


def GetConditionalWaveformsFromChainedNodes(nodeChain: Iir) -> Iterable["ConditionalWaveform"]:
    """Translates a chain of ``Conditional_Waveform`` nodes (shared by concurrent and sequential
    conditional signal assignments) into a sequence of :class:`ConditionalWaveform`."""
    return [ConditionalWaveform.parse(node) for node in utils.chain_iter(nodeChain)]


def GetSelectedWaveformsFromChainedNodes(nodeChain: Iir) -> Iterable:
    """
    Translates a chain of choices (shared by concurrent and sequential selected signal assignments)
    into a sequence of :class:`SelectedWaveform`/:class:`OthersSelectedWaveform`.

    Mirrors the grouping algorithm already used for case-generate alternatives
    (``Get_Same_Alternative_Flag`` groups e.g. ``when 0 | 1 =>`` into one alternative): the *first*
    choice in a group owns the real content (``Get_Associated_Chain``, ``Same_Alternative_Flag=False``);
    later choices in the same group (``Same_Alternative_Flag=True``) have a null associated chain and
    are just additional choice values for that same, already-established alternative.
    """
    from pyGHDL.dom._Utils import GetIirKindOfNode
    from pyGHDL.dom._Translate import GetExpressionFromNode, GetRangeFromNode
    from pyGHDL.dom.Sequential import IndexedChoice, RangedChoice

    alternatives = []
    choices = None
    ownerNode = None
    choice = nodeChain
    while choice != nodes.Null_Iir:
        kind = GetIirKindOfNode(choice)
        sameAlternative = nodes.Get_Same_Alternative_Flag(choice)

        if kind == nodes.Iir_Kind.Choice_By_Expression:
            choiceValue = IndexedChoice(choice, GetExpressionFromNode(nodes.Get_Choice_Expression(choice)))
            if sameAlternative:
                choices.append(choiceValue)
                choice = nodes.Get_Chain(choice)
                continue
        elif kind == nodes.Iir_Kind.Choice_By_Range:
            choiceValue = RangedChoice(choice, GetRangeFromNode(nodes.Get_Choice_Range(choice)))
            if sameAlternative:
                choices.append(choiceValue)
                choice = nodes.Get_Chain(choice)
                continue
        elif kind == nodes.Iir_Kind.Choice_By_Others:
            if choices is not None:
                waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Associated_Chain(ownerNode))
                alternatives.append(SelectedWaveform(ownerNode, choices, waveform))
                choices = None

            othersWaveform = GetWaveformElementsFromChainedNodes(nodes.Get_Associated_Chain(choice))
            alternatives.append(OthersSelectedWaveform(choice, othersWaveform))
            choice = nodes.Get_Chain(choice)
            continue
        else:
            position = Position.parse(choice)
            raise DOMException(f"Unknown choice kind '{kind.name}' in selected waveform at {position}.")

        if choices is not None:
            waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Associated_Chain(ownerNode))
            alternatives.append(SelectedWaveform(ownerNode, choices, waveform))

        ownerNode = choice
        choices = [choiceValue]
        choice = nodes.Get_Chain(choice)

    if choices is not None:
        waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Associated_Chain(ownerNode))
        alternatives.append(SelectedWaveform(ownerNode, choices, waveform))

    return alternatives


@export
class ConditionalWaveform(VHDLModel_ConditionalWaveform, DOMMixin):
    def __init__(self, node: Iir, waveform: Iterable[WaveformElement], condition: ExpressionUnion = None) -> None:
        super().__init__(waveform, condition)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "ConditionalWaveform":
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Waveform_Chain(node))
        condition = GetOptionalExpressionFromNode(nodes.Get_Condition(node))

        return cls(node, waveform, condition)


@export
class SelectedWaveform(VHDLModel_SelectedWaveform, DOMMixin):
    def __init__(self, node: Iir, choices: Iterable, waveform: Iterable[WaveformElement]) -> None:
        super().__init__(choices, waveform)
        DOMMixin.__init__(self, node)


@export
class OthersSelectedWaveform(VHDLModel_OthersSelectedWaveform, DOMMixin):
    def __init__(self, node: Iir, waveform: Iterable[WaveformElement]) -> None:
        super().__init__(waveform)
        DOMMixin.__init__(self, node)


@export
class ConcurrentSimpleSignalAssignment(VHDLModel_ConcurrentSimpleSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        label: str,
        target: SignalSymbol,
        waveform: Iterable[WaveformElement],
    ) -> None:
        super().__init__(label, target, waveform)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str) -> "ConcurrentSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))

        waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Waveform_Chain(assignmentNode))

        return cls(assignmentNode, label, targetName, waveform)


@export
class ConcurrentConditionalSignalAssignment(VHDLModel_ConcurrentConditionalSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        label: str,
        target: SignalSymbol,
        conditionalWaveforms: Iterable[ConditionalWaveform],
    ) -> None:
        super().__init__(label, target, conditionalWaveforms)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str) -> "ConcurrentConditionalSignalAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))
        conditionalWaveforms = GetConditionalWaveformsFromChainedNodes(
            nodes.Get_Conditional_Waveform_Chain(assignmentNode)
        )

        return cls(assignmentNode, label, targetName, conditionalWaveforms)


@export
class ConcurrentSelectedSignalAssignment(VHDLModel_ConcurrentSelectedSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        label: str,
        target: SignalSymbol,
        expression: ExpressionUnion,
        selectedWaveforms: Iterable,
    ) -> None:
        super().__init__(label, target, expression, selectedWaveforms)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str) -> "ConcurrentSelectedSignalAssignment":
        from pyGHDL.dom._Translate import GetName, GetExpressionFromNode

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))
        expression = GetExpressionFromNode(nodes.Get_Expression(assignmentNode))
        selectedWaveforms = GetSelectedWaveformsFromChainedNodes(nodes.Get_Selected_Waveform_Chain(assignmentNode))

        return cls(assignmentNode, label, targetName, expression, selectedWaveforms)


@export
class ConcurrentProcedureCall(VHDLModel_ConcurrentProcedureCall, DOMMixin):
    def __init__(
        self,
        callNode: Iir,
        label: str,
        procedureName: Symbol,
        parameterAssociationItems: Iterable,
    ) -> None:
        super().__init__(label, procedureName, parameterAssociationItems)
        DOMMixin.__init__(self, callNode)

    @classmethod
    def parse(cls, concurrentCallNode: Iir, label: str) -> "ConcurrentProcedureCall":
        from pyGHDL.dom._Translate import GetName, GetParameterMapAspect

        callNode = nodes.Get_Procedure_Call(concurrentCallNode)

        prefix = nodes.Get_Prefix(callNode)
        procedureName = GetName(prefix)
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
    ) -> None:
        super().__init__(condition, message, severity, label)
        DOMMixin.__init__(self, assertNode)

    @classmethod
    def parse(cls, assertNode: Iir, label: str) -> "ConcurrentAssertStatement":
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        # FIXME: how to get the condition?
        # assertNode is a Psl_Assert_Directive
        condition = None  # GetOptionalExpressionFromNode(nodes.Get_Assertion_Condition(assertNode))
        message = GetOptionalExpressionFromNode(nodes.Get_Report_Expression(assertNode))
        severity = GetOptionalExpressionFromNode(nodes.Get_Severity_Expression(assertNode))

        return cls(assertNode, condition, message, severity, label)
