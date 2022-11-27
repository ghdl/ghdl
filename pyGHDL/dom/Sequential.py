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
# Package module:   DOM: Sequential statements.
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

from pyGHDL.dom.Concurrent import (
    WaveformElement,
    ParameterAssociationItem,
)  # TODO: move out from concurrent?
from pyGHDL.dom.Range import Range
from pyVHDLModel.SyntaxModel import (
    IfBranch as VHDLModel_IfBranch,
    ElsifBranch as VHDLModel_ElsifBranch,
    ElseBranch as VHDLModel_ElseBranch,
    IfStatement as VHDLModel_IfStatement,
    IndexedChoice as VHDLModel_IndexedChoice,
    RangedChoice as VHDLModel_RangedChoice,
    OthersCase as VHDLModel_OthersCase,
    Case as VHDLModel_Case,
    CaseStatement as VHDLModel_CaseStatement,
    ForLoopStatement as VHDLModel_ForLoopStatement,
    SequentialSimpleSignalAssignment as VHDLModel_SequentialSimpleSignalAssignment,
    SequentialProcedureCall as VHDLModel_SequentialProcedureCall,
    SequentialAssertStatement as VHDLModel_SequentialAssertStatement,
    SequentialReportStatement as VHDLModel_SequentialReportStatement,
    NullStatement as VHDLModel_NullStatement,
    WaitStatement as VHDLModel_WaitStatement,
    Name,
    SequentialStatement,
    ExpressionUnion,
    SequentialChoice,
    SequentialCase,
)


from pyGHDL.libghdl import Iir, utils
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, Position, DOMException
from pyGHDL.dom._Utils import GetNameOfNode


@export
class IfBranch(VHDLModel_IfBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(condition, statements)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, branchNode: Iir, label: str) -> "IfBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(nodes.Get_Condition(branchNode))
        statementChain = nodes.Get_Sequential_Statement_Chain(branchNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "if branch", label)

        return cls(branchNode, condition, statements)


@export
class ElsifBranch(VHDLModel_ElsifBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(condition, statements)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, branchNode: Iir, condition: Iir, label: str) -> "ElsifBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(condition)
        statementChain = nodes.Get_Sequential_Statement_Chain(branchNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "elsif branch", label)

        return cls(branchNode, condition, statements)


@export
class ElseBranch(VHDLModel_ElseBranch):
    def __init__(
        self,
        branchNode: Iir,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(statements)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, branchNode: Iir, label: str) -> "ElseBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
        )

        statementChain = nodes.Get_Sequential_Statement_Chain(branchNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "else branch", label)

        return cls(branchNode, statements)


@export
class IfStatement(VHDLModel_IfStatement, DOMMixin):
    def __init__(
        self,
        ifNode: Iir,
        ifBranch: IfBranch,
        elsifBranches: Iterable[ElsifBranch] = None,
        elseBranch: ElseBranch = None,
        label: str = None,
    ):
        super().__init__(ifBranch, elsifBranches, elseBranch, label)
        DOMMixin.__init__(self, ifNode)

    @classmethod
    def parse(cls, ifNode: Iir, label: str) -> "IfStatement":
        ifBranch = IfBranch.parse(ifNode, label)
        elsifBranches = []
        elseBranch = None
        # WORKAROUND: Python 3.8 syntax
        # elseClause = generateNode
        # while (elseClause := nodes.Get_Generate_Else_Clause(elseClause)) != nodes.Null_Iir:
        elseClause = nodes.Get_Else_Clause(ifNode)
        while elseClause != nodes.Null_Iir:
            condition = nodes.Get_Condition(elseClause)
            if condition != nodes.Null_Iir:
                elsifBranches.append(ElsifBranch.parse(elseClause, condition, label))
            else:
                elseBranch = ElseBranch.parse(elseClause, label)
                break

            elseClause = nodes.Get_Else_Clause(elseClause)

        return cls(ifNode, ifBranch, elsifBranches, elseBranch, label)


@export
class IndexedChoice(VHDLModel_IndexedChoice, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion):
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class RangedChoice(VHDLModel_RangedChoice, DOMMixin):
    def __init__(self, node: Iir, rng: Range):
        super().__init__(rng)
        DOMMixin.__init__(self, node)


@export
class Case(VHDLModel_Case, DOMMixin):
    def __init__(
        self,
        node: Iir,
        choices: Iterable[SequentialChoice],
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(choices, statements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, caseNode: Iir, choices: Iterable[SequentialChoice], label: str) -> "Case":
        from pyGHDL.dom._Translate import GetSequentialStatementsFromChainedNodes

        statementChain = nodes.Get_Associated_Chain(caseNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "case", label)

        return cls(caseNode, choices, statements)


@export
class OthersCase(VHDLModel_OthersCase, DOMMixin):
    def __init__(
        self,
        caseNode: Iir,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(statements)
        DOMMixin.__init__(self, caseNode)

    @classmethod
    def parse(cls, caseNode: Iir, label: str = None) -> "OthersCase":
        from pyGHDL.dom._Translate import GetSequentialStatementsFromChainedNodes

        body = nodes.Get_Associated_Block(caseNode)
        if body is nodes.Null_Iir:
            return cls(caseNode)

        statementChain = nodes.Get_Concurrent_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "case others", label)

        return cls(caseNode, statements)


@export
class CaseStatement(VHDLModel_CaseStatement, DOMMixin):
    def __init__(
        self,
        caseNode: Iir,
        label: str,
        expression: ExpressionUnion,
        cases: Iterable[SequentialCase],
    ):
        super().__init__(expression, cases, label)
        DOMMixin.__init__(self, caseNode)

    @classmethod
    def parse(cls, caseNode: Iir, label: str) -> "CaseStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetNameFromNode,
        )

        expression = GetExpressionFromNode(nodes.Get_Expression(caseNode))

        cases = []
        choices = None
        alternative = nodes.Get_Case_Statement_Alternative_Chain(caseNode)
        cNode = alternative

        while alternative != nodes.Null_Iir:
            choiceKind = GetIirKindOfNode(alternative)
            sameAlternative = nodes.Get_Same_Alternative_Flag(alternative)

            if choiceKind in (
                nodes.Iir_Kind.Choice_By_Name,
                nodes.Iir_Kind.Choice_By_Expression,
            ):
                choiceExpression = GetExpressionFromNode(nodes.Get_Choice_Expression(alternative))

                choice = IndexedChoice(alternative, choiceExpression)
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
                        f"Unknown choice range kind '{choiceRangeKind.name}' in case statement at line {pos.Line}."
                    )

                choice = RangedChoice(alternative, rng)
                if sameAlternative:
                    choices.append(choice)
                    alternative = nodes.Get_Chain(alternative)
                    continue
            elif choiceKind is nodes.Iir_Kind.Choice_By_Others:
                if choices is not None:
                    cases.append(Case.parse(alternative, choices, label))
                    choices = None
                cases.append(OthersCase.parse(alternative, label))
                alternative = nodes.Get_Chain(alternative)
                cNode = alternative
                continue
            else:
                pos = Position.parse(alternative)
                raise DOMException(f"Unknown choice kind '{choiceKind.name}' in case statement at line {pos.Line}.")

            if choices is not None:
                cases.append(Case.parse(cNode, choices, label))

            cNode = alternative
            choices = [
                choice,
            ]

            alternative = nodes.Get_Chain(alternative)

        if choices is not None:
            cases.append(Case.parse(cNode, choices, label))

        return cls(caseNode, label, expression, cases)


@export
class ForLoopStatement(VHDLModel_ForLoopStatement, DOMMixin):
    def __init__(
        self,
        loopNode: Iir,
        loopIndex: str,
        rng: Range,
        statements: Iterable[SequentialStatement] = None,
        label: str = None,
    ):
        super().__init__(loopIndex, rng, statements, label)
        DOMMixin.__init__(self, loopNode)

    @classmethod
    def parse(cls, loopNode: Iir, label: str) -> "ForLoopStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetRangeFromNode,
            GetNameFromNode,
        )

        spec = nodes.Get_Parameter_Specification(loopNode)
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
            pos = Position.parse(loopNode)
            raise DOMException(
                f"Unknown discete range kind '{rangeKind.name}' in for...loop statement at line {pos.Line}."
            )

        statementChain = nodes.Get_Sequential_Statement_Chain(loopNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "for", label)

        return cls(loopNode, loopIndex, rng, statements, label)


@export
class SequentialSimpleSignalAssignment(VHDLModel_SequentialSimpleSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: Name,
        waveform: Iterable[WaveformElement],
        label: str = None,
    ):
        super().__init__(target, waveform, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetNameFromNode

        target = nodes.Get_Target(assignmentNode)
        targetName = GetNameFromNode(target)

        waveform = []
        for wave in utils.chain_iter(nodes.Get_Waveform_Chain(assignmentNode)):
            waveform.append(WaveformElement.parse(wave))

        return cls(assignmentNode, targetName, waveform, label)


@export
class SequentialProcedureCall(VHDLModel_SequentialProcedureCall, DOMMixin):
    def __init__(
        self,
        callNode: Iir,
        procedureName: Name,
        parameterMappings: Iterable[ParameterAssociationItem],
        label: str = None,
    ):
        super().__init__(procedureName, parameterMappings, label)
        DOMMixin.__init__(self, callNode)

    @classmethod
    def parse(cls, callNode: Iir, label: str) -> "SequentialProcedureCall":
        from pyGHDL.dom._Translate import GetNameFromNode, GetParameterMapAspect

        cNode = nodes.Get_Procedure_Call(callNode)

        prefix = nodes.Get_Prefix(cNode)
        procedureName = GetNameFromNode(prefix)
        parameterAssociations = GetParameterMapAspect(nodes.Get_Parameter_Association_Chain(cNode))

        return cls(callNode, procedureName, parameterAssociations, label)


@export
class SequentialAssertStatement(VHDLModel_SequentialAssertStatement, DOMMixin):
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
    def parse(cls, assertNode: Iir, label: str) -> "SequentialAssertStatement":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        condition = GetExpressionFromNode(nodes.Get_Assertion_Condition(assertNode))
        messageNode = nodes.Get_Report_Expression(assertNode)
        message = None if messageNode is nodes.Null_Iir else GetExpressionFromNode(messageNode)
        severityNode = nodes.Get_Severity_Expression(assertNode)
        severity = None if severityNode is nodes.Null_Iir else GetExpressionFromNode(severityNode)

        return cls(assertNode, condition, message, severity, label)


@export
class SequentialReportStatement(VHDLModel_SequentialReportStatement, DOMMixin):
    def __init__(
        self,
        reportNode: Iir,
        message: ExpressionUnion,
        severity: ExpressionUnion = None,
        label: str = None,
    ):
        super().__init__(message, severity, label)
        DOMMixin.__init__(self, reportNode)

    @classmethod
    def parse(cls, reportNode: Iir, label: str) -> "SequentialReportStatement":
        from pyGHDL.dom._Translate import GetExpressionFromNode

        message = GetExpressionFromNode(nodes.Get_Report_Expression(reportNode))
        severityNode = nodes.Get_Severity_Expression(reportNode)
        severity = None if severityNode is nodes.Null_Iir else GetExpressionFromNode(severityNode)

        return cls(reportNode, message, severity, label)


@export
class NullStatement(VHDLModel_NullStatement, DOMMixin):
    def __init__(
        self,
        waitNode: Iir,
        label: str = None,
    ):
        super().__init__(label)
        DOMMixin.__init__(self, waitNode)


@export
class WaitStatement(VHDLModel_WaitStatement, DOMMixin):
    def __init__(
        self,
        waitNode: Iir,
        sensitivityList: Iterable[Name] = None,
        condition: ExpressionUnion = None,
        timeout: ExpressionUnion = None,
        label: str = None,
    ):
        super().__init__(sensitivityList, condition, timeout, label)
        DOMMixin.__init__(self, waitNode)

    @classmethod
    def parse(cls, waitNode: Iir, label: str) -> "WaitStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import GetExpressionFromNode

        sensitivityList = None
        sensitivityListNode = nodes.Get_Sensitivity_List(waitNode)
        if sensitivityListNode is not nodes.Null_Iir:
            print(GetIirKindOfNode(sensitivityListNode))

        conditionNode = nodes.Get_Condition_Clause(waitNode)
        condition = None if conditionNode is nodes.Null_Iir else GetExpressionFromNode(conditionNode)

        timeoutNode = nodes.Get_Timeout_Clause(waitNode)
        timeout = None if timeoutNode is nodes.Null_Iir else GetExpressionFromNode(timeoutNode)

        return cls(waitNode, sensitivityList, condition, timeout, label)
