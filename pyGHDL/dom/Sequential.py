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
"""
This module implements derived sequential statement classes from :mod:`pyVHDLModel.Sequential`.
"""

from typing import Iterable

from pyTooling.Decorators import export

from pyVHDLModel.Base import ExpressionUnion, Range
from pyVHDLModel.Symbol import Symbol
from pyVHDLModel.Sequential import SequentialStatement, SequentialChoice, SequentialCase
from pyVHDLModel.Sequential import IfBranch as VHDLModel_IfBranch
from pyVHDLModel.Sequential import ElsifBranch as VHDLModel_ElsifBranch
from pyVHDLModel.Sequential import ElseBranch as VHDLModel_ElseBranch
from pyVHDLModel.Sequential import IndexedChoice as VHDLModel_IndexedChoice
from pyVHDLModel.Sequential import RangedChoice as VHDLModel_RangedChoice
from pyVHDLModel.Sequential import Case as VHDLModel_Case
from pyVHDLModel.Sequential import OthersCase as VHDLModel_OthersCase
from pyVHDLModel.Sequential import IfStatement as VHDLModel_IfStatement
from pyVHDLModel.Sequential import CaseStatement as VHDLModel_CaseStatement
from pyVHDLModel.Sequential import ForLoopStatement as VHDLModel_ForLoopStatement
from pyVHDLModel.Sequential import WhileLoopStatement as VHDLModel_WhileLoopStatement
from pyVHDLModel.Sequential import NullStatement as VHDLModel_NullStatement
from pyVHDLModel.Sequential import ReturnStatement as VHDLModel_ReturnStatement
from pyVHDLModel.Sequential import WaitStatement as VHDLModel_WaitStatement
from pyVHDLModel.Sequential import NextStatement as VHDLModel_NextStatement
from pyVHDLModel.Sequential import ExitStatement as VHDLModel_ExitStatement
from pyVHDLModel.Sequential import SequentialProcedureCall as VHDLModel_SequentialProcedureCall
from pyVHDLModel.Sequential import SequentialSimpleSignalAssignment as VHDLModel_SequentialSimpleSignalAssignment
from pyVHDLModel.Sequential import SequentialVariableAssignment as VHDLModel_SequentialVariableAssignment
from pyVHDLModel.Sequential import (
    SequentialConditionalVariableAssignment as VHDLModel_SequentialConditionalVariableAssignment,
)
from pyVHDLModel.Sequential import (
    SequentialConditionalSignalAssignment as VHDLModel_SequentialConditionalSignalAssignment,
)
from pyVHDLModel.Sequential import (
    SequentialSelectedVariableAssignment as VHDLModel_SequentialSelectedVariableAssignment,
)
from pyVHDLModel.Sequential import SequentialSelectedSignalAssignment as VHDLModel_SequentialSelectedSignalAssignment
from pyVHDLModel.Sequential import SignalForceAssignment as VHDLModel_SignalForceAssignment
from pyVHDLModel.Sequential import SignalReleaseAssignment as VHDLModel_SignalReleaseAssignment
from pyVHDLModel.Common import ConditionalExpression as VHDLModel_ConditionalExpression
from pyVHDLModel.Common import SelectedExpression as VHDLModel_SelectedExpression
from pyVHDLModel.Common import OthersSelectedExpression as VHDLModel_OthersSelectedExpression
from pyVHDLModel.Sequential import SequentialReportStatement as VHDLModel_SequentialReportStatement
from pyVHDLModel.Sequential import SequentialAssertStatement as VHDLModel_SequentialAssertStatement

from pyGHDL.libghdl import Iir, utils
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import DOMMixin, Position, DOMException
from pyGHDL.dom.Concurrent import WaveformElement, ParameterAssociationItem  # TODO: move out from concurrent?
from pyGHDL.dom.Concurrent import GetWaveformElementsFromChainedNodes
from pyGHDL.dom.Symbol import SignalSymbol, VariableSymbol
from pyGHDL.dom.Concurrent import GetConditionalWaveformsFromChainedNodes, GetSelectedWaveformsFromChainedNodes


@export
class IfBranch(VHDLModel_IfBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        statements: Iterable[SequentialStatement] = None,
    ) -> None:
        """
        Initializes an if branch.

        :param branchNode: The IIR node of the branch this object represents.
        :param condition:  The condition guarding this statement.
        :param statements: List of all sequential statements in this construct.
        """
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
class ElsifBranch(VHDLModel_ElsifBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        condition: ExpressionUnion,
        statements: Iterable[SequentialStatement] = None,
    ) -> None:
        """
        Initializes an ``elsif`` branch of an if statement.

        :param branchNode: The IIR node of the branch this object represents.
        :param condition:  The condition guarding this statement.
        :param statements: List of all sequential statements in this construct.
        """
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
class ElseBranch(VHDLModel_ElseBranch, DOMMixin):
    def __init__(
        self,
        branchNode: Iir,
        statements: Iterable[SequentialStatement] = None,
    ) -> None:
        """
        Initializes an else branch.

        :param branchNode: The IIR node of the branch this object represents.
        :param statements: List of all sequential statements in this construct.
        """
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
    ) -> None:
        """
        Initializes an if statement.

        :param ifNode:        The IIR node of the if statement.
        :param ifBranch:      The mandatory ``if`` branch.
        :param elsifBranches: List of all ``elsif`` branches, in the order they were written.
        :param elseBranch:    The optional ``else`` branch, or ``None`` if none was given.
        :param label:         The label of a model entity.
        """
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
    def __init__(self, node: Iir, expression: ExpressionUnion) -> None:
        """
        Initializes a case choice given by a single value.

        :param node:       The IIR node this object was translated from.
        :param expression: The expression this choice selects on.
        """
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class RangedChoice(VHDLModel_RangedChoice, DOMMixin):
    def __init__(self, node: Iir, rng: Range) -> None:
        """
        Initializes a case choice given by a range.

        :param node: The IIR node this object was translated from.
        :param rng:  The range this choice selects on.
        """
        super().__init__(rng)
        DOMMixin.__init__(self, node)


@export
class Case(VHDLModel_Case, DOMMixin):
    def __init__(
        self,
        node: Iir,
        choices: Iterable[SequentialChoice],
        statements: Iterable[SequentialStatement] = None,
    ) -> None:
        """
        Initializes a case.

        :param node:       The IIR node this object was translated from.
        :param choices:    List of all choices selecting this alternative.
        :param statements: List of all sequential statements in this construct.
        """
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
    ) -> None:
        """
        Initializes a sequential case.

        :param caseNode:   The IIR node of the case statement.
        :param statements: List of all sequential statements in this construct.
        """
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
    ) -> None:
        """
        Initializes a case statement.

        :param caseNode:   The IIR node of the case statement.
        :param label:      The label of a model entity.
        :param expression: The expression being tested.
        :param cases:      List of all alternatives, in the order they were written.
        """
        super().__init__(expression, cases, label)
        DOMMixin.__init__(self, caseNode)

    @classmethod
    def parse(cls, caseNode: Iir, label: str) -> "CaseStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetName,
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
                    rng = GetName(choiceRange)
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
    ) -> None:
        """
        Initializes a for-loop statement.

        :param loopNode:   The IIR node of the loop statement.
        :param loopIndex:  The name of the loop's index.
        :param rng:        The range the loop iterates over.
        :param statements: List of all sequential statements in this construct.
        :param label:      The label of a model entity.
        """
        super().__init__(loopIndex, rng, statements, label)
        DOMMixin.__init__(self, loopNode)

    @classmethod
    def parse(cls, loopNode: Iir, label: str) -> "ForLoopStatement":
        from pyGHDL.dom._Utils import GetNameOfNode
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetDiscreteRangeFromNode,
        )

        spec = nodes.Get_Parameter_Specification(loopNode)
        loopIndex = GetNameOfNode(spec)

        rng = GetDiscreteRangeFromNode(nodes.Get_Discrete_Range(spec), "for...loop statement")

        statementChain = nodes.Get_Sequential_Statement_Chain(loopNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "for", label)

        return cls(loopNode, loopIndex, rng, statements, label)


@export
class WhileLoopStatement(VHDLModel_WhileLoopStatement, DOMMixin):
    def __init__(
        self,
        loopNode: Iir,
        condition: ExpressionUnion,
        statements: Iterable[SequentialStatement] = None,
        label: str = None,
    ) -> None:
        """
        Initializes a while-loop statement.

        :param loopNode:   The IIR node of the loop statement.
        :param condition:  The condition guarding this statement.
        :param statements: List of all sequential statements in this construct.
        :param label:      The label of a model entity.
        """
        super().__init__(condition, statements, label)
        DOMMixin.__init__(self, loopNode)

    @classmethod
    def parse(cls, loopNode: Iir, label: str) -> "WhileLoopStatement":
        from pyGHDL.dom._Utils import GetNameOfNode, GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetRangeFromNode,
            GetName,
            GetOptionalExpressionFromNode,
        )

        # spec = nodes.Get_Parameter_Specification(loopNode)
        # loopIndex = GetNameOfNode(spec)
        #
        # discreteRange = nodes.Get_Discrete_Range(spec)
        # rangeKind = GetIirKindOfNode(discreteRange)
        # if rangeKind == nodes.Iir_Kind.Range_Expression:
        #     rng = GetRangeFromNode(discreteRange)
        # elif rangeKind in (
        #     nodes.Iir_Kind.Attribute_Name,
        #     nodes.Iir_Kind.Parenthesis_Name,
        # ):
        #     rng = GetName(discreteRange)
        # else:
        #     pos = Position.parse(loopNode)
        #     raise DOMException(
        #         f"Unknown discrete range kind '{rangeKind.name}' in for...loop statement at line {pos.Line}."
        #     )

        condition = GetOptionalExpressionFromNode(nodes.Get_Condition(loopNode))

        statementChain = nodes.Get_Sequential_Statement_Chain(loopNode)
        statements = GetSequentialStatementsFromChainedNodes(statementChain, "while", label)

        return cls(loopNode, condition, statements, label)


@export
class SequentialSimpleSignalAssignment(VHDLModel_SequentialSimpleSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: SignalSymbol,
        waveform: Iterable[WaveformElement],
        label: str = None,
    ) -> None:
        """
        Initializes a simple sequential signal assignment.

        :param assignmentNode: The IIR node of the assignment statement.
        :param target:         Reference to the assignment's destination.
        :param waveform:       List of all waveform elements, in the order they were written.
        :param label:          The label of a model entity.
        """
        super().__init__(target, waveform, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))

        waveform = GetWaveformElementsFromChainedNodes(nodes.Get_Waveform_Chain(assignmentNode))

        return cls(assignmentNode, targetName, waveform, label)


def GetConditionalExpressionsFromChainedNodes(nodeChain: Iir) -> Iterable["ConditionalExpression"]:
    """
    Translates a chain of ``Conditional_Expression`` nodes into a sequence of :class:`ConditionalExpression`.

    :param nodeChain: The IIR node starting the chain of conditional expressions.
    :returns:         The translated conditional expressions, in source order.
    """
    return [ConditionalExpression.parse(node) for node in utils.chain_iter(nodeChain)]


def GetSelectedExpressionsFromChainedNodes(nodeChain: Iir) -> Iterable:
    """
    Translates a chain of choices into a sequence of :class:`SelectedExpression`/
    :class:`OthersSelectedExpression`. Same grouping algorithm as
    :func:`pyGHDL.dom.Concurrent.GetSelectedWaveformsFromChainedNodes`, but the associated content is
    a plain expression (``Get_Associated_Expr``) instead of a waveform chain.

    :param nodeChain:     The IIR node starting the chain of choices.
    :returns:             The translated selected expressions, in source order.
    :raises DOMException: If a choice's kind is not handled.
    """
    from pyGHDL.dom._Utils import GetIirKindOfNode
    from pyGHDL.dom._Translate import GetExpressionFromNode, GetRangeFromNode

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
                expression = GetExpressionFromNode(nodes.Get_Associated_Expr(ownerNode))
                alternatives.append(SelectedExpression(ownerNode, choices, expression))
                choices = None

            othersExpression = GetExpressionFromNode(nodes.Get_Associated_Expr(choice))
            alternatives.append(OthersSelectedExpression(choice, othersExpression))
            choice = nodes.Get_Chain(choice)
            continue
        else:
            position = Position.parse(choice)
            raise DOMException(f"Unknown choice kind '{kind.name}' in selected expression at {position}.")

        if choices is not None:
            expression = GetExpressionFromNode(nodes.Get_Associated_Expr(ownerNode))
            alternatives.append(SelectedExpression(ownerNode, choices, expression))

        ownerNode = choice
        choices = [choiceValue]
        choice = nodes.Get_Chain(choice)

    if choices is not None:
        expression = GetExpressionFromNode(nodes.Get_Associated_Expr(ownerNode))
        alternatives.append(SelectedExpression(ownerNode, choices, expression))

    return alternatives


@export
class ConditionalExpression(VHDLModel_ConditionalExpression, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion, condition: ExpressionUnion = None) -> None:
        """
        Initializes a conditional expression.

        :param node:       The IIR node this object was translated from.
        :param expression: The value assigned when the condition holds.
        :param condition:  The condition selecting this alternative.
        """
        super().__init__(expression, condition)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, node: Iir) -> "ConditionalExpression":
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetOptionalExpressionFromNode

        expression = GetExpressionFromNode(nodes.Get_Expression(node))
        condition = GetOptionalExpressionFromNode(nodes.Get_Condition(node))

        return cls(node, expression, condition)


@export
class SelectedExpression(VHDLModel_SelectedExpression, DOMMixin):
    def __init__(self, node: Iir, choices: Iterable, expression: ExpressionUnion) -> None:
        """
        Initializes a selected expression.

        :param node:       The IIR node this object was translated from.
        :param choices:    List of all choices selecting this alternative.
        :param expression: The value assigned for the matching choices.
        """
        super().__init__(choices, expression)
        DOMMixin.__init__(self, node)


@export
class OthersSelectedExpression(VHDLModel_OthersSelectedExpression, DOMMixin):
    def __init__(self, node: Iir, expression: ExpressionUnion) -> None:
        """
        Initializes an others selected expression.

        :param node:       The IIR node this object was translated from.
        :param expression: The value assigned for every unnamed choice.
        """
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class SequentialVariableAssignment(VHDLModel_SequentialVariableAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: VariableSymbol,
        expression: ExpressionUnion,
        label: str = None,
    ) -> None:
        """
        Initializes a simple sequential variable assignment.

        :param assignmentNode: The IIR node of the assignment statement.
        :param target:         Reference to the assignment's destination.
        :param expression:     The assigned expression.
        :param label:          The label of a model entity.
        """
        super().__init__(target, expression, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialVariableAssignment":
        from pyGHDL.dom._Translate import GetName, GetExpressionFromNode

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = VariableSymbol(targetNode, GetName(targetNode))
        expression = GetExpressionFromNode(nodes.Get_Expression(assignmentNode))

        return cls(assignmentNode, targetName, expression, label)


@export
class SequentialConditionalVariableAssignment(VHDLModel_SequentialConditionalVariableAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: VariableSymbol,
        conditionalExpressions: Iterable[ConditionalExpression],
        label: str = None,
    ) -> None:
        """
        Initializes a conditional sequential variable assignment.

        :param assignmentNode:         The IIR node of the assignment statement.
        :param target:                 Reference to the assignment's destination.
        :param conditionalExpressions: List of all alternatives, in the order they were written.
        :param label:                  The label of a model entity.
        """
        super().__init__(target, conditionalExpressions, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialConditionalVariableAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = VariableSymbol(targetNode, GetName(targetNode))
        conditionalExpressions = GetConditionalExpressionsFromChainedNodes(
            nodes.Get_Conditional_Expression_Chain(assignmentNode)
        )

        return cls(assignmentNode, targetName, conditionalExpressions, label)


@export
class SequentialConditionalSignalAssignment(VHDLModel_SequentialConditionalSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: SignalSymbol,
        conditionalWaveforms: Iterable,
        label: str = None,
    ) -> None:
        """
        Initializes a conditional sequential signal assignment.

        :param assignmentNode:       The IIR node of the assignment statement.
        :param target:               Reference to the assignment's destination.
        :param conditionalWaveforms: All alternatives, in order.
        :param label:                The label of a model entity.
        """
        super().__init__(target, conditionalWaveforms, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialConditionalSignalAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))
        conditionalWaveforms = GetConditionalWaveformsFromChainedNodes(
            nodes.Get_Conditional_Waveform_Chain(assignmentNode)
        )

        return cls(assignmentNode, targetName, conditionalWaveforms, label)


@export
class SequentialSelectedVariableAssignment(VHDLModel_SequentialSelectedVariableAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: VariableSymbol,
        expression: ExpressionUnion,
        selectedExpressions: Iterable,
        label: str = None,
    ) -> None:
        """
        Initializes a selected sequential variable assignment.

        :param assignmentNode:      The IIR node of the assignment statement.
        :param target:              Reference to the assignment's destination.
        :param expression:          The selector expression.
        :param selectedExpressions: All alternatives, in order.
        :param label:               The label of a model entity.
        """
        super().__init__(target, expression, selectedExpressions, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialSelectedVariableAssignment":
        from pyGHDL.dom._Translate import GetName, GetExpressionFromNode

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = VariableSymbol(targetNode, GetName(targetNode))
        expression = GetExpressionFromNode(nodes.Get_Expression(assignmentNode))
        selectedExpressions = GetSelectedExpressionsFromChainedNodes(
            nodes.Get_Selected_Expressions_Chain(assignmentNode)
        )

        return cls(assignmentNode, targetName, expression, selectedExpressions, label)


@export
class SequentialSelectedSignalAssignment(VHDLModel_SequentialSelectedSignalAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: SignalSymbol,
        expression: ExpressionUnion,
        selectedWaveforms: Iterable,
        label: str = None,
    ) -> None:
        """
        Initializes a selected sequential signal assignment.

        :param assignmentNode:    The IIR node of the assignment statement.
        :param target:            Reference to the assignment's destination.
        :param expression:        The selector expression.
        :param selectedWaveforms: All alternatives, in order.
        :param label:             The label of a model entity.
        """
        super().__init__(target, expression, selectedWaveforms, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SequentialSelectedSignalAssignment":
        from pyGHDL.dom._Translate import GetName, GetExpressionFromNode

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))
        expression = GetExpressionFromNode(nodes.Get_Expression(assignmentNode))
        selectedWaveforms = GetSelectedWaveformsFromChainedNodes(nodes.Get_Selected_Waveform_Chain(assignmentNode))

        return cls(assignmentNode, targetName, expression, selectedWaveforms, label)


@export
class SignalForceAssignment(VHDLModel_SignalForceAssignment, DOMMixin):
    def __init__(
        self,
        assignmentNode: Iir,
        target: SignalSymbol,
        expression: ExpressionUnion,
        label: str = None,
    ) -> None:
        """
        Initializes a signal force assignment.

        :param assignmentNode: The IIR node of the assignment statement.
        :param target:         Reference to the assignment's destination.
        :param expression:     The value forced onto the signal.
        :param label:          The label of a model entity.
        """
        super().__init__(target, expression, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SignalForceAssignment":
        from pyGHDL.dom._Translate import GetName, GetExpressionFromNode

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))
        expression = GetExpressionFromNode(nodes.Get_Expression(assignmentNode))

        return cls(assignmentNode, targetName, expression, label)


@export
class SignalReleaseAssignment(VHDLModel_SignalReleaseAssignment, DOMMixin):
    def __init__(self, assignmentNode: Iir, target: SignalSymbol, label: str = None) -> None:
        """
        Initializes a signal release assignment.

        :param assignmentNode: The IIR node of the assignment statement.
        :param target:         Reference to the assignment's destination.
        :param label:          The label of a model entity.
        """
        super().__init__(target, label)
        DOMMixin.__init__(self, assignmentNode)

    @classmethod
    def parse(cls, assignmentNode: Iir, label: str = None) -> "SignalReleaseAssignment":
        from pyGHDL.dom._Translate import GetName

        targetNode = nodes.Get_Target(assignmentNode)
        targetName = SignalSymbol(targetNode, GetName(targetNode))

        return cls(assignmentNode, targetName, label)


@export
class SequentialProcedureCall(VHDLModel_SequentialProcedureCall, DOMMixin):
    def __init__(
        self,
        callNode: Iir,
        procedureName: Symbol,
        parameterAssociationItems: Iterable[ParameterAssociationItem],
        label: str = None,
    ) -> None:
        """
        Initializes a procedure call as a sequential statement.

        :param callNode:                  The IIR node of the subprogram call.
        :param procedureName:             Reference to the called procedure.
        :param parameterAssociationItems: List of all parameter associations of the call.
        :param label:                     The label of a model entity.
        """
        super().__init__(procedureName, parameterAssociationItems, label)
        DOMMixin.__init__(self, callNode)

    @classmethod
    def parse(cls, callNode: Iir, label: str) -> "SequentialProcedureCall":
        from pyGHDL.dom._Translate import GetName, GetParameterMapAspect

        cNode = nodes.Get_Procedure_Call(callNode)

        prefix = nodes.Get_Prefix(cNode)
        procedureName = GetName(prefix)
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
    ) -> None:
        """
        Initializes a sequential assertion statement.

        :param assertNode: The IIR node of the assertion.
        :param condition:  The condition guarding this statement.
        :param message:    The reported message, or ``None`` if none was given.
        :param severity:   The reported severity level, or ``None`` if none was given.
        :param label:      The label of a model entity.
        """
        super().__init__(condition, message, severity, label)
        DOMMixin.__init__(self, assertNode)

    @classmethod
    def parse(cls, assertNode: Iir, label: str) -> "SequentialAssertStatement":
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetOptionalExpressionFromNode

        condition = GetExpressionFromNode(nodes.Get_Assertion_Condition(assertNode))
        message = GetOptionalExpressionFromNode(nodes.Get_Report_Expression(assertNode))
        severity = GetOptionalExpressionFromNode(nodes.Get_Severity_Expression(assertNode))

        return cls(assertNode, condition, message, severity, label)


@export
class SequentialReportStatement(VHDLModel_SequentialReportStatement, DOMMixin):
    def __init__(
        self,
        reportNode: Iir,
        message: ExpressionUnion,
        severity: ExpressionUnion = None,
        label: str = None,
    ) -> None:
        """
        Initializes a sequential report statement.

        :param reportNode: The IIR node of the report statement.
        :param message:    The reported message, or ``None`` if none was given.
        :param severity:   The reported severity level, or ``None`` if none was given.
        :param label:      The label of a model entity.
        """
        super().__init__(message, severity, label)
        DOMMixin.__init__(self, reportNode)

    @classmethod
    def parse(cls, reportNode: Iir, label: str) -> "SequentialReportStatement":
        from pyGHDL.dom._Translate import GetExpressionFromNode, GetOptionalExpressionFromNode

        message = GetExpressionFromNode(nodes.Get_Report_Expression(reportNode))
        severity = GetOptionalExpressionFromNode(nodes.Get_Severity_Expression(reportNode))

        return cls(reportNode, message, severity, label)


@export
class ReturnStatement(VHDLModel_ReturnStatement, DOMMixin):
    def __init__(
        self,
        returnNode: Iir,
        returnValue: ExpressionUnion = None,
        label: str = None,
    ) -> None:
        """
        Initializes a return statement.

        :param returnNode:  The IIR node of the return statement.
        :param returnValue: The returned expression, or ``None`` for a procedure.
        :param label:       The label of a model entity.
        """
        super().__init__(returnValue, label)
        DOMMixin.__init__(self, returnNode)

    @classmethod
    def parse(cls, returnNode: Iir, label: str) -> "ReturnStatement":
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        returnValue = GetOptionalExpressionFromNode(nodes.Get_Expression(returnNode))

        return cls(returnNode, returnValue, label)


@export
class NullStatement(VHDLModel_NullStatement, DOMMixin):
    def __init__(
        self,
        waitNode: Iir,
        label: str = None,
    ) -> None:
        """
        Initializes a statement.

        :param waitNode: The IIR node of the wait statement.
        :param label:    The label of a model entity.
        """
        super().__init__(label)
        DOMMixin.__init__(self, waitNode)


@export
class NextStatement(VHDLModel_NextStatement, DOMMixin):
    def __init__(
        self,
        exitNode: Iir,
        condition: ExpressionUnion = None,
        label: str = None,
    ) -> None:
        """
        Initializes a loop control statement.

        :param exitNode:  The IIR node of the exit statement.
        :param condition: The condition guarding this statement.
        :param label:     The label of the loop this statement applies to, or ``None`` for the enclosing loop.
        """
        super().__init__(condition, loopLabel=label)
        DOMMixin.__init__(self, exitNode)

    @classmethod
    def parse(cls, exitNode: Iir, label: str) -> "NextStatement":
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        condition = GetOptionalExpressionFromNode(nodes.Get_Condition(exitNode))

        return cls(exitNode, condition, label)


@export
class ExitStatement(VHDLModel_ExitStatement, DOMMixin):
    def __init__(
        self,
        exitNode: Iir,
        condition: ExpressionUnion = None,
        label: str = None,
    ) -> None:
        """
        Initializes a loop control statement.

        :param exitNode:  The IIR node of the exit statement.
        :param condition: The condition guarding this statement.
        :param label:     The label of the loop this statement applies to, or ``None`` for the enclosing loop.
        """
        super().__init__(condition, loopLabel=label)
        DOMMixin.__init__(self, exitNode)

    @classmethod
    def parse(cls, exitNode: Iir, label: str) -> "ExitStatement":
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        condition = GetOptionalExpressionFromNode(nodes.Get_Condition(exitNode))

        return cls(exitNode, condition, label)


@export
class WaitStatement(VHDLModel_WaitStatement, DOMMixin):
    def __init__(
        self,
        waitNode: Iir,
        sensitivityList: Iterable[Symbol] = None,
        condition: ExpressionUnion = None,
        timeout: ExpressionUnion = None,
        label: str = None,
    ) -> None:
        """
        Initializes a wait statement.

        :param waitNode:        The IIR node of the wait statement.
        :param sensitivityList: List of all signal names to wait on, or ``None`` if none was given.
        :param condition:       The condition guarding this statement.
        :param timeout:         The timeout expression, or ``None`` if none was given.
        :param label:           The label of a model entity.
        """
        super().__init__(sensitivityList, condition, timeout, label)
        DOMMixin.__init__(self, waitNode)

    @classmethod
    def parse(cls, waitNode: Iir, label: str) -> "WaitStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import GetOptionalExpressionFromNode

        sensitivityList = None
        sensitivityListNode = nodes.Get_Sensitivity_List(waitNode)
        if sensitivityListNode is not nodes.Null_Iir:
            pass
            # print(f"WaitStatement: wait on {GetIirKindOfNode(sensitivityListNode)}")

        condition = GetOptionalExpressionFromNode(nodes.Get_Condition_Clause(waitNode))

        timeout = GetOptionalExpressionFromNode(nodes.Get_Timeout_Clause(waitNode))

        return cls(waitNode, sensitivityList, condition, timeout, label)
