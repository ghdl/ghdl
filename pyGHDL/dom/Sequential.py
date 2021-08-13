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

from pydecor import export

from pyGHDL.dom.Concurrent import WaveformElement  # TODO: move out from concurrent?
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
    Name,
    SequentialStatement,
    Expression,
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
        condition: Expression,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(condition, statements)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "IfBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(nodes.Get_Condition(generateNode))
        body = nodes.Get_Generate_Statement_Body(generateNode)

        statementChain = nodes.Get_Sequential_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(
            statementChain, "if branch", label
        )

        return cls(generateNode, condition, statements)


@export
class ElsifBranch(VHDLModel_ElsifBranch):
    def __init__(
        self,
        branchNode: Iir,
        condition: Expression,
        statements: Iterable[SequentialStatement] = None,
    ):
        super().__init__(condition, statements)
        DOMMixin.__init__(self, branchNode)

    @classmethod
    def parse(cls, generateNode: Iir, condition: Iir, label: str) -> "ElsifBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
            GetExpressionFromNode,
        )

        condition = GetExpressionFromNode(condition)
        body = nodes.Get_Generate_Statement_Body(generateNode)

        statementChain = nodes.Get_Sequential_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(
            statementChain, "elsif branch", label
        )

        return cls(generateNode, condition, statements)


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
    def parse(cls, generateNode: Iir, label: str) -> "ElseBranch":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
        )

        body = nodes.Get_Generate_Statement_Body(generateNode)

        statementChain = nodes.Get_Sequential_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(
            statementChain, "else branch", label
        )

        return cls(generateNode, statements)


@export
class IfStatement(VHDLModel_IfStatement, DOMMixin):
    def __init__(
        self,
        generateNode: Iir,
        ifBranch: IfBranch,
        elsifBranches: Iterable[ElsifBranch] = None,
        elseBranch: ElseBranch = None,
        label: str = None,
    ):
        super().__init__(ifBranch, elsifBranches, elseBranch, label)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "IfStatement":
        ifBranch = IfBranch.parse(generateNode, label)
        elsifBranches = []
        elseBranch = None
        # WORKAROUND: Python 3.8 syntax
        # elseClause = generateNode
        # while (elseClause := nodes.Get_Generate_Else_Clause(elseClause)) != nodes.Null_Iir:
        elseClause = nodes.Get_Generate_Else_Clause(generateNode)
        while elseClause != nodes.Null_Iir:
            condition = nodes.Get_Condition(elseClause)
            if condition != nodes.Null_Iir:
                elsifBranches.append(ElsifBranch.parse(elseClause, condition, label))
            else:
                elseBranch = ElseBranch.parse(elseClause, label)
                break

            elseClause = nodes.Get_Generate_Else_Clause(elseClause)

        return cls(generateNode, ifBranch, elsifBranches, elseBranch, label)


@export
class IndexedChoice(VHDLModel_IndexedChoice, DOMMixin):
    def __init__(self, node: Iir, expression: Expression):
        super().__init__(expression)
        DOMMixin.__init__(self, node)


@export
class RangedChoice(VHDLModel_RangedChoice, DOMMixin):
    def __init__(self, node: Iir, rng: Range):
        super().__init__(rng)
        DOMMixin.__init__(self, node)


@export
class Case(VHDLModel_Case, DOMMixin):
    def __init__(self, node: Iir, choices: Iterable[SequentialChoice], statements: Iterable[SequentialStatement]):
        super().__init__(choices, statements)
        DOMMixin.__init__(self, node)

    @classmethod
    def parse(cls, caseNode: Iir, label: str) -> "Case":
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
        )

        body = nodes.Get_Generate_Statement_Body(caseNode)

        statementChain = nodes.Get_Sequential_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(
            statementChain, "case", label
        )

        choices = []

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
    def parse(cls, caseNode: Iir) -> "OthersCase":
        from pyGHDL.dom._Translate import (
            GetDeclaredItemsFromChainedNodes,
            GetSequentialStatementsFromChainedNodes,
        )

        # body = nodes.Get_Generate_Statement_Body(caseNode)
        #
        # statementChain = nodes.Get_Sequential_Statement_Chain(body)
        # statements = GetStatementsFromChainedNodes(
        #     statementChain, "else branch", alternativeLabel
        # )

        # return cls(caseNode, declaredItems, statements, alternativeLabel)
        return cls(caseNode, [], [], "")


@export
class CaseStatement(VHDLModel_CaseStatement, DOMMixin):
    def __init__(
        self, generateNode: Iir, label: str, expression: Expression, cases: Iterable[SequentialCase]
    ):
        super().__init__(expression, cases, label)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "CaseStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetExpressionFromNode,
            GetRangeFromNode,
            GetNameFromNode,
        )

        # TODO: get choices

        expression = GetExpressionFromNode(nodes.Get_Expression(generateNode))

        cases = []
        choices = []
        alternatives = nodes.Get_Case_Statement_Alternative_Chain(generateNode)
        for alternative in utils.chain_iter(alternatives):
            choiceKind = GetIirKindOfNode(alternative)

            if choiceKind in (
                nodes.Iir_Kind.Choice_By_Name,
                nodes.Iir_Kind.Choice_By_Expression,
            ):
                choiceExpression = GetExpressionFromNode(
                    nodes.Get_Choice_Expression(alternative)
                )
                choices.append(IndexedChoice(alternative, choiceExpression))
                cases.append(Case(alternative, choices))
                choices = []
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
                    pos = Position.parse(generateNode)
                    raise DOMException(
                        "Unknown choice range kind '{kind}' in case statement at line {line}.".format(
                            kind=choiceRangeKind.name, line=pos.Line
                        )
                    )

                choices.append(RangedChoice(alternative, rng))
                cases.append(Case(alternative, choices))
                choices = []
            elif choiceKind is nodes.Iir_Kind.Choice_By_Others:
                cases.append(OthersCase.parse(alternative))
            else:
                print(choiceKind)

        return cls(generateNode, label, expression, cases)


@export
class ForLoopStatement(VHDLModel_ForLoopStatement, DOMMixin):
    def __init__(
        self,
        generateNode: Iir,
        loopIndex: str,
        range: Range,
        statements: Iterable[SequentialStatement] = None,
        label: str = None,
    ):
        super().__init__(loopIndex, range, statements, label)
        DOMMixin.__init__(self, generateNode)

    @classmethod
    def parse(cls, generateNode: Iir, label: str) -> "ForLoopStatement":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import (
            GetSequentialStatementsFromChainedNodes,
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
                "Unknown discete range kind '{kind}' in for...loop statement at line {line}.".format(
                    kind=rangeKind.name, line=pos.Line
                )
            )

        body = nodes.Get_Generate_Statement_Body(generateNode)

        statementChain = nodes.Get_Sequential_Statement_Chain(body)
        statements = GetSequentialStatementsFromChainedNodes(
            statementChain, "for", label
        )

        return cls(generateNode, loopIndex, rng, statements, label)


@export
class SequentialSimpleSignalAssignment(
    VHDLModel_SequentialSimpleSignalAssignment, DOMMixin
):
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
    def parse(
        cls, assignmentNode: Iir, label: str = None
    ) -> "SequentialSimpleSignalAssignment":
        from pyGHDL.dom._Translate import GetNameFromNode

        target = nodes.Get_Target(assignmentNode)
        targetName = GetNameFromNode(target)

        waveform = nodes.Get_Waveform_Chain(assignmentNode)

        # TODO: translate waveforms to series of "expressions".
        expression = [None]

        return cls(assignmentNode, targetName, expression, label)


@export
class SequentialProcedureCall(VHDLModel_SequentialProcedureCall, DOMMixin):
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
    def parse(cls, callNode: Iir, label: str) -> "SequentialProcedureCall":
        from pyGHDL.dom._Utils import GetIirKindOfNode
        from pyGHDL.dom._Translate import GetNameFromNode

        call = nodes.Get_Procedure_Call(callNode)
        prefix = nodes.Get_Prefix(call)

        procedureName = GetNameFromNode(prefix)

        # TODO: parameter mappings
        parameterMappings = []

        return cls(callNode, label, procedureName, parameterMappings)
