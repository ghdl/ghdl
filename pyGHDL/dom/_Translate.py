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
# Package module:   DOM: IIR to *** translations.
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
from typing import List, Generator

from pydecor import export

from pyVHDLModel.SyntaxModel import (
    Constraint,
    Direction,
    Expression,
    SubtypeOrSymbol,
    BaseType,
    GenericInterfaceItem,
    PortInterfaceItem,
    ParameterInterfaceItem,
    ModelEntity,
    Name,
)

from pyGHDL.libghdl import utils
from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom import Position, DOMException
from pyGHDL.dom._Utils import (
    GetNameOfNode,
    GetIirKindOfNode,
)
from pyGHDL.dom.Names import (
    SimpleName,
    SelectedName,
    AttributeName,
    ParenthesisName,
    AllName,
)
from pyGHDL.dom.Symbol import (
    SimpleObjectOrFunctionCallSymbol,
    SimpleSubtypeSymbol,
    ConstrainedCompositeSubtypeSymbol,
    IndexedObjectOrFunctionCallSymbol,
    ConstrainedScalarSubtypeSymbol,
)
from pyGHDL.dom.Type import (
    IntegerType,
    Subtype,
    ArrayType,
    RecordType,
    EnumeratedType,
    AccessType,
    ProtectedType,
    ProtectedTypeBody,
    FileType,
    PhysicalType,
    IncompleteType,
)
from pyGHDL.dom.Range import Range
from pyGHDL.dom.Literal import (
    IntegerLiteral,
    CharacterLiteral,
    FloatingPointLiteral,
    StringLiteral,
    PhysicalIntegerLiteral,
    PhysicalFloatingLiteral,
    NullLiteral,
)
from pyGHDL.dom.Object import Variable
from pyGHDL.dom.Expression import (
    SubtractionExpression,
    AdditionExpression,
    MultiplyExpression,
    DivisionExpression,
    InverseExpression,
    ExponentiationExpression,
    Aggregate,
    NegationExpression,
    ParenthesisExpression,
    ConcatenationExpression,
    QualifiedExpression,
    ModuloExpression,
    RemainderExpression,
    AndExpression,
    NandExpression,
    OrExpression,
    NorExpression,
    XorExpression,
    XnorExpression,
    EqualExpression,
    UnequalExpression,
    LessThanExpression,
    GreaterThanExpression,
    GreaterEqualExpression,
    LessEqualExpression,
    ShiftLeftLogicExpression,
    ShiftRightLogicExpression,
    ShiftLeftArithmeticExpression,
    ShiftRightArithmeticExpression,
    RotateLeftExpression,
    RotateRightExpression,
    RangeExpression,
    QualifiedExpressionAllocation,
    SubtypeAllocation,
    IdentityExpression,
    AbsoluteExpression,
    MatchingGreaterEqualExpression,
    MatchingEqualExpression,
    MatchingUnequalExpression,
    MatchingLessThanExpression,
    MatchingLessEqualExpression,
    MatchingGreaterThanExpression,
)
from pyGHDL.dom.Subprogram import Function, Procedure
from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.PSL import DefaultClock


__all__ = []


@export
def GetNameFromNode(node: Iir) -> Name:
    kind = GetIirKindOfNode(node)
    if kind == nodes.Iir_Kind.Simple_Name:
        name = GetNameOfNode(node)
        return SimpleName(node, name)
    elif kind == nodes.Iir_Kind.Selected_Name:
        name = GetNameOfNode(node)
        prefixName = GetNameFromNode(nodes.Get_Prefix(node))
        return SelectedName(node, name, prefixName)
    elif kind == nodes.Iir_Kind.Attribute_Name:
        name = GetNameOfNode(node)
        prefixName = GetNameFromNode(nodes.Get_Prefix(node))
        return AttributeName(node, name, prefixName)
    elif kind == nodes.Iir_Kind.Parenthesis_Name:
        prefixName = GetNameFromNode(nodes.Get_Prefix(node))
        associations = GetAssociations(node)

        return ParenthesisName(node, prefixName, associations)
    elif kind == nodes.Iir_Kind.Selected_By_All_Name:
        prefixName = GetNameFromNode(nodes.Get_Prefix(node))
        return AllName(node, prefixName)
    else:
        raise DOMException("Unknown name kind '{kind}'".format(kind=kind.name))


def GetAssociations(node: Iir) -> List:
    associations = []
    for item in utils.chain_iter(nodes.Get_Association_Chain(node)):
        kind = GetIirKindOfNode(item)

        if kind in (
            nodes.Iir_Kind.Association_Element_By_Expression,
            nodes.Iir_Kind.Association_Element_By_Name,
        ):
            actual = nodes.Get_Actual(item)
            expr = GetExpressionFromNode(actual)

            associations.append(expr)
        else:
            raise DOMException(
                "Unknown association kind '{kindName}'({kind}) in array index/slice or function call '{node}'.".format(
                    kind=kind, kindName=kind.name, node=node
                )
            )

    return associations


@export
def GetArrayConstraintsFromSubtypeIndication(
    subtypeIndication: Iir,
) -> List[Constraint]:
    constraints = []
    for constraint in utils.flist_iter(
        nodes.Get_Index_Constraint_List(subtypeIndication)
    ):
        constraintKind = GetIirKindOfNode(constraint)
        if constraintKind == nodes.Iir_Kind.Range_Expression:
            constraints.append(RangeExpression.parse(constraint))
        elif constraintKind in (
            nodes.Iir_Kind.Simple_Name,
            nodes.Iir_Kind.Parenthesis_Name,
            nodes.Iir_Kind.Selected_Name,
            nodes.Iir_Kind.Attribute_Name,
        ):
            constraints.append(GetNameFromNode(constraint))
        else:
            position = Position.parse(constraint)
            raise DOMException(
                "Unknown constraint kind '{kind}' for constraint '{constraint}' in subtype indication '{indication}' at {file}:{line}:{column}.".format(
                    kind=constraintKind.name,
                    constraint=constraint,
                    indication=subtypeIndication,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

    return constraints


@export
def GetTypeFromNode(node: Iir) -> BaseType:
    typeName = GetNameOfNode(node)
    typeDefinition = nodes.Get_Type_Definition(node)
    if typeDefinition is nodes.Null_Iir:
        return IncompleteType(node, typeName)

    kind = GetIirKindOfNode(typeDefinition)
    if kind == nodes.Iir_Kind.Enumeration_Type_Definition:
        return EnumeratedType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Array_Type_Definition:
        return ArrayType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Record_Type_Definition:
        return RecordType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Access_Type_Definition:
        return AccessType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.File_Type_Definition:
        return FileType.parse(typeName, typeDefinition)
    elif kind == nodes.Iir_Kind.Protected_Type_Declaration:
        return ProtectedType.parse(typeName, typeDefinition)
    else:
        position = Position.parse(typeDefinition)
        raise DOMException(
            "GetTypeFromNode: Unknown type definition kind '{kind}' for type '{name}' at {file}:{line}:{column}.".format(
                kind=kind.name,
                name=typeName,
                file=position.Filename,
                line=position.Line,
                column=position.Column,
            )
        )


@export
def GetAnonymousTypeFromNode(node: Iir) -> BaseType:
    typeName = GetNameOfNode(node)
    typeDefinition = nodes.Get_Type_Definition(node)
    if typeDefinition is nodes.Null_Iir:
        return IncompleteType(node, typeName)

    kind = GetIirKindOfNode(typeDefinition)
    if kind == nodes.Iir_Kind.Range_Expression:
        r = GetRangeFromNode(typeDefinition)
        return IntegerType(node, typeName, r)

    elif kind in (nodes.Iir_Kind.Attribute_Name, nodes.Iir_Kind.Parenthesis_Name):
        n = GetNameFromNode(typeDefinition)

        return IntegerType(node, typeName, n)
    elif kind == nodes.Iir_Kind.Physical_Type_Definition:
        return PhysicalType.parse(typeName, typeDefinition)

    elif kind == nodes.Iir_Kind.Array_Subtype_Definition:
        print("[NOT IMPLEMENTED] Array_Subtype_Definition")

        return ArrayType(typeDefinition, "????", [], None)
    else:
        position = Position.parse(typeDefinition)
        raise DOMException(
            "GetAnonymousTypeFromNode: Unknown type definition kind '{kind}' for type '{name}' at {file}:{line}:{column}.".format(
                kind=kind.name,
                name=typeName,
                file=position.Filename,
                line=position.Line,
                column=position.Column,
            )
        )


@export
def GetSubtypeIndicationFromNode(node: Iir, entity: str, name: str) -> SubtypeOrSymbol:
    subtypeIndicationNode = nodes.Get_Subtype_Indication(node)
    return GetSubtypeIndicationFromIndicationNode(subtypeIndicationNode, entity, name)


@export
def GetSubtypeIndicationFromIndicationNode(
    subtypeIndicationNode: Iir, entity: str, name: str
) -> SubtypeOrSymbol:
    if subtypeIndicationNode is nodes.Null_Iir:
        raise ValueError("Parameter 'subtypeIndicationNode' is 'Null_Iir'.")

    kind = GetIirKindOfNode(subtypeIndicationNode)
    if kind in (
        nodes.Iir_Kind.Simple_Name,
        nodes.Iir_Kind.Selected_Name,
        nodes.Iir_Kind.Attribute_Name,
    ):
        return GetSimpleTypeFromNode(subtypeIndicationNode)
    elif kind == nodes.Iir_Kind.Subtype_Definition:
        return GetScalarConstrainedSubtypeFromNode(subtypeIndicationNode)
    elif kind == nodes.Iir_Kind.Array_Subtype_Definition:
        return GetCompositeConstrainedSubtypeFromNode(subtypeIndicationNode)
    else:
        raise DOMException(
            "Unknown kind '{kind}' for an subtype indication in a {entity} of `{name}`.".format(
                kind=kind.name, entity=entity, name=name
            )
        )


@export
def GetSimpleTypeFromNode(subtypeIndicationNode: Iir) -> SimpleSubtypeSymbol:
    subtypeName = GetNameFromNode(subtypeIndicationNode)
    return SimpleSubtypeSymbol(subtypeIndicationNode, subtypeName)


@export
def GetScalarConstrainedSubtypeFromNode(
    subtypeIndicationNode: Iir,
) -> ConstrainedScalarSubtypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subtypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    simpleTypeMark = SimpleName(typeMark, typeMarkName)
    rangeConstraint = nodes.Get_Range_Constraint(subtypeIndicationNode)
    r = GetRangeFromNode(rangeConstraint)
    return ConstrainedScalarSubtypeSymbol(subtypeIndicationNode, simpleTypeMark, r)


@export
def GetCompositeConstrainedSubtypeFromNode(
    subtypeIndicationNode: Iir,
) -> ConstrainedCompositeSubtypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subtypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    simpleTypeMark = SimpleName(typeMark, typeMarkName)

    constraints = GetArrayConstraintsFromSubtypeIndication(subtypeIndicationNode)
    return ConstrainedCompositeSubtypeSymbol(
        subtypeIndicationNode, simpleTypeMark, constraints
    )


@export
def GetSubtypeFromNode(subtypeNode: Iir) -> SubtypeOrSymbol:
    subtypeName = GetNameOfNode(subtypeNode)

    return Subtype(subtypeNode, subtypeName)


@export
def GetRangeFromNode(node: Iir) -> Range:
    direction = nodes.Get_Direction(node)
    leftBound = nodes.Get_Left_Limit_Expr(node)
    rightBound = nodes.Get_Right_Limit_Expr(node)

    return Range(
        GetExpressionFromNode(leftBound),
        GetExpressionFromNode(rightBound),
        Direction.DownTo if direction else Direction.To,
    )


__EXPRESSION_TRANSLATION = {
    nodes.Iir_Kind.Simple_Name: SimpleObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Selected_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Attribute_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Parenthesis_Name: IndexedObjectOrFunctionCallSymbol,
    nodes.Iir_Kind.Null_Literal: NullLiteral,
    nodes.Iir_Kind.Integer_Literal: IntegerLiteral,
    nodes.Iir_Kind.Floating_Point_Literal: FloatingPointLiteral,
    nodes.Iir_Kind.Physical_Int_Literal: PhysicalIntegerLiteral,
    nodes.Iir_Kind.Physical_Fp_Literal: PhysicalFloatingLiteral,
    nodes.Iir_Kind.Character_Literal: CharacterLiteral,
    nodes.Iir_Kind.String_Literal8: StringLiteral,
    nodes.Iir_Kind.Identity_Operator: IdentityExpression,
    nodes.Iir_Kind.Negation_Operator: NegationExpression,
    nodes.Iir_Kind.Absolute_Operator: AbsoluteExpression,
    nodes.Iir_Kind.Range_Expression: RangeExpression,
    nodes.Iir_Kind.Addition_Operator: AdditionExpression,
    nodes.Iir_Kind.Concatenation_Operator: ConcatenationExpression,
    nodes.Iir_Kind.Not_Operator: InverseExpression,
    nodes.Iir_Kind.Parenthesis_Expression: ParenthesisExpression,
    nodes.Iir_Kind.Substraction_Operator: SubtractionExpression,
    nodes.Iir_Kind.Multiplication_Operator: MultiplyExpression,
    nodes.Iir_Kind.Division_Operator: DivisionExpression,
    nodes.Iir_Kind.Modulus_Operator: ModuloExpression,
    nodes.Iir_Kind.Remainder_Operator: RemainderExpression,
    nodes.Iir_Kind.Exponentiation_Operator: ExponentiationExpression,
    nodes.Iir_Kind.And_Operator: AndExpression,
    nodes.Iir_Kind.Nand_Operator: NandExpression,
    nodes.Iir_Kind.Or_Operator: OrExpression,
    nodes.Iir_Kind.Nor_Operator: NorExpression,
    nodes.Iir_Kind.Xor_Operator: XorExpression,
    nodes.Iir_Kind.Xnor_Operator: XnorExpression,
    nodes.Iir_Kind.Equality_Operator: EqualExpression,
    nodes.Iir_Kind.Inequality_Operator: UnequalExpression,
    nodes.Iir_Kind.Less_Than_Operator: LessThanExpression,
    nodes.Iir_Kind.Less_Than_Or_Equal_Operator: LessEqualExpression,
    nodes.Iir_Kind.Greater_Than_Operator: GreaterThanExpression,
    nodes.Iir_Kind.Greater_Than_Or_Equal_Operator: GreaterEqualExpression,
    nodes.Iir_Kind.Match_Equality_Operator: MatchingEqualExpression,
    nodes.Iir_Kind.Match_Inequality_Operator: MatchingUnequalExpression,
    nodes.Iir_Kind.Match_Less_Than_Operator: MatchingLessThanExpression,
    nodes.Iir_Kind.Match_Less_Than_Or_Equal_Operator: MatchingLessEqualExpression,
    nodes.Iir_Kind.Match_Greater_Than_Operator: MatchingGreaterThanExpression,
    nodes.Iir_Kind.Match_Greater_Than_Or_Equal_Operator: MatchingGreaterEqualExpression,
    nodes.Iir_Kind.Sll_Operator: ShiftLeftLogicExpression,
    nodes.Iir_Kind.Srl_Operator: ShiftRightLogicExpression,
    nodes.Iir_Kind.Sla_Operator: ShiftLeftArithmeticExpression,
    nodes.Iir_Kind.Sra_Operator: ShiftRightArithmeticExpression,
    nodes.Iir_Kind.Rol_Operator: RotateLeftExpression,
    nodes.Iir_Kind.Ror_Operator: RotateRightExpression,
    nodes.Iir_Kind.Qualified_Expression: QualifiedExpression,
    nodes.Iir_Kind.Aggregate: Aggregate,
    nodes.Iir_Kind.Allocator_By_Subtype: SubtypeAllocation,
    nodes.Iir_Kind.Allocator_By_Expression: QualifiedExpressionAllocation,
}


@export
def GetExpressionFromNode(node: Iir) -> Expression:
    kind = GetIirKindOfNode(node)

    try:
        cls = __EXPRESSION_TRANSLATION[kind]
    except KeyError:
        position = Position.parse(node)
        raise DOMException(
            "Unknown expression kind '{kind}' in expression '{expr}' at {file}:{line}:{column}.".format(
                kind=kind.name,
                expr=node,
                file=position.Filename,
                line=position.Line,
                column=position.Column,
            )
        )

    return cls.parse(node)


@export
def GetGenericsFromChainedNodes(
    nodeChain: Iir,
) -> Generator[GenericInterfaceItem, None, None]:
    from pyGHDL.dom.InterfaceItem import (
        GenericTypeInterfaceItem,
        GenericPackageInterfaceItem,
        GenericProcedureInterfaceItem,
        GenericFunctionInterfaceItem,
    )

    generic = nodeChain
    while generic != nodes.Null_Iir:
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            genericConstant = GenericConstantInterfaceItem.parse(generic)

            if nodes.Get_Has_Identifier_List(generic):
                nextNode = nodes.Get_Chain(generic)
                for nextGeneric in utils.chain_iter(nextNode):
                    if nodes.Get_Subtype_Indication(nextGeneric) == nodes.Null_Iir:
                        genericConstant.Identifiers.append(GetNameOfNode(nextGeneric))
                    else:
                        generic = nextGeneric
                        break
            else:
                generic = nodes.Get_Chain(generic)

            yield genericConstant
            continue
        else:
            if kind == nodes.Iir_Kind.Interface_Type_Declaration:
                yield GenericTypeInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Package_Declaration:
                yield GenericPackageInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Procedure_Declaration:
                yield GenericProcedureInterfaceItem.parse(generic)
            elif kind == nodes.Iir_Kind.Interface_Function_Declaration:
                yield GenericFunctionInterfaceItem.parse(generic)
            else:
                position = Position.parse(generic)
                raise DOMException(
                    "Unknown generic kind '{kindName}'({kind}) in generic '{generic}' at {file}:{line}:{column}.".format(
                        kind=kind,
                        kindName=kind.name,
                        generic=generic,
                        file=position.Filename,
                        line=position.Line,
                        column=position.Column,
                    )
                )

        generic = nodes.Get_Chain(generic)


@export
def GetPortsFromChainedNodes(
    nodeChain: Iir,
) -> Generator[PortInterfaceItem, None, None]:

    port = nodeChain
    while port != nodes.Null_Iir:
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = PortSignalInterfaceItem.parse(port)

            if nodes.Get_Has_Identifier_List(port):
                nextNode = nodes.Get_Chain(port)
                for nextPort in utils.chain_iter(nextNode):
                    if nodes.Get_Subtype_Indication(nextPort) == nodes.Null_Iir:
                        portSignal.Identifiers.append(GetNameOfNode(nextPort))
                    else:
                        port = nextPort
                        break
            else:
                port = nodes.Get_Chain(port)

            yield portSignal
            continue
        else:
            position = Position.parse(port)
            raise DOMException(
                "Unknown port kind '{kindName}'({kind}) in port '{port}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    port=port,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )


@export
def GetParameterFromChainedNodes(
    nodeChain: Iir,
) -> Generator[ParameterInterfaceItem, None, None]:

    parameter = nodeChain
    while parameter != nodes.Null_Iir:
        kind = GetIirKindOfNode(parameter)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterConstantInterfaceItem

            param = ParameterConstantInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_Variable_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterVariableInterfaceItem

            param = ParameterVariableInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterSignalInterfaceItem

            param = ParameterSignalInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_File_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterFileInterfaceItem

            param = ParameterFileInterfaceItem.parse(parameter)
        else:
            position = Position.parse(parameter)
            raise DOMException(
                "Unknown parameter kind '{kindName}'({kind}) in parameter '{param}' at {file}:{line}:{column}.".format(
                    kind=kind,
                    kindName=kind.name,
                    param=parameter,
                    file=position.Filename,
                    line=position.Line,
                    column=position.Column,
                )
            )

        if nodes.Get_Has_Identifier_List(parameter):
            nextNode = nodes.Get_Chain(parameter)
            for nextParameter in utils.chain_iter(nextNode):
                if nodes.Get_Subtype_Indication(nextParameter) == nodes.Null_Iir:
                    param.Identifiers.append(GetNameOfNode(nextParameter))
                else:
                    parameter = nextParameter
                    break
        else:
            parameter = nodes.Get_Chain(parameter)

        yield param


def GetDeclaredItemsFromChainedNodes(
    nodeChain: Iir, entity: str, name: str
) -> Generator[ModelEntity, None, None]:
    item = nodeChain
    while item != nodes.Null_Iir:
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            obj = Constant.parse(item)

        elif kind == nodes.Iir_Kind.Variable_Declaration:
            from pyGHDL.dom.Object import SharedVariable

            if nodes.Get_Shared_Flag(item):
                obj = SharedVariable.parse(item)
            else:
                obj = Variable.parse(item)
        #                raise DOMException("Found non-shared variable.")
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            obj = Signal.parse(item)
        elif kind == nodes.Iir_Kind.File_Declaration:
            from pyGHDL.dom.Object import File

            obj = File.parse(item)
        else:
            if kind == nodes.Iir_Kind.Type_Declaration:
                yield GetTypeFromNode(item)

            elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
                yield GetAnonymousTypeFromNode(item)

            elif kind == nodes.Iir_Kind.Subtype_Declaration:
                yield GetSubtypeFromNode(item)

            elif kind == nodes.Iir_Kind.Function_Declaration:
                yield Function.parse(item)

            elif kind == nodes.Iir_Kind.Function_Body:
                #                procedureName = NodeToName(item)
                print("found function body '{name}'".format(name="????"))
            elif kind == nodes.Iir_Kind.Procedure_Declaration:
                yield Procedure.parse(item)
            elif kind == nodes.Iir_Kind.Procedure_Body:
                #                procedureName = NodeToName(item)
                print("found procedure body '{name}'".format(name="????"))
            elif kind == nodes.Iir_Kind.Protected_Type_Body:
                yield ProtectedTypeBody.parse(item)
            elif kind == nodes.Iir_Kind.Object_Alias_Declaration:
                yield GetAliasFromNode(item)
            elif kind == nodes.Iir_Kind.Component_Declaration:
                from pyGHDL.dom.DesignUnit import Component

                yield Component.parse(item)
            elif kind == nodes.Iir_Kind.Attribute_Declaration:
                from pyGHDL.dom.Attribute import Attribute

                yield Attribute.parse(item)
            elif kind == nodes.Iir_Kind.Attribute_Specification:
                from pyGHDL.dom.Attribute import AttributeSpecification

                yield AttributeSpecification.parse(item)
            elif kind == nodes.Iir_Kind.Use_Clause:
                from pyGHDL.dom.DesignUnit import UseClause

                yield UseClause.parse(item)
            elif kind == nodes.Iir_Kind.Package_Declaration:
                from pyGHDL.dom.DesignUnit import Package

                yield Package.parse(item)
            elif kind == nodes.Iir_Kind.Package_Instantiation_Declaration:
                from pyGHDL.dom.DesignUnit import PackageInstantiation

                yield PackageInstantiation.parse(item)
            elif kind == nodes.Iir_Kind.Configuration_Specification:
                print(
                    "[NOT IMPLEMENTED] Configuration specification in {name}".format(
                        name=name
                    )
                )
            elif kind == nodes.Iir_Kind.Psl_Default_Clock:
                yield DefaultClock.parse(item)
            elif kind == nodes.Iir_Kind.Group_Declaration:
                print("[NOT IMPLEMENTED] Group declaration in {name}".format(name=name))
            elif kind == nodes.Iir_Kind.Group_Template_Declaration:
                print(
                    "[NOT IMPLEMENTED] Group template declaration in {name}".format(
                        name=name
                    )
                )
            elif kind == nodes.Iir_Kind.Disconnection_Specification:
                print(
                    "[NOT IMPLEMENTED] Disconnect specification in {name}".format(
                        name=name
                    )
                )
            else:
                position = Position.parse(item)
                raise DOMException(
                    "Unknown declared item kind '{kind}' in {entity} '{name}' at {file}:{line}:{column}.".format(
                        kind=kind.name,
                        entity=entity,
                        name=name,
                        file=position.Filename,
                        line=position.Line,
                        column=position.Column,
                    )
                )

            item = nodes.Get_Chain(item)
            continue

        if nodes.Get_Has_Identifier_List(item):
            nextNode = nodes.Get_Chain(item)
            for nextItem in utils.chain_iter(nextNode):
                if nodes.Get_Subtype_Indication(nextItem) == nodes.Null_Iir:
                    obj.Identifiers.append(GetNameOfNode(nextItem))
                else:
                    item = nextItem
                    break
        else:
            item = nodes.Get_Chain(item)

        yield obj


def GetAliasFromNode(aliasNode: Iir):
    aliasName = GetNameOfNode(aliasNode)

    return Alias(aliasNode, aliasName)
