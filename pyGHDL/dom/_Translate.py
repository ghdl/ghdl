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
from typing import List, Generator

from pydecor import export

from pyGHDL.dom import Position, DOMException
from pyGHDL.dom.Object import Variable
from pyVHDLModel.VHDLModel import (
    Constraint,
    Direction,
    Expression,
    SubTypeOrSymbol,
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
    SimpleSubTypeSymbol,
    ConstrainedCompositeSubTypeSymbol,
    IndexedObjectOrFunctionCallSymbol,
    ConstrainedScalarSubTypeSymbol,
)
from pyGHDL.dom.Type import (
    IntegerType,
    SubType,
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
)
from pyGHDL.dom.Subprogram import Function, Procedure
from pyGHDL.dom.Misc import Alias


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

        if kind == nodes.Iir_Kind.Association_Element_By_Expression:
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
    subTypeIndication: Iir,
) -> List[Constraint]:
    constraints = []
    for constraint in utils.flist_iter(
        nodes.Get_Index_Constraint_List(subTypeIndication)
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
                    indication=subTypeIndication,
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
        print(1, node, typeName)
        return IncompleteType(node, typeName)

    kind = GetIirKindOfNode(typeDefinition)
    if kind == nodes.Iir_Kind.Range_Expression:
        r = GetRangeFromNode(typeDefinition)
        return IntegerType(node, typeName, r)

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
def GetSubTypeIndicationFromNode(node: Iir, entity: str, name: str) -> SubTypeOrSymbol:
    subTypeIndicationNode = nodes.Get_Subtype_Indication(node)
    #     if subTypeIndicationNode is nodes.Null_Iir:
    #         return None
    return GetSubTypeIndicationFromIndicationNode(subTypeIndicationNode, entity, name)


@export
def GetSubTypeIndicationFromIndicationNode(
    subTypeIndicationNode: Iir, entity: str, name: str
) -> SubTypeOrSymbol:
    if subTypeIndicationNode is nodes.Null_Iir:
        print(
            "[NOT IMPLEMENTED]: Unhandled multiple declarations for {entity} '{name}'.".format(
                entity=entity, name=name
            )
        )
        return None
    kind = GetIirKindOfNode(subTypeIndicationNode)
    if kind in (
        nodes.Iir_Kind.Simple_Name,
        nodes.Iir_Kind.Selected_Name,
        nodes.Iir_Kind.Attribute_Name,
    ):
        return GetSimpleTypeFromNode(subTypeIndicationNode)
    elif kind == nodes.Iir_Kind.Subtype_Definition:
        return GetScalarConstrainedSubTypeFromNode(subTypeIndicationNode)
    elif kind == nodes.Iir_Kind.Array_Subtype_Definition:
        return GetCompositeConstrainedSubTypeFromNode(subTypeIndicationNode)
    else:
        raise DOMException(
            "Unknown kind '{kind}' for an subtype indication in a {entity} of `{name}`.".format(
                kind=kind.name, entity=entity, name=name
            )
        )


@export
def GetSimpleTypeFromNode(subTypeIndicationNode: Iir) -> SimpleSubTypeSymbol:
    subTypeName = GetNameFromNode(subTypeIndicationNode)
    return SimpleSubTypeSymbol(subTypeIndicationNode, subTypeName)


@export
def GetScalarConstrainedSubTypeFromNode(
    subTypeIndicationNode: Iir,
) -> ConstrainedScalarSubTypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subTypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)
    rangeConstraint = nodes.Get_Range_Constraint(subTypeIndicationNode)
    r = GetRangeFromNode(rangeConstraint)
    return ConstrainedScalarSubTypeSymbol(subTypeIndicationNode, typeMarkName, r)


@export
def GetCompositeConstrainedSubTypeFromNode(
    subTypeIndicationNode: Iir,
) -> ConstrainedCompositeSubTypeSymbol:
    typeMark = nodes.Get_Subtype_Type_Mark(subTypeIndicationNode)
    typeMarkName = GetNameOfNode(typeMark)

    constraints = GetArrayConstraintsFromSubtypeIndication(subTypeIndicationNode)
    return ConstrainedCompositeSubTypeSymbol(
        subTypeIndicationNode, typeMarkName, constraints
    )


@export
def GetSubTypeFromNode(subTypeNode: Iir) -> SubTypeOrSymbol:
    subTypeName = GetNameOfNode(subTypeNode)

    return SubType(subTypeNode, subTypeName)


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
    nodes.Iir_Kind.Negation_Operator: NegationExpression,
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
            "Unknown expression kind '{kindName}'({kind}) in expression '{expr}' at {file}:{line}:{column}.".format(
                kind=kind,
                kindName=kind.name,
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

    for generic in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            yield GenericConstantInterfaceItem.parse(generic)
        elif kind == nodes.Iir_Kind.Interface_Type_Declaration:
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


@export
def GetPortsFromChainedNodes(
    nodeChain: Iir,
) -> Generator[PortInterfaceItem, None, None]:
    for port in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = PortSignalInterfaceItem.parse(port)

            yield portSignal
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
    for parameter in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(parameter)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterConstantInterfaceItem

            yield ParameterConstantInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_Variable_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterVariableInterfaceItem

            yield ParameterVariableInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterSignalInterfaceItem

            yield ParameterSignalInterfaceItem.parse(parameter)
        elif kind == nodes.Iir_Kind.Interface_File_Declaration:
            from pyGHDL.dom.InterfaceItem import ParameterFileInterfaceItem

            yield ParameterFileInterfaceItem.parse(parameter)
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


def GetDeclaredItemsFromChainedNodes(
    nodeChain: Iir, entity: str, name: str
) -> Generator[ModelEntity, None, None]:
    for item in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            yield Constant.parse(item)

        elif kind == nodes.Iir_Kind.Variable_Declaration:
            from pyGHDL.dom.Object import SharedVariable

            if nodes.Get_Shared_Flag(item):
                yield SharedVariable.parse(item)
            else:
                yield Variable.parse(item)
        #                raise DOMException("Found non-shared variable.")
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            yield Signal.parse(item)
        elif kind == nodes.Iir_Kind.File_Declaration:
            from pyGHDL.dom.Object import File

            yield File.parse(item)
        elif kind == nodes.Iir_Kind.Type_Declaration:
            yield GetTypeFromNode(item)

        elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
            yield GetAnonymousTypeFromNode(item)

        elif kind == nodes.Iir_Kind.Subtype_Declaration:
            yield GetSubTypeFromNode(item)

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
        elif kind == nodes.Iir_Kind.Package_Instantiation_Declaration:
            from pyGHDL.dom.DesignUnit import PackageInstantiation

            yield PackageInstantiation.parse(item)
        elif kind == nodes.Iir_Kind.Configuration_Specification:
            print(
                "[NOT IMPLEMENTED] Configuration specification in {name}".format(
                    name=name
                )
            )
        elif kind == nodes.Iir_Kind.Group_Declaration:
            print("[NOT IMPLEMENTED] Group declaration in {name}".format(name=name))
        elif kind == nodes.Iir_Kind.Group_Template_Declaration:
            print(
                "[NOT IMPLEMENTED] Group template declaration in {name}".format(
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


def GetAliasFromNode(aliasNode: Iir):
    aliasName = GetNameOfNode(aliasNode)

    return Alias(aliasNode, aliasName)
