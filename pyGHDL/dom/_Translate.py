from typing import List


from pydecor import export
from pyVHDLModel.VHDLModel import Constraint, Direction, Expression, SubTypeOrSymbol

from pyGHDL.libghdl import utils, name_table
from pyGHDL.libghdl.utils import flist_iter
from pyGHDL.libghdl.vhdl import nodes
from pyGHDL.dom._Utils import NodeToName, GetIirKindOfNode
from pyGHDL.dom.Common import DOMException
from pyGHDL.dom.Range import Range, RangeExpression
from pyGHDL.dom.Symbol import (
    SimpleObjectSymbol,
    SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol,
)
from pyGHDL.dom.Literal import IntegerLiteral, CharacterLiteral, FloatingPointLiteral
from pyGHDL.dom.Expression import (
    SubtractionExpression,
    AdditionExpression,
    MultiplyExpression,
    DivisionExpression,
    InverseExpression,
    ExponentiationExpression,
)

__all__ = []


@export
def GetSubtypeIndicationFromNode(node, entity: str, name: str) -> SubTypeOrSymbol:
    subTypeIndication = nodes.Get_Subtype_Indication(node)
    subTypeKind = GetIirKindOfNode(subTypeIndication)

    if subTypeKind == nodes.Iir_Kind.Simple_Name:
        subTypeName = NodeToName(subTypeIndication)

        subType = SimpleSubTypeSymbol(subTypeName)
    elif subTypeKind == nodes.Iir_Kind.Array_Subtype_Definition:
        typeMark = nodes.Get_Subtype_Type_Mark(subTypeIndication)
        typeMarkName = NodeToName(typeMark)

        constraints = GetArrayConstraintsFromSubtypeIndication(subTypeIndication)
        subType = ConstrainedSubTypeSymbol(typeMarkName, constraints)
    elif subTypeKind == nodes.Iir_Kind.Subtype_Definition:
        raise DOMException(
            "Unknown handling of subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}'.".format(
                kind=subTypeKind, indication=subTypeIndication, entity=entity, name=name
            )
        )
    else:
        raise DOMException(
            "Unknown subtype kind '{kind}' of subtype indication '{indication}' while parsing {entity} '{name}'.".format(
                kind=subTypeKind, indication=subTypeIndication, entity=entity, name=name
            )
        )

    return subType


@export
def GetArrayConstraintsFromSubtypeIndication(subTypeIndication) -> List[Constraint]:
    constraints = []
    for constraint in flist_iter(nodes.Get_Index_Constraint_List(subTypeIndication)):
        constraintKind = GetIirKindOfNode(constraint)
        if constraintKind == nodes.Iir_Kind.Range_Expression:
            direction = nodes.Get_Direction(constraint)
            leftBound = nodes.Get_Left_Limit_Expr(constraint)
            rightBound = nodes.Get_Right_Limit_Expr(constraint)

            r = Range(
                GetExpressionFromNode(leftBound),
                GetExpressionFromNode(rightBound),
                Direction.DownTo if direction else Direction.To,
            )
            constraints.append(RangeExpression(r))
        elif constraintKind == nodes.Iir_Kind.Attribute_Name:
            raise DOMException("[NOT IMPLEMENTED] Attribute name as range.")
        elif constraintKind == nodes.Iir_Kind.Simple_Name:
            raise DOMException("[NOT IMPLEMENTED] Subtype as range.")
        else:
            raise DOMException(
                "Unknown constraint kind '{kind}' for constraint '{constraint}' in subtype indication '{indication}'.".format(
                    kind=constraintKind,
                    constraint=constraint,
                    indication=subTypeIndication,
                )
            )

    return constraints


@export
def GetExpressionFromNode(node) -> Expression:
    kind = GetIirKindOfNode(node)

    if kind == nodes.Iir_Kind.Simple_Name:
        name = NodeToName(node)
        return SimpleObjectSymbol(name)
    elif kind == nodes.Iir_Kind.Integer_Literal:
        integerLiteralValue = nodes.Get_Value(node)
        return IntegerLiteral(integerLiteralValue)
    elif kind == nodes.Iir_Kind.Floating_Point_Literal:
        fpLiteralValue = nodes.Get_Fp_Value(node)
        return FloatingPointLiteral(fpLiteralValue)
    elif kind == nodes.Iir_Kind.Character_Literal:
        identifier = nodes.Get_Identifier(node)
        characterLiteralValue = name_table.Get_Character(identifier)
        return CharacterLiteral(characterLiteralValue)
    elif kind == nodes.Iir_Kind.Negation_Operator:
        operand = GetExpressionFromNode(nodes.Get_Operand(node))
        return InverseExpression(operand)
    elif kind == nodes.Iir_Kind.Addition_Operator:
        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return AdditionExpression(left, right)
    elif kind == nodes.Iir_Kind.Substraction_Operator:
        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return SubtractionExpression(left, right)
    elif kind == nodes.Iir_Kind.Multiplication_Operator:
        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return MultiplyExpression(left, right)
    elif kind == nodes.Iir_Kind.Division_Operator:
        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return DivisionExpression(left, right)
    elif kind == nodes.Iir_Kind.Exponentiation_Operator:
        left = GetExpressionFromNode(nodes.Get_Left(node))
        right = GetExpressionFromNode(nodes.Get_Right(node))
        return ExponentiationExpression(left, right)
    else:
        raise DOMException(
            "Unknown expression kind '{kindName}'({kind}) in expression '{expr}'.".format(
                kind=kind, kindName=kind.name, expr=node
            )
        )


# FIXME: rewrite to generator
@export
def GetGenericsFromChainedNodes(nodeChain):
    result = []
    for generic in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(generic)
        if kind == nodes.Iir_Kind.Interface_Constant_Declaration:
            from pyGHDL.dom.InterfaceItem import GenericConstantInterfaceItem

            genericConstant = GenericConstantInterfaceItem.parse(generic)

            result.append(genericConstant)
        else:
            raise DOMException(
                "Unknown generic kind '{kindName}'({kind}) in generic '{generic}'.".format(
                    kind=kind, kindName=kind.name, generic=generic
                )
            )

    return result


# FIXME: rewrite to generator
@export
def GetPortsFromChainedNodes(nodeChain):
    result = []
    for port in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(port)
        if kind == nodes.Iir_Kind.Interface_Signal_Declaration:
            from pyGHDL.dom.InterfaceItem import PortSignalInterfaceItem

            portSignal = PortSignalInterfaceItem.parse(port)

            result.append(portSignal)
        else:
            raise DOMException(
                "Unknown port kind '{kindName}'({kind}) in port '{port}'.".format(
                    kind=kind, kindName=kind.name, port=port
                )
            )

    return result


def GetDeclaredItemsFromChainedNodes(nodeChain, entity: str, name: str):
    result = []
    for item in utils.chain_iter(nodeChain):
        kind = GetIirKindOfNode(item)
        if kind == nodes.Iir_Kind.Constant_Declaration:
            from pyGHDL.dom.Object import Constant

            constantName = NodeToName(item)
            subTypeIndication = GetSubtypeIndicationFromNode(
                item, "constant", constantName
            )
            defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(item))

            constant = Constant(constantName, subTypeIndication, defaultExpression)

            result.append(constant)
        elif kind == nodes.Iir_Kind.Signal_Declaration:
            from pyGHDL.dom.Object import Signal

            signalName = NodeToName(item)
            subTypeIndication = GetSubtypeIndicationFromNode(item, "signal", signalName)
            defaultExpression = GetExpressionFromNode(nodes.Get_Default_Value(item))

            constant = Signal(signalName, subTypeIndication, defaultExpression)

            result.append(constant)
        elif kind == nodes.Iir_Kind.Anonymous_Type_Declaration:
            typeName = NodeToName(item)
            print("found type '{name}'".format(name=typeName))
        elif kind == nodes.Iir_Kind.Subtype_Declaration:
            subTypeName = NodeToName(item)
            print("found subtype '{name}'".format(name=subTypeName))
        elif kind == nodes.Iir_Kind.Function_Declaration:
            functionName = NodeToName(item)
            print("found function '{name}'".format(name=functionName))
        elif kind == nodes.Iir_Kind.Function_Body:
            #                functionName = NodeToName(item)
            print("found function body '{name}'".format(name="????"))
        elif kind == nodes.Iir_Kind.Object_Alias_Declaration:
            aliasName = NodeToName(item)
            print("found alias '{name}'".format(name=aliasName))
        else:
            raise DOMException(
                "Unknown declared item kind '{kindName}'({kind}) in {entity} '{name}'.".format(
                    kind=kind, kindName=kind.name, entity=entity, name=name
                )
            )

    return result
