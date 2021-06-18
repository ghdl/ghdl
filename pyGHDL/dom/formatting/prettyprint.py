from typing import List, Union

from pydecor import export

from pyGHDL.dom.Aggregates import SimpleAggregateElement, IndexedAggregateElement, RangedAggregateElement, NamedAggregateElement, OthersAggregateElement
from pyGHDL.dom.Object import Constant, Signal
from pyGHDL.dom.Range import Range
from pyVHDLModel.VHDLModel import (
    GenericInterfaceItem,
    Expression,
    Direction,
    Mode,
    NamedEntity,
    PortInterfaceItem,
    BinaryExpression,
    IdentityExpression,
    UnaryExpression,
    WithDefaultExpression,
    AggregateElement
)

from pyGHDL import GHDLBaseException
from pyGHDL.dom.Misc import Document
from pyGHDL.dom.DesignUnit import (
    Entity,
    Architecture,
    Package,
    PackageBody,
    Configuration,
    Context,
)
from pyGHDL.dom.InterfaceItem import (
    GenericConstantInterfaceItem,
    PortSignalInterfaceItem,
)
from pyGHDL.dom.Symbol import (
    SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol,
    SimpleObjectSymbol,
)
from pyGHDL.dom.Literal import IntegerLiteral, CharacterLiteral, FloatingPointLiteral

from pyGHDL.dom.Expression import (
    SubtractionExpression,
    AdditionExpression,
    MultiplyExpression,
    DivisionExpression,
    InverseExpression,
    AbsoluteExpression,
    NegationExpression,
    ExponentiationExpression, Aggregate,
)

StringBuffer = List[str]

DirectionTranslation = {Direction.To: "to", Direction.DownTo: "downto"}

ModeTranslation = {
    Mode.In: "in",
    Mode.Out: "out",
    Mode.InOut: "inout",
    Mode.Buffer: "buffer",
    Mode.Linkage: "linkage",
}

UnaryExpressionTranslation = {
    IdentityExpression: " +",
    NegationExpression: " -",
    InverseExpression: "not ",
    AbsoluteExpression: "abs ",
}

BinaryExpressionTranslation = {
    AdditionExpression: " + ",
    SubtractionExpression: " - ",
    MultiplyExpression: " * ",
    DivisionExpression: " / ",
    ExponentiationExpression: "**",
}


@export
class PrettyPrintException(GHDLBaseException):
    pass


@export
class PrettyPrint:
    # _buffer: StringBuffer
    #
    # def __init__(self):
    #     self._buffer = []

    def formatDocument(self, document: Document, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(
            "{prefix}Document '{doc!s}':".format(doc=document.Path, prefix=prefix)
        )
        buffer.append("{prefix}  Entities:".format(prefix=prefix))
        for entity in document.Entities:
            for line in self.formatEntity(entity, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Architectures:".format(prefix=prefix))
        for architecture in document.Architectures:
            for line in self.formatArchitecture(architecture, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Packages:".format(prefix=prefix))
        for package in document.Packages:
            for line in self.formatPackage(package, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  PackageBodies:".format(prefix=prefix))
        for packageBodies in document.PackageBodies:
            for line in self.formatPackageBody(packageBodies, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Configurations:".format(prefix=prefix))
        for configuration in document.Configurations:
            for line in self.formatConfiguration(configuration, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Contexts:".format(prefix=prefix))
        for context in document.Contexts:
            for line in self.formatContext(context, level + 1):
                buffer.append(line)

        return buffer

    def formatEntity(self, entity: Entity, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=entity.Name, prefix=prefix))
        buffer.append("{prefix}  Generics:".format(prefix=prefix))
        for generic in entity.GenericItems:
            for line in self.formatGeneric(generic, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Ports:".format(prefix=prefix))
        for port in entity.PortItems:
            for line in self.formatPort(port, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Declared:".format(prefix=prefix))
        for item in entity.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)

        return buffer

    def formatArchitecture(
        self, architecture: Architecture, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=architecture.Name, prefix=prefix))
        buffer.append("{prefix}  Declared:".format(prefix=prefix))
        for item in architecture.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 2):
                buffer.append(line)

        return buffer

    def formatPackage(self, package: Package, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=package.Name, prefix=prefix))
        buffer.append("{prefix}  Declared:".format(prefix=prefix))
        for item in package.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)

        return buffer

    def formatPackageBody(
        self, packageBody: PackageBody, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=packageBody.Name, prefix=prefix))
        buffer.append("{prefix}  Declared:".format(prefix=prefix))
        for item in packageBody.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 1):
                buffer.append(line)

        return buffer

    def formatConfiguration(
        self, configuration: Configuration, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=configuration.Name, prefix=prefix))

        return buffer

    def formatContext(self, context: Context, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- {name}".format(name=context.Name, prefix=prefix))

        return buffer

    def formatGeneric(
        self, generic: Union[NamedEntity, GenericInterfaceItem], level: int = 0
    ) -> StringBuffer:
        if isinstance(generic, GenericConstantInterfaceItem):
            return self.formatGenericConstant(generic, level)
        else:
            raise PrettyPrintException(
                "Unhandled generic kind for generic '{name}'.".format(name=generic.Name)
            )

    def formatPort(
        self, port: Union[NamedEntity, PortInterfaceItem], level: int = 0
    ) -> StringBuffer:
        if isinstance(port, PortSignalInterfaceItem):
            return self.formatPortSignal(port, level)
        else:
            raise PrettyPrintException(
                "Unhandled port kind for port '{name}'.".format(name=port.Name)
            )

    def formatGenericConstant(
        self, generic: GenericConstantInterfaceItem, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        subType = generic.SubType
        if isinstance(subType, SimpleSubTypeSymbol):
            buffer.append(
                "{prefix}  - {name} : {mode} {type}{initialValue}".format(
                    prefix=prefix,
                    name=generic.Name,
                    mode=ModeTranslation[generic.Mode],
                    type=subType.SymbolName,
                    initialValue=self.formatInitialValue(generic),
                )
            )
        elif isinstance(subType, ConstrainedSubTypeSymbol):
            buffer.append(
                "{prefix}  - {name} : {mode} {type}({constraints}){initialValue}".format(
                    prefix=prefix,
                    name=generic.Name,
                    mode=ModeTranslation[generic.Mode],
                    type=subType.SymbolName,
                    constraints=", ".join(
                        [
                            "{left} {dir} {right}".format(
                                left=self.formatExpression(constraint.Range.LeftBound),
                                right=self.formatExpression(
                                    constraint.Range.RightBound
                                ),
                                dir=DirectionTranslation[constraint.Range.Direction],
                            )
                            for constraint in subType.Constraints
                        ]
                    ),
                    initialValue=self.formatInitialValue(generic),
                )
            )
        else:
            raise PrettyPrintException(
                "Unhandled constraint kind for generic '{name}'.".format(
                    name=generic.Name
                )
            )

        return buffer

    def formatPortSignal(
        self, port: PortSignalInterfaceItem, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        buffer.append(
            "{prefix}  - {name} : {mode} {subtypeindication}{initialValue}".format(
                prefix=prefix,
                name=port.Name,
                mode=ModeTranslation[port.Mode],
                subtypeindication=self.formatSubtypeIndication(
                    port.SubType, "port", port.Name
                ),
                initialValue=self.formatInitialValue(port),
            )
        )

        return buffer

    def formatDeclaredItems(self, item, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        if isinstance(item, Constant):
            buffer.append(
                "{prefix}- constant {name} : {subtype} := {expr}".format(
                    prefix=prefix,
                    name=item.Name,
                    subtype=self.formatSubtypeIndication(
                        item.SubType, "constant", item.Name
                    ),
                    expr=self.formatExpression(item.DefaultExpression),
                )
            )
        elif isinstance(item, Signal):
            buffer.append(
                "{prefix}- signal {name} : {subtype}{initValue}".format(
                    prefix=prefix,
                    name=item.Name,
                    subtype=self.formatSubtypeIndication(
                        item.SubType, "signal", item.Name
                    ),
                    initValue=" := {expr}".format(
                        expr=self.formatExpression(item.DefaultExpression)
                    )
                    if item.DefaultExpression is not None
                    else "",
                )
            )
        else:
            raise PrettyPrintException("Unhandled declared item kind.")

        return buffer

    def formatSubtypeIndication(self, subTypeIndication, entity: str, name: str) -> str:
        if isinstance(subTypeIndication, SimpleSubTypeSymbol):
            return "{type}".format(type=subTypeIndication.SymbolName)
        elif isinstance(subTypeIndication, ConstrainedSubTypeSymbol):
            constraints = ", ".join(
                [self.formatRange(constraint.Range) for constraint in subTypeIndication.Constraints]
            )

            return "{type}({constraints})".format(
                type=subTypeIndication.SymbolName, constraints=constraints
            )
        else:
            raise PrettyPrintException(
                "Unhandled constraint kind for {entity} '{name}'.".format(
                    entity=entity, name=name
                )
            )

    def formatInitialValue(self, item: WithDefaultExpression) -> str:
        if item.DefaultExpression is None:
            return ""

        return " := {expr}".format(expr=self.formatExpression(item.DefaultExpression))

    def formatRange(self, r: Range) -> str:
        return "{left} {dir} {right}".format(
            left=self.formatExpression(r.LeftBound),
            right=self.formatExpression(r.RightBound),
            dir=DirectionTranslation[r.Direction],
        )

    def formatExpression(self, expression: Expression) -> str:
        if isinstance(expression, SimpleObjectSymbol):
            return "{name}".format(name=expression.SymbolName)
        elif isinstance(expression, IntegerLiteral):
            return "{value}".format(value=expression.Value)
        elif isinstance(expression, FloatingPointLiteral):
            return "{value}".format(value=expression.Value)
        elif isinstance(expression, CharacterLiteral):
            return "'{value}'".format(value=expression.Value)
        elif isinstance(expression, UnaryExpression):
            try:
                operator = UnaryExpressionTranslation[type(expression)]
            except KeyError:
                raise PrettyPrintException("Unhandled operator for unary expression.")

            return "{operator}{operand}".format(
                operand=self.formatExpression(expression.Operand), operator=operator
            )
        elif isinstance(expression, BinaryExpression):
            try:
                operator = BinaryExpressionTranslation[type(expression)]
            except KeyError:
                raise PrettyPrintException("Unhandled operator for binary expression.")

            return "{left}{operator}{right}".format(
                left=self.formatExpression(expression.LeftOperand),
                right=self.formatExpression(expression.RightOperand),
                operator=operator,
            )
        elif isinstance(expression, Aggregate):
            return "({choices})".format(choices=", ".join([self.formatAggregateElement(element) for element in expression.Elements]))
        else:
            raise PrettyPrintException("Unhandled expression kind.")

    def formatAggregateElement(self, aggregateElement: AggregateElement):
        if isinstance(aggregateElement, SimpleAggregateElement):
            return "{value}".format(
                value=self.formatExpression(aggregateElement.Expression)
            )
        elif isinstance(aggregateElement, IndexedAggregateElement):
            return "{index} => {value}".format(
                index=self.formatExpression(aggregateElement.Index),
                value=self.formatExpression(aggregateElement.Expression)
            )
        elif isinstance(aggregateElement, RangedAggregateElement):
            return "{range} => {value}".format(
                range=self.formatRange(aggregateElement.Range),
                value=self.formatExpression(aggregateElement.Expression)
            )
        elif isinstance(aggregateElement, NamedAggregateElement):
            return "{name} => {value}".format(
                name=aggregateElement.Name,
                value=self.formatExpression(aggregateElement.Expression)
            )
        elif isinstance(aggregateElement, OthersAggregateElement):
            return "other => {value}".format(
                value=self.formatExpression(aggregateElement.Expression)
            )
