from typing import List, Union

from pydecor import export

from pyGHDL.dom.Misc import Alias
from pyGHDL.dom.Subprogram import Procedure
from pyGHDL.dom.Type import IntegerType, SubType
from pyVHDLModel.VHDLModel import (
    GenericInterfaceItem,
    NamedEntity,
    PortInterfaceItem,
    WithDefaultExpression,
    Function,
)

from pyGHDL import GHDLBaseException
from pyGHDL.dom.NonStandard import Document, Design, Library
from pyGHDL.dom.DesignUnit import (
    Entity,
    Architecture,
    Package,
    PackageBody,
    Configuration,
    Context,
    Component,
)
from pyGHDL.dom.Object import Constant, Signal
from pyGHDL.dom.InterfaceItem import (
    GenericConstantInterfaceItem,
    PortSignalInterfaceItem,
)
from pyGHDL.dom.Symbol import (
    SimpleSubTypeSymbol,
    ConstrainedSubTypeSymbol,
)


StringBuffer = List[str]


@export
class PrettyPrintException(GHDLBaseException):
    pass


@export
class PrettyPrint:
    # _buffer: StringBuffer
    #
    # def __init__(self):
    #     self._buffer = []

    def formatDesign(self, design: Design, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}Libraries:".format(prefix=prefix))
        for library in design.Libraries:
            for line in self.formatLibrary(library, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Documents:".format(prefix=prefix))
        for document in design.Documents:
            buffer.append(
                "{prefix}- Path: '{doc!s}':".format(doc=document.Path, prefix=prefix)
            )
            for line in self.formatDocument(document, level + 1):
                buffer.append(line)

        return buffer

    def formatLibrary(self, library: Library, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}Entities:".format(prefix=prefix))
        for entity in library.Entities:
            for line in self.formatEntity(entity, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Architectures:".format(prefix=prefix))
        for architecture in library.Architectures:
            for line in self.formatArchitecture(architecture, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Packages:".format(prefix=prefix))
        for package in library.Packages:
            for line in self.formatPackage(package, level + 1):
                buffer.append(line)
        buffer.append("{prefix}PackageBodies:".format(prefix=prefix))
        for packageBodies in library.PackageBodies:
            for line in self.formatPackageBody(packageBodies, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Configurations:".format(prefix=prefix))
        for configuration in library.Configurations:
            for line in self.formatConfiguration(configuration, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Contexts:".format(prefix=prefix))
        for context in library.Contexts:
            for line in self.formatContext(context, level + 1):
                buffer.append(line)

        return buffer

    def formatDocument(self, document: Document, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}Entities:".format(prefix=prefix))
        for entity in document.Entities:
            for line in self.formatEntity(entity, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Architectures:".format(prefix=prefix))
        for architecture in document.Architectures:
            for line in self.formatArchitecture(architecture, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Packages:".format(prefix=prefix))
        for package in document.Packages:
            for line in self.formatPackage(package, level + 1):
                buffer.append(line)
        buffer.append("{prefix}PackageBodies:".format(prefix=prefix))
        for packageBodies in document.PackageBodies:
            for line in self.formatPackageBody(packageBodies, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Configurations:".format(prefix=prefix))
        for configuration in document.Configurations:
            for line in self.formatConfiguration(configuration, level + 1):
                buffer.append(line)
        buffer.append("{prefix}Contexts:".format(prefix=prefix))
        for context in document.Contexts:
            for line in self.formatContext(context, level + 1):
                buffer.append(line)

        return buffer

    def formatEntity(self, entity: Entity, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- Name: {name}".format(name=entity.Name, prefix=prefix))
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
        buffer.append(
            "{prefix}- Name: {name}".format(name=architecture.Name, prefix=prefix)
        )
        buffer.append(
            "{prefix}  Entity: {entity}".format(
                entity=architecture.Entity.SymbolName, prefix=prefix
            )
        )
        buffer.append("{prefix}  Declared:".format(prefix=prefix))
        for item in architecture.DeclaredItems:
            for line in self.formatDeclaredItems(item, level + 2):
                buffer.append(line)

        return buffer

    def formatComponent(self, component: Component, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append(
            "{prefix}- Component: {name}".format(name=component.Name, prefix=prefix)
        )
        buffer.append("{prefix}  Generics:".format(prefix=prefix))
        for generic in component.GenericItems:
            for line in self.formatGeneric(generic, level + 1):
                buffer.append(line)
        buffer.append("{prefix}  Ports:".format(prefix=prefix))
        for port in component.PortItems:
            for line in self.formatPort(port, level + 1):
                buffer.append(line)

        return buffer

    def formatPackage(self, package: Package, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- Name: {name}".format(name=package.Name, prefix=prefix))
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
        buffer.append(
            "{prefix}- Name: {name}".format(name=packageBody.Name, prefix=prefix)
        )
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
        buffer.append(
            "{prefix}- Name: {name}".format(name=configuration.Name, prefix=prefix)
        )

        return buffer

    def formatContext(self, context: Context, level: int = 0) -> StringBuffer:
        buffer = []
        prefix = "  " * level
        buffer.append("{prefix}- Name: {name}".format(name=context.Name, prefix=prefix))

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

        buffer.append(
            "{prefix}  - {name} : {mode!s} {subtypeindication}{initialValue}".format(
                prefix=prefix,
                name=generic.Name,
                mode=generic.Mode,
                subtypeindication=self.formatSubtypeIndication(
                    generic.SubType, "generic", generic.Name
                ),
                initialValue=self.formatInitialValue(generic),
            )
        )

        return buffer

    def formatPortSignal(
        self, port: PortSignalInterfaceItem, level: int = 0
    ) -> StringBuffer:
        buffer = []
        prefix = "  " * level

        buffer.append(
            "{prefix}  - {name} : {mode!s} {subtypeindication}{initialValue}".format(
                prefix=prefix,
                name=port.Name,
                mode=port.Mode,
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
                    expr=str(item.DefaultExpression),
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
                    initValue=" := {expr}".format(expr=str(item.DefaultExpression))
                    if item.DefaultExpression is not None
                    else "",
                )
            )
        elif isinstance(item, IntegerType):
            buffer.append(
                "{prefix}- type {name} is range {range}".format(
                    prefix=prefix,
                    name=item.Name,
                    range="{left!s} to {right!s}".format(
                        left=item.LeftBound, right=item.RightBound
                    ),
                )
            )
        elif isinstance(item, SubType):
            buffer.append(
                "{prefix}- subtype {name} is ?????".format(
                    prefix=prefix,
                    name=item.Name,
                )
            )
        elif isinstance(item, Alias):
            buffer.append(
                "{prefix}- alias {name} is ?????".format(
                    prefix=prefix,
                    name=item.Name,
                )
            )
        elif isinstance(item, Function):
            buffer.append(
                "{prefix}- function {name}".format(
                    prefix=prefix,
                    name=item.Name,
                )
            )
        elif isinstance(item, Procedure):
            buffer.append(
                "{prefix}- procedure {name}".format(
                    prefix=prefix,
                    name=item.Name,
                )
            )
        elif isinstance(item, Component):
            for line in self.formatComponent(item, level):
                buffer.append(line)
        else:
            raise PrettyPrintException(
                "Unhandled declared item kind '{name}'.".format(
                    name=item.__class__.__name__
                )
            )

        return buffer

    def formatSubtypeIndication(self, subTypeIndication, entity: str, name: str) -> str:
        if isinstance(subTypeIndication, SimpleSubTypeSymbol):
            return "{type}".format(type=subTypeIndication.SymbolName)
        elif isinstance(subTypeIndication, ConstrainedSubTypeSymbol):
            ranges = [str(c.Range) for c in subTypeIndication.Constraints]
            constraints = ", ".join(ranges)

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

        return " := {expr!s}".format(expr=item.DefaultExpression)
