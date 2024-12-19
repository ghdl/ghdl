from enum import Flag
from pathlib import Path
from textwrap import dedent
from typing import Dict, List, ClassVar

from pyTooling.Decorators import export
from pyTooling.Graph import Graph, Vertex
from pyTooling.MetaClasses import abstractmethod, ExtendedType

from pyVHDLModel import (
    DependencyGraphVertexKind,
    DependencyGraphEdgeKind,
    Library as VHDLModel_Library,
    Document as VHDLModel_Document,
)


@export
class Formatter:  # (metaclass=ExtendedType):
    _graph: Graph

    NODE_COLORS: ClassVar[Dict[Flag, str]]
    EDGE_COLORS: ClassVar[Dict[Flag, str]]

    def __init__(self, graph: Graph) -> None:
        self._graph = graph

    @abstractmethod
    def WriteGraphML(self, path: Path):
        pass


@export
class DependencyGraphFormatter(Formatter):
    NODE_COLORS = {
        DependencyGraphVertexKind.Document: "#999999",
        DependencyGraphVertexKind.Library: "#99ccff",
        DependencyGraphVertexKind.Package: "#ff9900",
        DependencyGraphVertexKind.PackageBody: "#ff9900",
        DependencyGraphVertexKind.Context: "#cc99ff",
        DependencyGraphVertexKind.Entity: "#ffff99",
        DependencyGraphVertexKind.Architecture: "#ff99cc",
        DependencyGraphVertexKind.Configuration: "#ff9900",
    }
    EDGE_COLORS = {
        DependencyGraphEdgeKind.SourceFile: "#000000",
        DependencyGraphEdgeKind.CompileOrder: "#ff0000",
        DependencyGraphEdgeKind.LibraryClause: "#000000",
        DependencyGraphEdgeKind.UseClause: "#000000",
        DependencyGraphEdgeKind.ContextReference: "#000000",
        DependencyGraphEdgeKind.EntityImplementation: "#99ccff",
        DependencyGraphEdgeKind.PackageImplementation: "#99ccff",
        DependencyGraphEdgeKind.EntityInstantiation: "#000000",
        DependencyGraphEdgeKind.ComponentInstantiation: "#000000",
        DependencyGraphEdgeKind.ConfigurationInstantiation: "#000000",
    }

    def WriteGraphML(self, path: Path):
        with path.open("w") as file:
            file.write(
                dedent(
                    f"""\
            <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
              <key id="nd1" for="node" attr.name="id" attr.type="string"/>
              <key id="nd2" for="node" attr.name="value" attr.type="string"/>
              <key id="nd3" for="node" attr.name="kind" attr.type="string"/>
              <key id="nd4" for="node" attr.name="color" attr.type="string"/>

              <key id="ed3" for="edge" attr.name="kind" attr.type="string"/>
              <key id="ed4" for="edge" attr.name="color" attr.type="string"/>
              <graph id="DependencyGraph"
                     edgedefault="directed"
                     parse.nodes="{len(self._graph._verticesWithID)}"
                     parse.edges="{len(self._graph._edgesWithoutID)}"
                     parse.order="nodesfirst">
            """
                )
            )
            groups: Dict[str, List[Vertex]] = {}
            for vertex in self._graph._verticesWithID.values():
                if isinstance(vertex.Value, VHDLModel_Library):
                    identifier = vertex.Value.NormalizedIdentifier
                elif isinstance(vertex.Value, VHDLModel_Document):
                    identifier = vertex.Value.DesignUnits[0].Library.NormalizedIdentifier
                else:
                    identifier = vertex.Value.Library.NormalizedIdentifier

                if identifier in groups:
                    groups[identifier].append(vertex)
                else:
                    groups[identifier] = [vertex]

            for group, vertices in groups.items():
                file.write(
                    dedent(
                        """\
                    {prefix}<node id="grp_{id}">
                    {prefix}  <data key="nd2">{value}</data>
                    {prefix}  <graph id="{id}" edgedefault="directed">
                """
                    ).format(prefix="    ", id=group, value=group)
                )

                for vertex in vertices:
                    if vertex["kind"] is DependencyGraphVertexKind.Architecture:
                        value = f"{vertex.Value.Entity.Name.Identifier}({vertex.Value.Identifier})"
                    elif vertex["kind"] is DependencyGraphVertexKind.Document:
                        value = f"{vertex.ID}"
                    else:
                        value = f"{vertex.Value.Identifier}"
                    file.write(
                        dedent(
                            """\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{value}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """
                        ).format(prefix="        ", vertex=vertex, value=value, color=self.NODE_COLORS[vertex["kind"]])
                    )

                file.write(
                    dedent(
                        """\
                    {prefix}  </graph>
                    {prefix}</node>
                """
                    ).format(prefix="    ")
                )

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(
                    dedent(
                        """\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """
                    ).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]])
                )
                edgeCount += 1

            file.write(
                dedent(
                    """\
              </graph>
            </graphml>
            """
                )
            )


@export
class HierarchyGraphFormatter(Formatter):
    NODE_COLORS = {
        DependencyGraphVertexKind.Document: "#999999",
        DependencyGraphVertexKind.Library: "#99ccff",
        DependencyGraphVertexKind.Package: "#ff9900",
        DependencyGraphVertexKind.PackageBody: "#ff9900",
        DependencyGraphVertexKind.Context: "#cc99ff",
        DependencyGraphVertexKind.Entity: "#ffff99",
        DependencyGraphVertexKind.Architecture: "#ff99cc",
        DependencyGraphVertexKind.Configuration: "#ff9900",
    }
    EDGE_COLORS = {
        DependencyGraphEdgeKind.SourceFile: "#000000",
        DependencyGraphEdgeKind.CompileOrder: "#ff0000",
        DependencyGraphEdgeKind.LibraryClause: "#000000",
        DependencyGraphEdgeKind.UseClause: "#000000",
        DependencyGraphEdgeKind.ContextReference: "#000000",
        DependencyGraphEdgeKind.EntityImplementation: "#99ccff",
        DependencyGraphEdgeKind.PackageImplementation: "#99ccff",
        DependencyGraphEdgeKind.EntityInstantiation: "#000000",
        DependencyGraphEdgeKind.ComponentInstantiation: "#000000",
        DependencyGraphEdgeKind.ConfigurationInstantiation: "#000000",
    }

    def WriteGraphML(self, path: Path):
        with path.open("w") as file:
            file.write(
                dedent(
                    f"""\
            <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
              <key id="nd1" for="node" attr.name="id" attr.type="string"/>
              <key id="nd2" for="node" attr.name="value" attr.type="string"/>
              <key id="nd3" for="node" attr.name="kind" attr.type="string"/>
              <key id="nd4" for="node" attr.name="color" attr.type="string"/>

              <key id="ed3" for="edge" attr.name="kind" attr.type="string"/>
              <key id="ed4" for="edge" attr.name="color" attr.type="string"/>
              <graph id="HierarchyGraph"
                     edgedefault="directed"
                     parse.nodes="{len(self._graph._verticesWithID)}"
                     parse.edges="{len(self._graph._edgesWithoutID)}"
                     parse.order="nodesfirst">
            """
                )
            )

            for vertex in self._graph._verticesWithID.values():
                if vertex["kind"] is DependencyGraphVertexKind.Entity:
                    file.write(
                        dedent(
                            """\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Identifier}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """
                        ).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]])
                    )
                elif vertex["kind"] is DependencyGraphVertexKind.Architecture:
                    file.write(
                        dedent(
                            """\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Identifier}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """
                        ).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]])
                    )

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(
                    dedent(
                        """\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """
                    ).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]])
                )
                edgeCount += 1

            file.write(
                dedent(
                    """\
              </graph>
            </graphml>
            """
                )
            )


@export
class CompileOrderGraphFormatter(Formatter):
    NODE_COLORS = {
        DependencyGraphVertexKind.Document: "#999999",
        DependencyGraphVertexKind.Library: "#99ccff",
        DependencyGraphVertexKind.Package: "#ff9900",
        DependencyGraphVertexKind.PackageBody: "#ff9900",
        DependencyGraphVertexKind.Context: "#cc99ff",
        DependencyGraphVertexKind.Entity: "#ffff99",
        DependencyGraphVertexKind.Architecture: "#ff99cc",
        DependencyGraphVertexKind.Configuration: "#ff9900",
    }
    EDGE_COLORS = {
        DependencyGraphEdgeKind.SourceFile: "#000000",
        DependencyGraphEdgeKind.CompileOrder: "#ff0000",
        DependencyGraphEdgeKind.LibraryClause: "#000000",
        DependencyGraphEdgeKind.UseClause: "#000000",
        DependencyGraphEdgeKind.ContextReference: "#000000",
        DependencyGraphEdgeKind.EntityImplementation: "#99ccff",
        DependencyGraphEdgeKind.PackageImplementation: "#99ccff",
        DependencyGraphEdgeKind.EntityInstantiation: "#000000",
        DependencyGraphEdgeKind.ComponentInstantiation: "#000000",
        DependencyGraphEdgeKind.ConfigurationInstantiation: "#000000",
    }

    def WriteGraphML(self, path: Path):
        with path.open("w") as file:
            file.write(
                dedent(
                    f"""\
            <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">
              <key id="nd1" for="node" attr.name="id" attr.type="string"/>
              <key id="nd2" for="node" attr.name="value" attr.type="string"/>
              <key id="nd3" for="node" attr.name="kind" attr.type="string"/>
              <key id="nd4" for="node" attr.name="color" attr.type="string"/>

              <key id="ed3" for="edge" attr.name="kind" attr.type="string"/>
              <key id="ed4" for="edge" attr.name="color" attr.type="string"/>
              <graph id="CompileOrderGraph"
                     edgedefault="directed"
                     parse.nodes="{len(self._graph._verticesWithID)}"
                     parse.edges="{len(self._graph._edgesWithoutID)}"
                     parse.order="nodesfirst">
            """
                )
            )

            for vertex in self._graph._verticesWithID.values():
                if vertex["kind"] is DependencyGraphVertexKind.Document:
                    file.write(
                        dedent(
                            """\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Path.name}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """
                        ).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]])
                    )

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(
                    dedent(
                        """\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """
                    ).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]])
                )
                edgeCount += 1

            file.write(
                dedent(
                    """\
              </graph>
            </graphml>
            """
                )
            )
