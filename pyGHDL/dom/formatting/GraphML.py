"""
This module offers formatters to write a design's graphs as `GraphML <http://graphml.graphdrawing.org>`__ files.
"""

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
    """
    Abstract base-class for writing a pyVHDLModel graph as a `GraphML <http://graphml.graphdrawing.org>`__ file.

    A derived formatter provides the two color translation dictionaries and implements
    :meth:`WriteGraphML` for the graph it can render.
    """

    _graph: Graph  #: The graph to be written as GraphML.

    #: Translation dictionary of *vertex kind* to *fill color*; set by each derived formatter.
    NODE_COLORS: ClassVar[Dict[Flag, str]]
    #: Translation dictionary of *edge kind* to *line color*; set by each derived formatter.
    EDGE_COLORS: ClassVar[Dict[Flag, str]]

    def __init__(self, graph: Graph) -> None:
        """
        Initializes a GraphML formatter for the given graph.

        :param graph: The graph to be written as GraphML by :meth:`WriteGraphML`.
        """
        self._graph = graph

    @abstractmethod
    def WriteGraphML(self, path: Path):
        """
        Writes the graph as a GraphML file.

        :param path: The path of the file to write.
        """
        pass


@export
class DependencyGraphFormatter(Formatter):
    """
    Writes a design's *dependency graph* as GraphML, grouping the vertices by library.
    """

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
        DependencyGraphEdgeKind.PackageInstantiation: "#000000",
    }

    def WriteGraphML(self, path: Path):
        """
        Writes the dependency graph as a GraphML file, grouping the vertices by library.

        The document declares six `GraphML <http://graphml.graphdrawing.org>`__ keys, four for nodes and two for edges:

        .. list-table::
           :header-rows: 1
           :widths: 8 8 12 72

           * - Key
             - Applies to
             - Attribute
             - Value
           * - ``nd1``
             - node
             - ``id``
             - The vertex' ID, repeated as data so a reader ignoring the XML ``id`` still sees it.
           * - ``nd2``
             - node
             - ``value``
             - The label to display. An architecture is written as ``entity(architecture)``, a document as its ID,
               anything else as its identifier.
           * - ``nd3``
             - node
             - ``kind``
             - The name of the vertex' :class:`~pyVHDLModel.DependencyGraphVertexKind`.
           * - ``nd4``
             - node
             - ``color``
             - The fill color from :attr:`NODE_COLORS`, selected by vertex kind.
           * - ``ed3``
             - edge
             - ``kind``
             - The name of the edge's :class:`~pyVHDLModel.DependencyGraphEdgeKind`.
           * - ``ed4``
             - edge
             - ``color``
             - The line color from :attr:`EDGE_COLORS`, selected by edge kind.

        A node's XML ``id`` is the vertex' ID from the model. An edge has none, so edges are numbered in the order they
        are written and get ``e1``, ``e2``, ...; their ``source`` and ``target`` are vertex IDs.

        The graph is **grouped into one subgraph per library**: an enclosing node ``grp_<library>`` holds a nested
        ``<graph id="<library>">`` with that library's vertices. A vertex is assigned to a library by asking the model -
        a library vertex for itself, a document vertex through its first design unit, anything else through its own
        ``Library``. Edges are written afterwards at the top level, so an edge may cross from one subgraph into another.

        :param path: The path of the file to write.
        """
        with path.open("w") as file:
            file.write(dedent(f"""\
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
            """))
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
                file.write(dedent("""\
                    {prefix}<node id="grp_{id}">
                    {prefix}  <data key="nd2">{value}</data>
                    {prefix}  <graph id="{id}" edgedefault="directed">
                """).format(prefix="    ", id=group, value=group))

                for vertex in vertices:
                    if vertex["kind"] is DependencyGraphVertexKind.Architecture:
                        value = f"{vertex.Value.Entity.Name.Identifier}({vertex.Value.Identifier})"
                    elif vertex["kind"] is DependencyGraphVertexKind.Document:
                        value = f"{vertex.ID}"
                    else:
                        value = f"{vertex.Value.Identifier}"
                    file.write(dedent("""\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{value}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """).format(prefix="        ", vertex=vertex, value=value, color=self.NODE_COLORS[vertex["kind"]]))

                file.write(dedent("""\
                    {prefix}  </graph>
                    {prefix}</node>
                """).format(prefix="    "))

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(dedent("""\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]]))
                edgeCount += 1

            file.write(dedent("""\
              </graph>
            </graphml>
            """))


@export
class HierarchyGraphFormatter(Formatter):
    """
    Writes a design's *hierarchy graph* as GraphML.
    """

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
        """
        Writes the hierarchy graph as a GraphML file.

        The document declares six `GraphML <http://graphml.graphdrawing.org>`__ keys, four for nodes and two for edges:

        .. list-table::
           :header-rows: 1
           :widths: 8 8 12 72

           * - Key
             - Applies to
             - Attribute
             - Value
           * - ``nd1``
             - node
             - ``id``
             - The vertex' ID, repeated as data so a reader ignoring the XML ``id`` still sees it.
           * - ``nd2``
             - node
             - ``value``
             - The label to display. The vertex' identifier.
           * - ``nd3``
             - node
             - ``kind``
             - The name of the vertex' :class:`~pyVHDLModel.DependencyGraphVertexKind`.
           * - ``nd4``
             - node
             - ``color``
             - The fill color from :attr:`NODE_COLORS`, selected by vertex kind.
           * - ``ed3``
             - edge
             - ``kind``
             - The name of the edge's :class:`~pyVHDLModel.DependencyGraphEdgeKind`.
           * - ``ed4``
             - edge
             - ``color``
             - The line color from :attr:`EDGE_COLORS`, selected by edge kind.

        A node's XML ``id`` is the vertex' ID from the model. An edge has none, so edges are numbered in the order they
        are written and get ``e1``, ``e2``, ...; their ``source`` and ``target`` are vertex IDs.

        The vertices are written flat, without subgraphs.

        :param path: The path of the file to write.
        """
        with path.open("w") as file:
            file.write(dedent(f"""\
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
            """))

            for vertex in self._graph._verticesWithID.values():
                if vertex["kind"] is DependencyGraphVertexKind.Entity:
                    file.write(dedent("""\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Identifier}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]]))
                elif vertex["kind"] is DependencyGraphVertexKind.Architecture:
                    file.write(dedent("""\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Identifier}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]]))

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(dedent("""\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]]))
                edgeCount += 1

            file.write(dedent("""\
              </graph>
            </graphml>
            """))


@export
class CompileOrderGraphFormatter(Formatter):
    """
    Writes a design's *compile order graph* as GraphML.
    """

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
        """
        Writes the compile order graph as a GraphML file.

        The document declares six `GraphML <http://graphml.graphdrawing.org>`__ keys, four for nodes and two for edges:

        .. list-table::
           :header-rows: 1
           :widths: 8 8 12 72

           * - Key
             - Applies to
             - Attribute
             - Value
           * - ``nd1``
             - node
             - ``id``
             - The vertex' ID, repeated as data so a reader ignoring the XML ``id`` still sees it.
           * - ``nd2``
             - node
             - ``value``
             - The label to display. The vertex' identifier.
           * - ``nd3``
             - node
             - ``kind``
             - The name of the vertex' :class:`~pyVHDLModel.DependencyGraphVertexKind`.
           * - ``nd4``
             - node
             - ``color``
             - The fill color from :attr:`NODE_COLORS`, selected by vertex kind.
           * - ``ed3``
             - edge
             - ``kind``
             - The name of the edge's :class:`~pyVHDLModel.DependencyGraphEdgeKind`.
           * - ``ed4``
             - edge
             - ``color``
             - The line color from :attr:`EDGE_COLORS`, selected by edge kind.

        A node's XML ``id`` is the vertex' ID from the model. An edge has none, so edges are numbered in the order they
        are written and get ``e1``, ``e2``, ...; their ``source`` and ``target`` are vertex IDs.

        The vertices are written flat, without subgraphs.

        :param path: The path of the file to write.
        """
        with path.open("w") as file:
            file.write(dedent(f"""\
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
            """))

            for vertex in self._graph._verticesWithID.values():
                if vertex["kind"] is DependencyGraphVertexKind.Document:
                    file.write(dedent("""\
                        {prefix}<node id="{vertex.ID}">
                        {prefix}  <data key="nd1">{vertex.ID}</data>
                        {prefix}  <data key="nd2">{vertex.Value.Path.name}</data>
                        {prefix}  <data key="nd3">{vertex[kind].name}</data>
                        {prefix}  <data key="nd4">{color}</data>
                        {prefix}</node>
                    """).format(prefix="    ", vertex=vertex, color=self.NODE_COLORS[vertex["kind"]]))

            edgeCount = 1
            for edge in self._graph._edgesWithoutID:
                file.write(dedent("""\
                    {prefix}<edge id="e{edgeCount}" source="{edge.Source.ID}" target="{edge.Destination.ID}">
                    {prefix}  <data key="ed3">{edge[kind].name}</data>
                    {prefix}  <data key="ed4">{color}</data>
                    {prefix}</edge>
                """).format(prefix="    ", edgeCount=edgeCount, edge=edge, color=self.EDGE_COLORS[edge["kind"]]))
                edgeCount += 1

            file.write(dedent("""\
              </graph>
            </graphml>
            """))
