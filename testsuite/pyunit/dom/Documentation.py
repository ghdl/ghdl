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
# Testsuite:        Check which DOM element receives a documentation comment.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2026 Tristan Gingold
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
from inspect import currentframe
from pathlib import Path
from textwrap import dedent
from unittest import TestCase, expectedFailure

from pyVHDLModel import VHDLVersion

from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class InterfaceDocumentation(TestCase):
    """Documentation comments of generics and ports: before, on the same line, continued, and on identifier lists."""

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/InterfaceDocumentation.vhdl"

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()
        cls._document = Document(cls._filename)
        cls._design.Documents.append(cls._document)
        cls._entity = cls._document.Entities["interfacedocumentation"]

    def test_Entity(self) -> None:
        self.assertEqual("", self._entity.Documentation)

    def test_Generics(self) -> None:
        expected = [
            (("G1",), "--! Generic before."),
            (("G2",), "--! Generic same line."),
            (("G3", "G4"), "--! Generic list."),
        ]
        self.assertEqual(len(expected), len(self._entity.GenericItems))
        for generic, (identifiers, documentation) in zip(self._entity.GenericItems, expected):
            with self.subTest(generic=identifiers):
                self.assertEqual(identifiers, generic.Identifiers)
                self.assertEqual(documentation, generic.Documentation)

    def test_Ports(self) -> None:
        expected = [
            (("Clock",), "--! Port before."),
            (("Reset",), "--! Port same line.\n--! Port continuation."),
            (("A", "B"), "--! Port list."),
            (("Q",), "--! Last port, no semicolon."),
        ]
        self.assertEqual(len(expected), len(self._entity.PortItems))
        for port, (identifiers, documentation) in zip(self._entity.PortItems, expected):
            with self.subTest(port=identifiers):
                self.assertEqual(identifiers, port.Identifiers)
                self.assertEqual(documentation, port.Documentation)


class DeclarationDocumentation(TestCase):
    """Documentation comments of declarations in a package, a package body, an architecture and a process."""

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/DeclarationDocumentation.vhdl"

    @classmethod
    def setUpClass(cls) -> None:
        cls._design = Design()
        cls._document = Document(cls._filename)
        cls._design.Documents.append(cls._document)
        cls._package = cls._document.Packages["declarationdocumentation"]
        cls._packageBody = cls._document.PackageBodies["declarationdocumentation"]
        cls._architecture = cls._document.Architectures["interfacedocumentation"]["rtl"]

    def test_Constants(self) -> None:
        c1, c2 = self._package.DeclaredItems[0:2]

        self.assertEqual("--! Constant before.", c1.Documentation)
        self.assertEqual("--! Constant same line.", c2.Documentation)

    def test_FunctionAndParameters(self) -> None:
        function = self._package.DeclaredItems[2]

        self.assertEqual("F", function.Identifier)
        self.assertEqual("--! Function before.", function.Documentation)

        expected = [
            (("X",), "--! Parameter before."),
            (("Y",), "--! Parameter same line."),
            (("Z",), ""),
        ]
        self.assertEqual(len(expected), len(function.ParameterItems))
        for parameter, (identifiers, documentation) in zip(function.ParameterItems, expected):
            with self.subTest(parameter=identifiers):
                self.assertEqual(identifiers, parameter.Identifiers)
                self.assertEqual(documentation, parameter.Documentation)

    def test_Procedures(self) -> None:
        p, q = self._package.DeclaredItems[3:5]

        self.assertEqual("--! Procedure before.", p.Documentation)
        self.assertEqual("--! Procedure same line, no parameters.", q.Documentation)

    def test_RecordAndElements(self) -> None:
        record = self._package.DeclaredItems[5]

        self.assertEqual("", record.Documentation)
        a, b = record.Elements
        self.assertEqual("--! Element before.", a.Documentation)
        self.assertEqual("--! Element same line.", b.Documentation)

    def test_FunctionBody(self) -> None:
        function = self._packageBody.DeclaredItems[0]

        self.assertEqual("F", function.Identifier)
        self.assertEqual("--! Body of F.", function.Documentation)

    def test_Architecture(self) -> None:
        self.assertEqual("--! Architecture inside.", self._architecture.Documentation)

    def test_Signals(self) -> None:
        expected = [
            (("S1",), "--! Signal before."),
            (("S2",), "--! Signal same line."),
            (("S3", "S4"), "--! Signal list."),
        ]
        self.assertEqual(len(expected), len(self._architecture.DeclaredItems))
        for signal, (identifiers, documentation) in zip(self._architecture.DeclaredItems, expected):
            with self.subTest(signal=identifiers):
                self.assertEqual(identifiers, signal.Identifiers)
                self.assertEqual(documentation, signal.Documentation)

    def test_ProcessAndVariable(self) -> None:
        process = self._architecture.Statements[0]

        self.assertEqual("--! Process before.", process.Documentation)
        self.assertEqual("--! Variable before.", process.DeclaredItems[0].Documentation)


class InMemoryDocumentation(TestCase):
    """Base class: parse a source string as a document of its own, named after the calling test."""

    _root = Path(__file__).resolve().parent

    @classmethod
    def setUpClass(cls) -> None:
        # Documents must stay referenced as long as libghdl holds their nodes.
        cls._design = Design(vhdlVersion=VHDLVersion.VHDL2019)

    def parse(self, sourceCode: str) -> Document:
        testName = currentframe().f_back.f_code.co_name[5:]
        filename = self._root / f"{self.__class__.__name__}_{testName}.vhdl"

        document = Document(filename, dedent(sourceCode), vhdlVersion=VHDLVersion.VHDL2019)
        self._design.Documents.append(document)

        return document


class DesignUnitDocumentation(InMemoryDocumentation):
    """
    Comments before a design unit's first token belong to libghdl's design unit node. Only the first unit of a file has
    a file header; all other comments of a design unit node document the unit.
    """

    def test_FirstUnitBefore(self) -> None:
        document = self.parse("""\
            --! Header.

            --! E doc.
            entity e is
            end entity;
            """)

        self.assertEqual("--! Header.", document.Documentation)
        self.assertEqual("--! E doc.", document.Entities["e"].Documentation)

    def test_FirstUnitBeforeWithoutHeader(self) -> None:
        document = self.parse("""\
            --! E doc 1.
            --
            --! E doc 2.
            entity e is
            end entity;
            """)

        self.assertEqual("", document.Documentation)
        self.assertEqual("--! E doc 1.\n--\n--! E doc 2.", document.Entities["e"].Documentation)

    def test_FirstUnitHeaderBlocks(self) -> None:
        document = self.parse("""\
            -- Header 1.

            -- Header 2.

            --! E doc.
            entity e is  --! E inside.
            end entity;
            """)

        self.assertEqual("-- Header 1.\n-- Header 2.", document.Documentation)
        self.assertEqual("--! E doc.\n--! E inside.", document.Entities["e"].Documentation)

    def test_HeaderBeforeContextClause(self) -> None:
        document = self.parse("""\
            --! Header.
            library ieee;
            use     ieee.std_logic_1164.all;

            --! E doc.
            entity e is
            end entity;
            """)

        self.assertEqual("--! Header.", document.Documentation)
        self.assertEqual("--! E doc.", document.Entities["e"].Documentation)

    def test_AfterContextClauseBlankLine(self) -> None:
        document = self.parse("""\
            --! Header.
            library ieee;
            --! E doc.

            entity e is
            end entity;
            """)

        self.assertEqual("--! Header.", document.Documentation)
        self.assertEqual("--! E doc.", document.Entities["e"].Documentation)

    def test_SecondUnitBefore(self) -> None:
        document = self.parse("""\
            entity e is
            end entity;

            --! A doc.
            architecture a of e is
            begin
            end architecture;
            """)

        self.assertEqual("", document.Documentation)
        self.assertEqual("", document.Entities["e"].Documentation)
        self.assertEqual("--! A doc.", document.Architectures["e"]["a"].Documentation)

    def test_EveryUnitKind(self) -> None:
        document = self.parse("""\
            --! Header.

            --! E doc.
            entity e is
            end entity;

            --! A doc.
            architecture a of e is
            begin
            end architecture;

            --! P doc.
            package p is
            end package;

            --! PB doc.
            package body p is
            end package body;

            --! C doc.
            context c is
            end context;

            --! CFG doc.
            configuration cfg of e is
              for a
              end for;
            end configuration;
            """)

        self.assertEqual("--! Header.", document.Documentation)
        self.assertEqual("--! E doc.", document.Entities["e"].Documentation)
        self.assertEqual("--! A doc.", document.Architectures["e"]["a"].Documentation)
        self.assertEqual("--! P doc.", document.Packages["p"].Documentation)
        self.assertEqual("--! PB doc.", document.PackageBodies["p"].Documentation)
        self.assertEqual("--! C doc.", document.Contexts["c"].Documentation)
        self.assertEqual("--! CFG doc.", document.Configurations["cfg"].Documentation)


class StatementDocumentation(InMemoryDocumentation):
    """A process statement carries its own comment."""

    def test_Processes(self) -> None:
        document = self.parse("""\
            entity e is
            end entity;

            architecture a of e is
              signal s : bit;
            begin
              --! P doc.
              p : process
              begin
                wait;
              end process;

              --! Unlabelled doc.
              process
              begin
                wait;
              end process;

              q : process (s)
                --! Q doc.

              begin
              end process;
            end architecture;
            """)
        p, unlabelled, q = document.Architectures["e"]["a"].Statements

        self.assertEqual("--! P doc.", p.Documentation)
        self.assertEqual("--! Unlabelled doc.", unlabelled.Documentation)
        self.assertEqual("--! Q doc.", q.Documentation)


class ObjectDocumentation(InMemoryDocumentation):
    """libghdl attaches a same-line comment of ``signal s, t : bit;`` to ``t``: the DOM reads every identifier."""

    def test_SignalListSameLine(self) -> None:
        document = self.parse("""\
            entity e is
            end entity;

            architecture a of e is
              signal s, t : bit;  --! ST doc.

              constant c, d : bit := '0';  --! CD doc.

              shared variable v, w : integer;  --! VW doc.
            begin
            end architecture;
            """)
        signal, constant, variable = document.Architectures["e"]["a"].DeclaredItems

        self.assertEqual(("s", "t"), signal.Identifiers)
        self.assertEqual("--! ST doc.", signal.Documentation)
        self.assertEqual("--! CD doc.", constant.Documentation)
        self.assertEqual("--! VW doc.", variable.Documentation)

    def test_SignalListBeforeAndSameLine(self) -> None:
        document = self.parse("""\
            entity e is
            end entity;

            architecture a of e is
              --! Before.
              signal s, t : bit;  --! Same line.

            begin
            end architecture;
            """)
        signal = document.Architectures["e"]["a"].DeclaredItems[0]

        self.assertEqual("--! Before.\n--! Same line.", signal.Documentation)


class GHDLLimitations(InMemoryDocumentation):
    """
    Documentation comments lost or misassigned by libghdl's comment association (``file_comments``, gathering calls in
    ``vhdl-parse.adb``). Each test states the expected result and fails until GHDL gathers the comment.
    """

    @expectedFailure
    def test_TrailingThenNextUnit(self) -> None:
        """GHDL: a same-line comment continues over code tokens and swallows the next unit's comment."""
        document = self.parse("""\
            entity e is
              port (
                q : out bit  --! Q doc.
              );
            end entity;

            --! A doc.
            architecture a of e is
            begin
            end architecture;
            """)

        self.assertEqual("--! Q doc.", document.Entities["e"].PortItems[0].Documentation)
        self.assertEqual("--! A doc.", document.Architectures["e"]["a"].Documentation)

    @expectedFailure
    def test_SameLineThenBefore(self) -> None:
        """GHDL: a same-line comment continues into the next line's comment."""
        document = self.parse("""\
            entity e is
              generic (
                G : natural := 1;  --! G doc.
                --! H doc.
                H : natural := 2
              );
            end entity;
            """)
        g, h = document.Entities["e"].GenericItems

        self.assertEqual("--! G doc.", g.Documentation)
        self.assertEqual("--! H doc.", h.Documentation)

    @expectedFailure
    def test_PackageBodyInside(self) -> None:
        """GHDL: a package body gathers no comment."""
        document = self.parse("""\
            package p is
            end package;

            package body p is
              --! Body doc.
            end package body;
            """)

        self.assertEqual("--! Body doc.", document.PackageBodies["p"].Documentation)

    @expectedFailure
    def test_Component(self) -> None:
        """GHDL: a component gathers no comment, its first port receives it."""
        document = self.parse("""\
            package p is
              --! C doc.
              component c is
                port (x : in bit);
              end component;
            end package;
            """)
        component = document.Packages["p"].DeclaredItems[0]

        self.assertEqual("--! C doc.", component.Documentation)
        self.assertEqual("", component.PortItems[0].Documentation)

    @expectedFailure
    def test_AliasAttributeSubtype(self) -> None:
        """GHDL: alias, attribute declaration and subtype gather no comment, the next declaration receives it."""
        document = self.parse("""\
            package p is
              constant c : bit := '0';
              --! Alias doc.
              alias a is c;
              constant d1 : bit := '0';
              --! Attribute doc.
              attribute at : bit;
              constant d2 : bit := '0';
              --! Subtype doc.
              subtype st is bit;
              constant d3 : bit := '0';
            end package;
            """)
        _, alias, d1, attribute, d2, subtype, d3 = document.Packages["p"].DeclaredItems

        self.assertEqual("--! Alias doc.", alias.Documentation)
        self.assertEqual("--! Attribute doc.", attribute.Documentation)
        self.assertEqual("--! Subtype doc.", subtype.Documentation)
        for constant in (d1, d2, d3):
            self.assertEqual("", constant.Documentation)

    @expectedFailure
    def test_RangeTypes(self) -> None:
        """GHDL: integer, physical and incomplete type declarations gather no comment."""
        document = self.parse("""\
            package p is
              --! Integer doc.
              type it is range 0 to 7;
              constant d1 : bit := '0';
              --! Physical doc.
              type pt is range 0 to 1000
                units
                  u;
                end units;
              constant d2 : bit := '0';
              --! Incomplete doc.
              type inc;
              constant d3 : bit := '0';
              type inc is access bit;
            end package;
            """)
        integerType, d1, physicalType, d2, incompleteType, d3, _ = document.Packages["p"].DeclaredItems

        self.assertEqual("--! Integer doc.", integerType.Documentation)
        self.assertEqual("--! Physical doc.", physicalType.Documentation)
        self.assertEqual("--! Incomplete doc.", incompleteType.Documentation)
        for constant in (d1, d2, d3):
            self.assertEqual("", constant.Documentation)

    @expectedFailure
    def test_ProtectedType(self) -> None:
        """GHDL: a protected type gathers no comment, its first method receives it."""
        document = self.parse("""\
            package p is
              --! PT doc.
              type pt is protected
                procedure m;
              end protected;
            end package;
            """)
        protectedType = document.Packages["p"].DeclaredItems[0]

        self.assertEqual("--! PT doc.", protectedType.Documentation)
        self.assertEqual("", protectedType.Methods[0].Documentation)

    @expectedFailure
    def test_ModeView(self) -> None:
        """GHDL: a mode view's comment goes to its first element."""
        document = self.parse("""\
            package p is
              type r is record
                a : bit;
              end record;

              --! MV doc.
              view mv of r is
                a : in;
              end view;
            end package;
            """)
        modeView = document.Packages["p"].DeclaredItems[1]

        self.assertEqual("--! MV doc.", modeView.Documentation)
        self.assertEqual("", modeView.Elements[0].Documentation)

    @expectedFailure
    def test_PortView(self) -> None:
        """GHDL: a port with a mode view is re-created; the comment stays on the freed node."""
        document = self.parse("""\
            entity e is
              port (
                --! V doc.
                v : view work.p.mv
              );
            end entity;
            """)

        self.assertEqual("--! V doc.", document.Entities["e"].PortItems[0].Documentation)

    @expectedFailure
    def test_GenericTypeSubprogramPackage(self) -> None:
        """GHDL: generic types, subprograms and packages gather no comment."""
        document = self.parse("""\
            entity e is
              generic (
                --! T doc.
                type T;
                --! F doc.
                function f return bit;
                --! GP doc.
                package gp is new work.q generic map (<>)
              );
            end entity;
            """)
        genericType, genericFunction, genericPackage = document.Entities["e"].GenericItems

        self.assertEqual("--! T doc.", genericType.Documentation)
        self.assertEqual("--! F doc.", genericFunction.Documentation)
        self.assertEqual("--! GP doc.", genericPackage.Documentation)

    @expectedFailure
    def test_SubprogramInstantiation(self) -> None:
        """GHDL: a subprogram instantiation frees the node the comment is attached to."""
        document = self.parse("""\
            package p is
              --! FI doc.
              function fi is new work.q.f generic map (T => bit);
            end package;
            """)

        self.assertEqual("--! FI doc.", document.Packages["p"].DeclaredItems[0].Documentation)

    @expectedFailure
    def test_FunctionSameLine(self) -> None:
        """GHDL: a function's same-line comment goes to its last parameter."""
        document = self.parse("""\
            package p is
              function f(x : bit) return bit;  --! F doc.

            end package;
            """)
        function = document.Packages["p"].DeclaredItems[0]

        self.assertEqual("--! F doc.", function.Documentation)
        self.assertEqual("", function.ParameterItems[0].Documentation)

    @expectedFailure
    def test_EnumSameLine(self) -> None:
        """GHDL: an enumerated type's same-line comment goes to its last literal."""
        document = self.parse("""\
            package p is
              type t is (a, b);  --! T doc.

            end package;
            """)

        self.assertEqual("--! T doc.", document.Packages["p"].DeclaredItems[0].Documentation)

    @expectedFailure
    def test_BlockComment(self) -> None:
        """GHDL: ``/* */`` comments are never gathered."""
        document = self.parse("""\
            /* E doc. */
            entity e is
              port (
                /* X doc. */
                x : in bit
              );
            end entity;
            """)
        entity = document.Entities["e"]

        self.assertEqual("/* E doc. */", entity.Documentation)
        self.assertEqual("/* X doc. */", entity.PortItems[0].Documentation)

    @expectedFailure
    def test_BlockStatement(self) -> None:
        """GHDL: a block statement gathers no comment."""
        document = self.parse("""\
            entity e is
            end entity;

            architecture a of e is
            begin
              --! B doc.
              b : block
              begin
              end block;
            end architecture;
            """)

        self.assertEqual("--! B doc.", document.Architectures["e"]["a"].Statements[0].Documentation)
