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
# Testsuite:        Check libghdl IIR translation of subprogram declarations.
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
from pathlib import Path
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.Subprogram import Function, Procedure
from pyGHDL.dom.InterfaceItem import GenericFunctionInterfaceItem, GenericProcedureInterfaceItem


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Subprograms(TestCase):
    """
    Regression tests: Function.ReturnType previously crashed with AttributeError on every instance
    (pyVHDLModel's Function.__init__ never set self._returnType, and Subprogram.__init__ didn't
    accept genericItems/parameterItems at all - pyGHDL.dom had to reach into private fields
    directly to work around it). Also, Function's IsPure was never read from the source at all
    (always defaulted to True, even for 'impure function').
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Subprograms.vhdl"

    def test_PureFunction(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        function = architecture.DeclaredItems[0]

        self.assertIsInstance(function, Function)
        self.assertEqual("double", function.Identifier)
        self.assertIsNotNone(function.ReturnType)
        self.assertTrue(function.IsPure)
        self.assertEqual(1, len(function.ParameterItems))

    def test_ImpureFunction(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        function = architecture.DeclaredItems[1]

        self.assertIsInstance(function, Function)
        self.assertEqual("get_random", function.Identifier)
        self.assertIsNotNone(function.ReturnType)
        self.assertFalse(function.IsPure)

    def test_Procedure(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        procedure = architecture.DeclaredItems[3]

        self.assertIsInstance(procedure, Procedure)
        self.assertEqual("log", procedure.Identifier)
        self.assertEqual(2, len(procedure.ParameterItems))


class SubprogramBodies(TestCase):
    """
    Regression tests: Function/Procedure bodies (local declarations and sequential statements) were
    previously never translated at all - Function.parse()/Procedure.parse() only ever read the
    specification (name, generics, parameters, return type), never the paired
    Function_Body/Procedure_Body node. Every Function/Procedure object always had empty
    DeclaredItems/Statements regardless of what the body actually contained.

    Also covers ReturnStatement, which previously crashed on .ReturnValue access
    (pyVHDLModel's ReturnStatement misused ConditionalMixin, which sets self._condition, not
    self._returnValue) and had no way to carry a label at all.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Subprograms.vhdl"

    def test_SimpleReturnStatement(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        function = architecture.DeclaredItems[0]

        self.assertEqual(1, len(function.Statements))
        returnStatement = function.Statements[0]
        self.assertIsNotNone(returnStatement.ReturnValue)

    def test_DeclaredItemsAndReturnValue(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        function = architecture.DeclaredItems[2]

        self.assertEqual("scale", function.Identifier)
        self.assertEqual(2, len(function.DeclaredItems))
        self.assertEqual(("FACTOR",), function.DeclaredItems[0].Identifiers)
        self.assertEqual(("result",), function.DeclaredItems[1].Identifiers)

        self.assertEqual(1, len(function.Statements))
        self.assertIsNotNone(function.Statements[0].ReturnValue)

    def test_EmptyProcedureBody(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        architecture = document.Architectures["subprograms"]["rtl"]
        procedure = architecture.DeclaredItems[3]

        self.assertEqual(0, len(procedure.DeclaredItems))
        self.assertEqual(0, len(procedure.Statements))


class GenericSubprograms(TestCase):
    """
    Regression test (HIGH PRIORITY, confirmed live): ``GenericFunctionInterfaceItem`` had no
    ``returnType`` parameter of its own in pyVHDLModel, so ``GenericFunctionInterfaceItem.parse()``
    crashed with ``AttributeError`` on any real ``generic (function f return t);`` clause - the most
    common shape of a VHDL-2008 generic subprogram interface item. Fixed on both sides: pyVHDLModel's
    ``GenericFunctionInterfaceItem.__init__`` now accepts ``returnType``, and this class's
    ``.parse()`` now reads it off the IIR node via ``Get_Return_Type_Mark`` (mirroring
    ``Function.parse()``) instead of never fetching it at all.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/GenericSubprograms.vhdl"

    def test_GenericFunctionInterfaceItem(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        package = document.Packages["genericsubprograms"]
        generic = package.GenericItems[0]

        self.assertIsInstance(generic, GenericFunctionInterfaceItem)
        self.assertEqual("compare", generic.Identifier)
        self.assertEqual("boolean", generic.ReturnType.Name.Identifier)

    def test_GenericProcedureInterfaceItem(self):
        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        package = document.Packages["genericsubprograms"]
        generic = package.GenericItems[1]

        self.assertIsInstance(generic, GenericProcedureInterfaceItem)
        self.assertEqual("log", generic.Identifier)
