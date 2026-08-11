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
# Testsuite:        Check libghdl IIR translation of subprogram declarations without a body.
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


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class SubprogramsWithoutBodies(TestCase):
    """
    Regression tests: a subprogram declaration without a body (e.g. a forward declaration in a
    package spec whose body lives in a separate package body) was previously silently dropped
    entirely from the declared items ('WarningCollector.Raise(NotImplementedError(...))', no yield
    at all) - the declaration simply vanished from the model, even though it's perfectly valid,
    common VHDL.

    Function.parse()/Procedure.parse() already handled the no-body case gracefully on their own
    (DeclaredItems/Statements come back empty, everything else populated correctly) - the fix was to
    stop special-casing the has-no-body case in the dispatcher and just always call them.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/SubprogramsWithoutBodies.vhdl"

    @staticmethod
    def _document():
        design = Design()
        document = Document(SubprogramsWithoutBodies._filename)
        design.Documents.append(document)
        return document

    def test_FunctionDeclarationWithoutBody(self) -> None:
        document = self._document()
        pkg = document.Packages["subprogramswithoutbodies"]
        function = pkg.DeclaredItems[0]

        self.assertIsInstance(function, Function)
        self.assertEqual("foo", function.Identifier)
        self.assertIsNotNone(function.ReturnType)
        self.assertEqual(0, len(function.DeclaredItems))
        self.assertEqual(0, len(function.Statements))

    def test_ProcedureDeclarationWithoutBody(self) -> None:
        document = self._document()
        pkg = document.Packages["subprogramswithoutbodies"]
        procedure = pkg.DeclaredItems[1]

        self.assertIsInstance(procedure, Procedure)
        self.assertEqual("bar", procedure.Identifier)
        self.assertEqual(0, len(procedure.DeclaredItems))
        self.assertEqual(0, len(procedure.Statements))

    def test_FunctionWithBodyStillWorks(self) -> None:
        """Confirms the with-body case in the package body still works correctly (no regression,
        no double-yielding via the separate Function_Body chain item)."""
        document = self._document()
        pkgBody = document.PackageBodies["subprogramswithoutbodies"]
        function = pkgBody.DeclaredItems[0]

        self.assertIsInstance(function, Function)
        self.assertEqual("foo", function.Identifier)
        self.assertEqual(1, len(function.Statements))

    def test_ProcedureWithBodyStillWorks(self) -> None:
        document = self._document()
        pkgBody = document.PackageBodies["subprogramswithoutbodies"]
        procedure = pkgBody.DeclaredItems[1]

        self.assertIsInstance(procedure, Procedure)
        self.assertEqual("bar", procedure.Identifier)
