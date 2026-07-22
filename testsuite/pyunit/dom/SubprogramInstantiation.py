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
# Testsuite:        Check libghdl IIR translation of subprogram instantiations.
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
from pyGHDL.dom.Subprogram import FunctionInstantiation, ProcedureInstantiation


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class SubprogramInstantiations(TestCase):
    """
    Checks translation of VHDL-2008 subprogram instantiations (generic subprograms):
    Function_Instantiation_Declaration and Procedure_Instantiation_Declaration.

    FunctionInstantiation/ProcedureInstantiation were previously bare stub classes with no way to
    populate SubprogramReference or GenericAssociationItems at all.
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/SubprogramInstantiation.vhdl"

    @staticmethod
    def _package():
        design = Design()
        document = Document(SubprogramInstantiations._filename)
        design.Documents.append(document)
        return document.Packages["instances"]

    def test_FunctionInstantiation(self) -> None:
        pkg = self._package()
        functionInstantiation = pkg.DeclaredItems[0]

        self.assertIsInstance(functionInstantiation, FunctionInstantiation)
        self.assertEqual("add_int", functionInstantiation.Identifier)
        self.assertIsNotNone(functionInstantiation.SubprogramReference)
        self.assertEqual(1, len(functionInstantiation.GenericAssociationItems))
        # ReturnType cannot be resolved without semantic analysis - see class docstring.
        self.assertIsNone(functionInstantiation.ReturnType)

    def test_ProcedureInstantiation(self) -> None:
        pkg = self._package()
        procedureInstantiation = pkg.DeclaredItems[1]

        self.assertIsInstance(procedureInstantiation, ProcedureInstantiation)
        self.assertEqual("proc_int", procedureInstantiation.Identifier)
        self.assertIsNotNone(procedureInstantiation.SubprogramReference)
        self.assertEqual(1, len(procedureInstantiation.GenericAssociationItems))
