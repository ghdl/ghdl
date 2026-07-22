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
# Testsuite:        Check libghdl IIR translation of configurations.
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
from pyGHDL.dom.Configuration import (
    EntityAspectEntity, EntityAspectConfiguration, EntityAspectOpen,
    ComponentConfiguration, BlockConfiguration, OthersInstantiationList,
)


if __name__ == "__main__":  # pragma: no cover
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Configurations(TestCase):
    """
    Checks translation of configuration declarations, block configurations, component
    configurations, configuration specifications, and all three entity aspect forms
    (entity/configuration/open), plus the 'others' instantiation list form.

    Configuration was previously a bare shell - no entity being configured, no block configuration
    content at all ('# FIXME: read specifications').
    """

    _root = Path(__file__).resolve().parent
    _filename: Path = _root / "examples/Configurations.vhdl"

    @staticmethod
    def _document():
        design = Design()
        document = Document(Configurations._filename)
        design.Documents.append(document)
        return document

    def test_ConfigurationDeclaration(self) -> None:
        document = self._document()
        cfg = document.Configurations["cfg"]

        self.assertEqual("Cfg", cfg.Identifier)
        self.assertIsNotNone(cfg.Entity)

        block = cfg.BlockConfiguration
        self.assertIsInstance(block, BlockConfiguration)
        self.assertIsNotNone(block.BlockSpecification)
        self.assertEqual(3, len(block.Items))

    def test_EntityAspectConfiguration(self) -> None:
        """``for U1 : SubComp use configuration work.BaseCfg;``"""
        document = self._document()
        item = document.Configurations["cfg"].BlockConfiguration.Items[0]

        self.assertIsInstance(item, ComponentConfiguration)
        aspect = item.BindingIndication.EntityAspect
        self.assertIsInstance(aspect, EntityAspectConfiguration)
        self.assertIsNotNone(aspect.Configuration)

    def test_EntityAspectOpen(self) -> None:
        """``for U2 : SubComp use open;``"""
        document = self._document()
        item = document.Configurations["cfg"].BlockConfiguration.Items[1]

        aspect = item.BindingIndication.EntityAspect
        self.assertIsInstance(aspect, EntityAspectOpen)

    def test_OthersInstantiationListAndEntityAspectEntity(self) -> None:
        """``for others : SubComp use entity work.Sub(Behav);``"""
        document = self._document()
        item = document.Configurations["cfg"].BlockConfiguration.Items[2]

        self.assertIsInstance(item.InstantiationList, OthersInstantiationList)

        aspect = item.BindingIndication.EntityAspect
        self.assertIsInstance(aspect, EntityAspectEntity)
        self.assertIsNotNone(aspect.Entity)
        self.assertIsNotNone(aspect.Architecture)

    def test_ConfigurationSpecification(self) -> None:
        """``for U4 : SubComp use entity work.Sub(Behav);`` - declared directly in the architecture,
        not inside a separate configuration declaration."""
        document = self._document()
        architecture = document.Architectures["consumer"]["Rtl"]
        item = architecture.DeclaredItems[1]

        self.assertIsInstance(item, ComponentConfiguration)
        self.assertIsNotNone(item.ComponentName)
        self.assertIsInstance(item.BindingIndication.EntityAspect, EntityAspectEntity)
