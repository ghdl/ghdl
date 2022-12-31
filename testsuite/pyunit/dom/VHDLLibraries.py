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
#   Unai Martinez-Corral
#
# Testsuite:        Parse files from ieee2008 directory.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2022 Tristan Gingold
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

from pytest import mark

from pyGHDL.dom.NonStandard import Design, Document


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


_GHDL_ROOT = Path(__file__).parent.parent.parent.parent.resolve()
_LIBRARIES_ROOT = _GHDL_ROOT / "libraries"

_IEEE2008_ROOT = _LIBRARIES_ROOT / "ieee2008"
_MENTOR_ROOT = _LIBRARIES_ROOT / "mentor"
_SYNOPSYS_ROOT = _LIBRARIES_ROOT / "synopsys"
_VITAL_ROOT = _LIBRARIES_ROOT / "vital2000"


design = Design()


@mark.parametrize("file", [str(f.relative_to(_IEEE2008_ROOT)) for f in _IEEE2008_ROOT.glob("*.vhdl")])
def test_IEEE2008(file):
    filePath = _IEEE2008_ROOT / file

    lib = design.GetLibrary("ieee2008")
    document = Document(filePath)
    design.AddDocument(document, lib)


@mark.parametrize("file", [str(f.relative_to(_MENTOR_ROOT)) for f in _MENTOR_ROOT.glob("*.vhdl")])
def test_Mentor(file):
    filePath = _MENTOR_ROOT / file

    lib = design.GetLibrary("mentor")
    document = Document(filePath)
    design.AddDocument(document, lib)


@mark.parametrize("file", [str(f.relative_to(_SYNOPSYS_ROOT)) for f in _SYNOPSYS_ROOT.glob("*.vhdl")])
def test_Synopsys(file):
    filePath = _SYNOPSYS_ROOT / file

    lib = design.GetLibrary("synopsys")
    document = Document(filePath)
    design.AddDocument(document, lib)


@mark.xfail(reason="Needs further investigations.")
@mark.parametrize("file", [str(f.relative_to(_VITAL_ROOT)) for f in _VITAL_ROOT.glob("*.vhdl")])
def test_Vital(file):
    filePath = _VITAL_ROOT / file

    lib = design.GetLibrary("vital")
    document = Document(filePath)
    design.AddDocument(document, lib)
