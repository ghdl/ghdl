# =============================================================================
#               ____ _   _ ____  _          _
#  _ __  _   _ / ___| | | |  _ \| |      __| | ___  _ __ ___
# | '_ \| | | | |  _| |_| | | | | |     / _` |/ _ \| '_ ` _ \
# | |_) | |_| | |_| |  _  | |_| | |___ | (_| | (_) | | | | | |
# | .__/ \__, |\____|_| |_|____/|_____(_)__,_|\___/|_| |_| |_|
# |_|    |___/
# =============================================================================
# Authors:
#   Unai Martinez-Corral
#
# Testsuite:        Parse files from sanity checks
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
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
from subprocess import check_call, STDOUT

from pytest import mark

from pyGHDL.dom.NonStandard import Design, Document

if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)

_TESTSUITE_ROOT = Path(__file__).parent.parent.parent.resolve()
_GHDL_ROOT = _TESTSUITE_ROOT.parent


design = Design()

@mark.xfail
@mark.parametrize("file", [str(f.relative_to(_TESTSUITE_ROOT)) for f in _TESTSUITE_ROOT.glob("sanity/**/*.vhdl")])
def test_AllVHDLSources(file):
    check_call(["python", _GHDL_ROOT / "pyGHDL/cli/DOM.py", file], stderr=STDOUT)

#    document = Document(Path(file))
#    design.Documents.append(document)
