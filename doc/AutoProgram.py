# =============================================================================
# Authors:          Patrick Lehmann
#
# Package module:   Stub module to extract argparse definitions.
#
# License:
# ============================================================================
# Copyright (C) 2019-2021 Tristan Gingold
#
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
#
from sys import path as sys_path

sys_path.append("..")

from pyGHDL.cli.lsp   import _generateCLIParser as lsp_parserGenerator
from scripts.pnodes   import _generateCLIParser as pnodes_parserGenerator
from scripts.pnodespy import _generateCLIParser as pnodespy_parserGenerator

# entry point
lsp_parser = lsp_parserGenerator()
pnodes_parser = pnodes_parserGenerator()
pnodespy_parser = pnodespy_parserGenerator()
