# =============================================================================
#                ____ _   _ ____  _
#   _ __  _   _ / ___| | | |  _ \| |
#  | '_ \| | | | |  _| |_| | | | | |
#  | |_) | |_| | |_| |  _  | |_| | |___
#  | .__/ \__, |\____|_| |_|____/|_____|
#  |_|    |___/
# =============================================================================
# Authors:          Tristan Gingold
#                   Patrick Lehmann
#                   Unai Martinez-Corral
#
# Package package:  Python binding for GHDL and high-level APIs.
#
# License:
# ============================================================================
# Copyright (C) 2019-2020 Tristan Gingold
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

"""
.. _python_interface:

GHDL offers two Python interfaces and a language server protocol service. All
this is provided from a ``pyGHDL`` packages with four sub-packages:

* :mod:`pyGHDL.cli` - Command line interface (CLI) applications.
* :mod:`pyGHDL.dom` - A high-level API offering a document object model (DOM).
  The underlying abstract VHDL language model is provided by :doc:`pyVHDLModel <vhdlmodel:index>`.
  The DOM is using ``libghdl`` for file analysis and parsing.
* :mod:`pyGHDL.libghdl` - A low-level API directly interacting with the shared library ``libghdl....so``/``libghdl....dll``.
  This is a procedural and C-like interface. It comes with some Python generators for easier iterating linked lists.
* :mod:`pyGHDL.lsp` - A :wikipedia:`language server protocol <Language_Server_Protocol>` (LSP)
  written in Python. The implementation offers an HTTPS service that can be used e.g. by editors and IDEs supporting LSP.
"""

__version__ = "5.0.0-dev"

__author__ = "Tristan Gingold and contributors"
__email__ = "tgingold@free.fr"
__copyright__ = "2002-2024, Tristan Gingold and contributors"
__license__ = "GNU General Public License v2"
__keywords__ = ["vhdl", "parser", "compiler", "simulator", "ghdl"]

from sys import version_info
from typing import List

from pyTooling.Decorators import export


@export
class GHDLBaseException(Exception):
    """Base exception derived from :exc:`Exception <python:Exception>` for all custom exceptions."""

    # WORKAROUND: for Python <3.11
    # Implementing a dummy method for Python versions before
    if version_info < (3, 11):  # pragma: no cover
        __notes__: List[str]

        def __init__(self, *args):
            super().__init__(*args)
            self.__notes__ = []

        def add_note(self, message: str) -> None:
            self.__notes__.append(message)

    @property
    def message(self) -> str:
        return str(self)
