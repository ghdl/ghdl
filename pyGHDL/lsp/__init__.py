# =============================================================================
#               ____ _   _ ____  _       _
#  _ __  _   _ / ___| | | |  _ \| |     | |___ _ __
# | '_ \| | | | |  _| |_| | | | | |     | / __| '_ \
# | |_) | |_| | |_| |  _  | |_| | |___ _| \__ \ |_) |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|___/ .__/
# |_|    |___/                                |_|
# =============================================================================
# Authors:
#   Tristan Gingold
#
# Package module:   Language Server Protocol implementation for VHDL.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2026 Tristan Gingold
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
from pyTooling.Decorators import export

from pyGHDL import GHDLBaseException


@export
class LSPException(GHDLBaseException):
    """
    The exception is raised for every failure of the language server.

    It is the base-class of the language server's exceptions, so ``except LSPException`` catches them
    without also catching failures of the analyzer or of the document object model.
    """


class LSPConnTrace(object):
    """Wrapper class to save in and out packets"""

    def __init__(self, basename, conn):
        """
        Initializes the trace by opening the two log files beside the connection.

        :param basename: The path the traces are written to, with ``.in`` and ``.out`` appended.
        :param conn:     The connection to wrap.
        """
        self.conn = conn
        self.trace_in = open(basename + ".in", "w")
        self.trace_out = open(basename + ".out", "w")

    def readline(self):
        """
        Read a line from the wrapped connection and record it.

        :returns: The line that was read.
        """
        res = self.conn.readline()
        self.trace_in.write(res)
        return res

    def read(self, size):
        """
        Read from the wrapped connection and record what was read.

        :param size: The number of characters to read.
        :returns:    The characters that were read.
        """
        res = self.conn.read(size)
        self.trace_in.write(res)
        self.trace_in.flush()
        return res

    def write(self, out):
        """
        Write to the wrapped connection and record what was written.

        :param out: The text to write.
        """
        self.conn.write(out)
        self.trace_out.write(out)
        self.trace_out.flush()
