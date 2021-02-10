#!/usr/bin/env python
# =============================================================================
#                ____ _   _ ____  _           _ _
#   _ __  _   _ / ___| | | |  _ \| |      ___| (_)
#  | '_ \| | | | |  _| |_| | | | | |     / __| | |
#  | |_) | |_| | |_| |  _  | |_| | |___ | (__| | |
#  | .__/ \__, |\____|_| |_|____/|_____(_)___|_|_|
#  |_|    |___/
# =============================================================================
#  Authors:
#   Tristan Gingold
#
# Package module:   GHDLs Language Server implementing LSP for VHDL.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2020 Tristan Gingold
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

from __future__ import absolute_import

from argparse import ArgumentParser
from logging import getLogger, DEBUG, INFO, ERROR, basicConfig
import sys
import os

from pydecor import export

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import version, errorout_console
from pyGHDL.lsp import LSPConnTrace
from pyGHDL.lsp.lsp import LSPConn, LanguageProtocolServer
from pyGHDL.lsp.vhdl_ls import VhdlLanguageServer

__loggerName = "ghdl-ls"


def __rotate_log_files(basename: str, num: int):
    """Rotate existing log files."""
    for i in range(num, 0, -1):
        oldfile = "{}.{}".format(basename, i - 1)
        if os.path.isfile(oldfile):
            os.rename(oldfile, "{}.{}".format(basename, i))
    if os.path.isfile(basename):
        os.rename(basename, "{}.0".format(basename))


def _generateCLIParser() -> ArgumentParser:
    """Creates an CLI argument parser based on ``argparse``."""
    parser = ArgumentParser(
        description="VHDL Language Protocol Server. Find info about clients in `ghdl/ghdl-language-server <https://github.com/ghdl/ghdl-language-server>`__."
    )
    parser.add_argument(
        "--version", "-V", action="version", version="%(prog)s " + version.__version__
    )
    parser.add_argument(
        "--verbose", "-v", action="count", default=0, help="Show debug output"
    )
    parser.add_argument(
        "--log-file", help="Redirect logs to the given file instead of stderr"
    )
    parser.add_argument(
        "--trace-file",
        help="Save RPC data to FILE.in and FILE.out (overrides :envvar:`GHDL_LS_TRACE`)",
    )
    parser.add_argument("--input", "-i", help="Read request from file")
    parser.add_argument(
        "--disp-config",
        action="store_true",
        help="Display installation configuration and exit",
    )

    return parser


@export
def main():
    """Entrypoint of GHDL's Language Protocol Server."""
    logger = getLogger(__loggerName)

    parser = _generateCLIParser()
    args = parser.parse_args()

    if args.disp_config:
        errorout_console.Install_Handler()
        libghdl.disp_config()
        return

    # Setup logging
    if args.verbose >= 2:
        loglevel = DEBUG
    elif args.verbose >= 1:
        loglevel = INFO
    else:
        loglevel = ERROR

    if args.log_file:
        __rotate_log_files(args.log_file, 5)
        logstream = open(args.log_file, "w")
    else:
        logstream = sys.stderr

    basicConfig(
        format="%(asctime)-15s [%(levelname)s] %(message)s",
        stream=logstream,
        level=loglevel,
    )

    if args.verbose != 0:
        sys.stderr.write("Args: {}\n".format(sys.argv))
        sys.stderr.write("Current directory: {}\n".format(os.getcwd()))

    logger.info("Args: %s", sys.argv)
    logger.info("Current directory is %s", os.getcwd())

    # Connection
    instream = sys.stdin.buffer
    if args.input is not None:
        instream = open(args.input, "rb")

    conn = LSPConn(instream, sys.stdout.buffer)

    trace_file = args.trace_file
    if trace_file is None:
        trace_file = os.environ.get("GHDL_LS_TRACE")
    if trace_file is not None:
        if args.input is None:
            __rotate_log_files(trace_file + ".in", 5)
            __rotate_log_files(trace_file + ".out", 5)
            conn = LSPConnTrace(trace_file, conn)
        else:
            logger.info("Traces disabled when -i/--input")

    handler = VhdlLanguageServer()

    try:
        server = LanguageProtocolServer(handler, conn)
        server.run()
    except Exception:
        logger.exception("Uncaught error")
        sys.exit(1)


if __name__ == "__main__":
    main()
