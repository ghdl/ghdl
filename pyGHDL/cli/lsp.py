#!/usr/bin/env python
# =============================================================================
#               ____ _   _ ____  _           _ _
#  _ __  _   _ / ___| | | |  _ \| |      ___| (_)
# | '_ \| | | | |  _| |_| | | | | |     / __| | |
# | |_) | |_| | |_| |  _  | |_| | |___ | (__| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)___|_|_|
# |_|    |___/
# =============================================================================
# Authors:
#   Tristan Gingold
#
# Package module:   GHDLs Language Server implementing LSP for VHDL.
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

from __future__ import absolute_import

from argparse import ArgumentParser
from logging import getLogger, DEBUG, INFO, ERROR, basicConfig
from sys import (
    argv as sys_argv,
    stdin as sys_stdin,
    stdout as sys_stdout,
    stderr as sys_stderr,
    exit as sys_exit,
)
import sys
from os import environ as os_environ, getcwd as os_getcwd
import os
from pathlib import Path

from pyTooling.Decorators import export

from pyGHDL import __version__ as ghdlVersion
import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import errorout_console
from pyGHDL.lsp import LSPConnTrace
from pyGHDL.lsp.lsp import LSPConn, LanguageProtocolServer
from pyGHDL.lsp.vhdl_ls import VhdlLanguageServer

__loggerName = "ghdl-ls"


def __rotate_log_files(basename: str, num: int):
    """Rotate existing log files."""
    # Remove the oldest file.  This one will be lost.
    # Required on Windows as it is an error to rename a file to an existing
    # one.
    # Note: Path.with_suffix cannot be used as there might be multiple
    # suffixes (like in trace.out.0).
    oldfile = Path("{}.{}".format(basename, num))
    if oldfile.is_file():
        oldfile.unlink()
    # Rotate old files
    for i in range(num, 0, -1):
        oldfile = Path("{}.{}".format(basename, i - 1))
        if oldfile.is_file():
            oldfile.rename(Path("{}.{}".format(basename, i)))
    # Rotate the newest log file.
    bname = Path(basename)
    if bname.is_file():
        bname.rename(Path("{}.{}".format(basename, 0)))


def _generateCLIParser() -> ArgumentParser:
    """Creates an CLI argument parser based on ``argparse``."""
    parser = ArgumentParser(
        description="VHDL Language Protocol Server. Find info about clients in `ghdl/ghdl-language-server <https://github.com/ghdl/ghdl-language-server>`__."
    )
    parser.add_argument("--version", "-V", action="version", version="%(prog)s " + ghdlVersion)
    parser.add_argument("--verbose", "-v", action="count", default=0, help="Show debug output")
    parser.add_argument("--log-file", help="Redirect logs to the given file instead of stderr")
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
        print("python:")
        print("sys.platform: {}, os.name: {}".format(sys.platform, os.name))
        print(sys.version)
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
        logstream = sys_stderr

    basicConfig(
        format="%(asctime)-15s [%(levelname)s] %(message)s",
        stream=logstream,
        level=loglevel,
    )

    if args.verbose != 0:
        sys_stderr.write("Args: {}\n".format(sys_argv))
        sys_stderr.write("Current directory: {}\n".format(os_getcwd()))

    logger.info("Args: %s", sys_argv)
    logger.info("Current directory is %s", os_getcwd())

    # Connection
    instream = sys_stdin.buffer
    if args.input is not None:
        instream = open(args.input, "rb")

    conn = LSPConn(instream, sys_stdout.buffer)

    trace_file = args.trace_file
    if trace_file is None:
        trace_file = os_environ.get("GHDL_LS_TRACE")
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
        sys_exit(1)


if __name__ == "__main__":
    main()
