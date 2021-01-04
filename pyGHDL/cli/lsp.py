#!/usr/bin/env python
# =============================================================================
#                ____ _   _ ____  _           _ _
#   _ __  _   _ / ___| | | |  _ \| |      ___| (_)
#  | '_ \| | | | |  _| |_| | | | | |     / __| | |
#  | |_) | |_| | |_| |  _  | |_| | |___ | (__| | |
#  | .__/ \__, |\____|_| |_|____/|_____(_)___|_|_|
#  |_|    |___/
# =============================================================================
# Authors:          Tristan Gingold
#
# Package module:   GHDLs Language Server implementing LSP for VHDL.
#
# License:
# ============================================================================
# Copyright (C) 2019-2020 Tristan Gingold
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
from __future__ import absolute_import

import argparse
import logging
import sys
import os

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl     import version, errorout_console
from pyGHDL.lsp         import LSPConnTrace
from pyGHDL.lsp.lsp     import LSPConn, LanguageProtocolServer
from pyGHDL.lsp.vhdl_ls import VhdlLanguageServer

logger = logging.getLogger("ghdl-ls")


def rotate_log_files(basename, num):
    for i in range(num, 0, -1):
        oldfile = "{}.{}".format(basename, i - 1)
        if os.path.isfile(oldfile):
            os.rename(oldfile, "{}.{}".format(basename, i))
    if os.path.isfile(basename):
        os.rename(basename, "{}.0".format(basename))


def main():
    parser = argparse.ArgumentParser(description="VHDL Language Protocol Server")
    parser.add_argument(
        "--version", "-V", action="version", version="%(prog)s " + version.__version__
    )
    parser.add_argument(
        "--verbose", "-v", action="count", default=0, help="Show debug output"
    )
    parser.add_argument(
        "--log-file", help="Redirect logs to the given file instead of stderr"
    )
    parser.add_argument("--trace-file", help="Save rpc data to FILE.in and FILE.out")
    parser.add_argument("--input", "-i", help="Read request from file")
    parser.add_argument(
        "--disp-config",
        action="store_true",
        help="Disp installation configuration and exit",
    )

    args = parser.parse_args()

    if args.disp_config:
        errorout_console.Install_Handler()
        libghdl.disp_config()
        return

    # Setup logging
    if args.verbose >= 2:
        loglevel = logging.DEBUG
    elif args.verbose >= 1:
        loglevel = logging.INFO
    else:
        loglevel = logging.ERROR

    if args.log_file:
        rotate_log_files(args.log_file, 5)
        logstream = open(args.log_file, "w")
    else:
        logstream = sys.stderr
    logging.basicConfig(
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
            rotate_log_files(trace_file + ".in", 5)
            rotate_log_files(trace_file + ".out", 5)
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

if __name__ == '__main__':
    main()
