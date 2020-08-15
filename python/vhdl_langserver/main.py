#!/usr/bin/env python
from __future__ import absolute_import

import argparse
import logging
import sys
import os

import libghdl
import libghdl.thin.errorout_console

from . import version
from . import lsp
from . import vhdl_ls

logger = logging.getLogger("ghdl-ls")


class LSPConnTrace(object):
    """Wrapper class to save in and out packets"""

    def __init__(self, basename, conn):
        self.conn = conn
        self.trace_in = open(basename + ".in", "w")
        self.trace_out = open(basename + ".out", "w")

    def readline(self):
        res = self.conn.readline()
        self.trace_in.write(res)
        return res

    def read(self, size):
        res = self.conn.read(size)
        self.trace_in.write(res)
        self.trace_in.flush()
        return res

    def write(self, out):
        self.conn.write(out)
        self.trace_out.write(out)
        self.trace_out.flush()


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
        libghdl.thin.errorout_console.Install_Handler()
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

    conn = lsp.LSPConn(instream, sys.stdout.buffer)

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

    handler = vhdl_ls.VhdlLanguageServer()

    try:
        server = lsp.LanguageProtocolServer(handler, conn)
        server.run()
    except Exception:
        logger.exception("Uncaught error")
        sys.exit(1)
