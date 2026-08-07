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
# Package module:   Command line helpers to convert between LSP and JSON trace formats.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2021 Tristan Gingold
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
import sys
import argparse
import json
from . import lsp


def lsp2json():
    """Utility that transforms lsp log file to a JSON list."""
    conn = lsp.LSPConn(sys.stdin.buffer, sys.stdout.buffer)
    ls = lsp.LanguageProtocolServer(None, conn)
    res = []
    while True:
        req = ls.read_request()
        if req is None:
            break
        res.append(json.loads(req))
    print(json.dumps(res, indent=2))


def json2lsp():
    """Utility that transform a JSON list to an lsp file."""
    res = json.load(sys.stdin)
    conn = lsp.LSPConn(sys.stdin.buffer, sys.stdout.buffer)
    ls = lsp.LanguageProtocolServer(None, conn)
    for req in res:
        ls.write_output(req)


def main():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(help="sub-command help")
    parser_l2j = subparsers.add_parser("lsp2json", help="convert lsp dump to JSON")
    parser_l2j.set_defaults(func=lsp2json)
    parser_j2l = subparsers.add_parser("json2lsp", help="convert JSON to lsp dump")
    parser_j2l.set_defaults(func=json2lsp)
    args = parser.parse_args()
    args.func()


if __name__ == "__main__":
    main()
