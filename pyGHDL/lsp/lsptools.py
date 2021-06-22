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
