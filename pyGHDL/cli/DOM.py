#!/usr/bin/env python3

from sys import argv
from sys import exit as sysexit

from pathlib import Path

from pydecor import export

from pyGHDL.dom import Misc
from pyGHDL import GHDLBaseException

__all__ = []
__api__ = __all__

from pyGHDL.dom.formatting.prettyprint import PrettyPrint


@export
class Application:
    _design: Misc.Design

    def __init__(self):
        self._design = Misc.Design()

    def addFile(self, filename: Path, library: str):
        document = Misc.Document(filename)
        self._design.Documents.append(document)

    def prettyPrint(self):
        PP = PrettyPrint()

        buffer = []

        buffer.append("Design:")
        for line in PP.formatDesign(self._design, 1):
            buffer.append(line)

        print("\n".join(buffer))


def main(items):
    _exitcode = 0

    if len(items) < 1:
        print("Please, provide the files to be analyzed as CLI arguments.")
        print("Using <testsuite/pyunit/SimpleEntity.vhdl> for demo purposes.\n")
        items = ["testsuite/pyunit/SimpleEntity.vhdl"]

    for item in items:
        try:
            app = Application()
            app.addFile(Path(item), "default_lib")
            app.prettyPrint()
        except GHDLBaseException as ex:
            print(ex)
            _exitcode = 1

    return _exitcode


if __name__ == "__main__":
    sysexit(main(argv[1:]))
