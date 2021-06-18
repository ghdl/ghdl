#!/usr/bin/env python3

from sys import argv

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
        buffer = []

        PP = PrettyPrint()

        for doc in self._design.Documents:
            for line in PP.formatDocument(doc):
                buffer.append(line)

        print("\n".join(buffer))


def main():
    items = argv[1:]
    if len(items) < 1:
        print("Please, provide the files to be analyzed as CLI arguments.")
        print("Using <testsuite/pyunit/SimpleEntity.vhdl> for demo purposes.\n")
        items = ["testsuite/pyunit/SimpleEntity.vhdl"]

    for item in items:
        print("·", item)
        try:
            app = Application()
            app.addFile(Path(item), "default_lib")
        except GHDLBaseException as ex:
            print(ex)

        app.prettyPrint()


if __name__ == "__main__":
    main()
