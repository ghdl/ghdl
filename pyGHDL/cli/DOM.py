#!/usr/bin/env python3

from sys import argv
from sys import exit as sysexit

from pathlib import Path

from pydecor import export

from pyGHDL import GHDLBaseException
from pyGHDL.libghdl import LibGHDLException
from pyGHDL.dom.Common import DOMException
from pyGHDL.dom.NonStandard import Design, Document, Library
from pyGHDL.dom.formatting.prettyprint import PrettyPrint, PrettyPrintException

__all__ = []
__api__ = __all__


@export
class Application:
    _design: Design

    def __init__(self):
        self._design = Design()

    def addFile(self, filename: Path, library: str):
        lib = self._design.GetLibrary(library)

        document = Document(filename)
        self._design.AddDocument(document, lib)

    def prettyPrint(self):
        PP = PrettyPrint()

        buffer = []

        buffer.append("Design:")
        for line in PP.formatDesign(self._design, 1):
            buffer.append(line)

        print("\n".join(buffer))


def handleException(ex):
    if isinstance(ex, PrettyPrintException):
        print("PP:", ex)
        return 5
    elif isinstance(ex, DOMException):
        print("DOM:", ex)
        return 4
    elif isinstance(ex, LibGHDLException):
        print("LIB:", ex)
        return 3
    elif isinstance(ex, GHDLBaseException):
        print("GHDL:", ex)
        return 2
    else:
        print(
            "Fatal: An unhandled exception has reached to the top-most exception handler."
        )
        return 1


def main(items=argv[1:]):
    _exitcode = 0

    if len(items) < 1:
        print("Please, provide the files to be analyzed as CLI arguments.")
        print("Using <testsuite/pyunit/SimpleEntity.vhdl> for demo purposes.\n")
        items = ["testsuite/pyunit/Current.vhdl"]

    for item in items:
        try:
            app = Application()
            app.addFile(Path(item), "default_lib")
            app.prettyPrint()
        except Exception as ex:
            _exitcode = handleException(ex)

    return _exitcode


if __name__ == "__main__":
    sysexit(main())
