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
    try:
        app = Application()
        app.addFile(Path("testsuite/pyunit/SimpleEntity.vhdl"), "default_lib")
    except GHDLBaseException as ex:
        print(ex)

    app.prettyPrint()


if __name__ == "__main__":
    main()
