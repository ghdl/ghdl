from pathlib  import Path
from textwrap import dedent
from unittest import TestCase

from pyGHDL.dom import Expression
from pyGHDL.dom.Misc       import Design, Document
from pyGHDL.dom.Symbol import SimpleObjectSymbol
from pyGHDL.dom.Object import Constant
from pyGHDL.dom.Expression import InverseExpression

if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Expressions(TestCase):
    _root = Path(__file__).resolve().parent.parent

    def test_NotExpression(self):
        self._filename: Path = self._root / "{className}.vhdl".format(className=self.__class__.__name__)

        sourceCode = dedent("""\
            package package_1 is
              constant c0 : boolean := not true;
            end package;
            """)

        with self._filename.open(mode="w", encoding="utf-8") as file:
            file.write(sourceCode)

        design = Design()
        document = Document(self._filename)
        design.Documents.append(document)

        self.assertEqual(len(design.Documents[0].Packages), 1)
        package = design.Documents[0].Packages[0]
        self.assertTrue(package.Name == "package_1")
        self.assertEqual(len(package.DeclaredItems), 1)

        item: Constant = package.DeclaredItems[0]
        self.assertTrue(isinstance(item, Constant))
        self.assertTrue(item.Name == "c0")
        self.assertTrue(item.SubType.SymbolName == "boolean")

        default: Expression = item.DefaultExpression
        self.assertTrue(isinstance(default, InverseExpression))
        self.assertTrue(isinstance(default.Operand, SimpleObjectSymbol))
        self.assertTrue(default.Operand.SymbolName == "true")
