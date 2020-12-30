from pathlib  import Path
from unittest import TestCase

from pyGHDL.dom.Misc       import Design, Document


if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)


class SimplePackage(TestCase):
	_root = Path(__file__).resolve().parent.parent
	_filename : Path = _root / "SimplePackage.vhdl"

	def test_Package(self):
		design = Design()
		document = Document(self._filename)
		design.Documents.append(document)

		self.assertEqual(len(design.Documents[0].Packages), 1)
		self.assertTrue(design.Documents[0].Packages[0].Name == "pack_1")

	def test_PackageBody(self):
		design = Design()
		document = Document(self._filename)
		design.Documents.append(document)

		self.assertEqual(len(design.Documents[0].PackageBodies), 1)
		self.assertTrue(design.Documents[0].PackageBodies[0].Name == "pack_1")
