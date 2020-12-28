from pathlib  import Path
from unittest import TestCase

from pyGHDL.dom.Misc       import Design, Library, Document


if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)


class SimpleEntity(TestCase):
	_root = Path(__file__).resolve().parent.parent
	_filename : Path = _root / "SimpleEntity.vhdl"

	def test_Design(self):
		design = Design()

		self.assertIsNotNone(design)

	# def test_Library(self):
	# 	library = Library()

	def test_Document(self):
		design = Design()
		document = Document(self._filename)
		design.Documents.append(document)

		self.assertTrue(len(design.Documents) == 1)

	def test_Entity(self):
		design = Design()
		document = Document(self._filename)
		design.Documents.append(document)

		self.assertEqual(len(design.Documents[0].Entities), 1)
		self.assertTrue(design.Documents[0].Entities[0].Name == "e1")

	def test_Architecture(self):
		design = Design()
		document = Document(self._filename)
		design.Documents.append(document)

		self.assertEqual(len(design.Documents[0].Architectures), 1)
		self.assertTrue(design.Documents[0].Architectures[0].Name == "behav")
