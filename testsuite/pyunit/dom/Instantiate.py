from pathlib  import Path
from unittest import TestCase

from pyVHDLModel.VHDLModel import Design, Library, Document, Entity


if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)


class Instantiate(TestCase):
	def test_Design(self):
		design = Design()

	def test_Library(self):
		library = Library()

	def test_Document(self):
		path = Path("tests.vhdl")
		document = Document(path)

	def test_Entity(self):
		entity = Entity("entity_1")
