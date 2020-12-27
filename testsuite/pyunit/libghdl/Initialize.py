from unittest import TestCase


if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)

class Instantiate(TestCase):
	def test_InitializeGHDL(self):
		pass

	def test_ReadSourceFile(self):
		pass

	def test_ParseFile(self):
		pass

	def test_ListDesignUnits(self):
		pass
