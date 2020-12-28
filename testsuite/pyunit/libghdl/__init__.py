from unittest import TestSuite

try:
	from testsuite.pyunit.libghdl import Initialize
except ModuleNotFoundError:
	from pyunit.libghdl import Initialize

def load_tests(loader, testCases, pattern):
	suite = TestSuite()

	suite.addTests(loader.loadTestsFromModule(Initialize))

	return suite
