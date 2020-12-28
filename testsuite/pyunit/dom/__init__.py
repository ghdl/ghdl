from unittest import TestSuite

try:
	from testsuite.pyunit.dom import SimpleEntity
except ModuleNotFoundError:
	from pyunit.dom import SimpleEntity

def load_tests(loader, testCases, pattern):
	suite = TestSuite()

	suite.addTests(loader.loadTestsFromModule(SimpleEntity))

	return suite
