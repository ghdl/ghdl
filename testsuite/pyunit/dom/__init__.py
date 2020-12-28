from unittest import TestSuite

from testsuite.pyunit.dom import SimpleEntity

def load_tests(loader, testCases, pattern):
	suite = TestSuite()

	suite.addTests(loader.loadTestsFromModule(SimpleEntity))

	return suite
