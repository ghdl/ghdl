from unittest import TestSuite

from testsuite.pyunit import libghdl, dom


def load_tests(loader, testCases, pattern):
	suite = TestSuite()

	suite.addTests(loader.loadTestsFromModule(libghdl))
	suite.addTests(loader.loadTestsFromModule(dom))

	return suite
