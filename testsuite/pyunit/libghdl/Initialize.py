from pathlib import Path
from typing import Any, NoReturn
from unittest import TestCase

import libghdl
from libghdl.thin import name_table
from libghdl.thin import files_map
from libghdl.thin.vhdl import nodes
from libghdl.thin.vhdl import sem_lib
from libghdl.thin import errorout_console


if __name__ == "__main__":
	print("ERROR: you called a testcase declaration file as an executable module.")
	print("Use: 'python -m unitest <testcase module>'")
	exit(1)


class Instantiate(TestCase):
	_filename : Path = Path("simpleEntity.vhdl")
	_sfe: Any
	_file: Any

	_continueTesting = True

	@staticmethod
	def getIdentifier(node):
		"""Return the Python string from node :param:`node` identifier"""
		return name_table.Get_Name_Ptr(nodes.Get_Identifier(node)).decode("utf-8")

	def setUp(self) -> None:
		"""Check for every test, if tests should continue."""
		if not self.__class__._continueTesting:
			self.skipTest("No reason to go on.")

	def fail(self, msg: Any = ...) -> NoReturn:
		self.__class__._continueTesting = False
		super().fail(msg)

	def test_InitializeGHDL(self) -> None:
		"""Initialization: set options and then load libaries"""

		# Print error messages on the console.
		errorout_console.Install_Handler()

		# Set options. This must be done before analyze_init()
		libghdl.set_option(b"--std=08")

		# Finish initialization. This will load the standard package.
		if libghdl.analyze_init_status() != 0:
			self.fail("libghdl initialization error")

	def test_ReadSourceFile(self) -> None:
		# Load the file
		file_id = name_table.Get_Identifier(str(self._filename).encode("utf_8"))
		self._sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
		if self._sfe == files_map.No_Source_File_Entry:
			self.fail("Cannot read file '{!s}'".format(self._filename))

	def test_ParseFile(self) -> None:
		# Parse
		self._file = sem_lib.Load_File(self._sfe)

	def test_ListDesignUnits_WhileLoop(self) -> None:
		# Display all design units
		designUnit = nodes.Get_First_Design_Unit(self._file)
		while designUnit != nodes.Null_Iir:
			libraryUnit = nodes.Get_Library_Unit(designUnit)

			if nodes.Get_Kind(libraryUnit) == nodes.Iir_Kind.Entity_Declaration:
				entityName = self.getIdentifier(libraryUnit)
				self.assertTrue(entityName == "e1")

			elif nodes.Get_Kind(libraryUnit) == nodes.Iir_Kind.Architecture_Body:
				architectureName = self.getIdentifier(libraryUnit)
				self.assertTrue(architectureName == "arch")

			else:
				self.fail("Unknown unit.")

			designUnit = nodes.Get_Chain(designUnit)
