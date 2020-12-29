from pathlib import Path
from typing  import Any

from pydecor import export

from pyVHDLModel.VHDLModel  import Design        as VHDLModel_Design
from pyVHDLModel.VHDLModel  import Library       as VHDLModel_Library
from pyVHDLModel.VHDLModel  import Document      as VHDLModel_Document

import pyGHDL.libghdl       as libghdl
from pyGHDL.libghdl         import name_table, files_map, errorout_console
from pyGHDL.libghdl.vhdl    import nodes, sem_lib

from pyGHDL.dom.Common      import LibGHDLException, GHDLException
from pyGHDL.dom.DesignUnit  import Entity, Architecture, Package, PackageBody, Context, Configuration

__all__ = []
__api__ = __all__


@export
class Design(VHDLModel_Design):
	def __init__(self):
		super().__init__()

		self.__ghdl_init()

	def __ghdl_init(self):
		"""Initialization: set options and then load libraries"""

		# Print error messages on the console
		errorout_console.Install_Handler()

		libghdl.set_option(b"--std=08")

		# Finish initialization. This will load the standard package.
		if libghdl.analyze_init_status() != 0:
			raise LibGHDLException("Error initializing 'libghdl'.")

@export
class Library(VHDLModel_Library):
	pass


@export
class Document(VHDLModel_Document):
	__ghdlFileID:           Any
	__ghdlSourceFileEntry:  Any
	__ghdlFile:             Any

	def __init__(self, path : Path = None, dontParse: bool = False):
		super().__init__(path)

		self.__ghdl_init()
		if (dontParse == False):
			self.parse()

	def __ghdl_init(self):
		# Read input file
		self.__ghdlFileID = name_table.Get_Identifier(str(self.Path).encode("utf_8"))
		self.__ghdlSourceFileEntry = files_map.Read_Source_File(name_table.Null_Identifier, self.__ghdlFileID)
		if self.__ghdlSourceFileEntry == files_map.No_Source_File_Entry:
			raise LibGHDLException("Cannot load file '{!s}'".format(self.Path))

		# parse
		self.__ghdlFile =  sem_lib.Load_File(self.__ghdlSourceFileEntry)

	def parse(self):
		unit = nodes.Get_First_Design_Unit(self.__ghdlFile)
		while unit != nodes.Null_Iir:
			libraryUnit = nodes.Get_Library_Unit(unit)
			nodeKind = nodes.Get_Kind(libraryUnit)

			if (nodeKind == nodes.Iir_Kind.Entity_Declaration):
				entity = Entity.parse(libraryUnit)
				self.Entities.append(entity)

			elif (nodeKind == nodes.Iir_Kind.Architecture_Body):
				architecture = Architecture.parse(libraryUnit)
				self.Architectures.append(architecture)

			elif (nodeKind == nodes.Iir_Kind.Package_Declaration):
				package = Package.parse(libraryUnit)
				self.Packages.append(package)

			elif (nodeKind == nodes.Iir_Kind.Package_Body):
				packageBody = PackageBody.parse(libraryUnit)
				self.PackageBodies.append(packageBody)

			elif (nodeKind == nodes.Iir_Kind.Context_Declaration):
				context = Context.parse(libraryUnit)
				self.Contexts.append(context)

			elif (nodeKind == nodes.Iir_Kind.Configuration_Declaration):
				configuration = Configuration.parse(libraryUnit)
				self.Configurations.append(configuration)

			else:
				raise GHDLException("Unknown design unit kind.")

			unit = nodes.Get_Chain(unit)
