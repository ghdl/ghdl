from pathlib import Path
from unittest import TestCase

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import name_table, files_map, errorout_console
from pyGHDL.libghdl.vhdl import nodes, sem_lib


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Instantiate(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _filename: Path = _root / "SimpleEntity.vhdl"

    @staticmethod
    def getIdentifier(node):
        """Return the Python string from node :obj:`node` identifier."""
        return name_table.Get_Name_Ptr(nodes.Get_Identifier(node))

    def test_InitializeGHDL(self) -> None:
        """Initialization: set options and then load libaries."""
        libghdl.initialize()

        # Print error messages on the console.
        errorout_console.Install_Handler()

        # Set options. This must be done before analyze_init()
        libghdl.set_option("--std=08")

        # Finish initialization. This will load the standard package.
        if libghdl.analyze_init_status() != 0:
            self.fail("libghdl initialization error")

        # Load the file
        file_id = name_table.Get_Identifier(str(self._filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail("Cannot read file '{!s}'".format(self._filename))

        # Parse
        file = sem_lib.Load_File(sfe)

        # Display all design units
        designUnit = nodes.Get_First_Design_Unit(file)
        while designUnit != nodes.Null_Iir:
            libraryUnit = nodes.Get_Library_Unit(designUnit)

            if nodes.Get_Kind(libraryUnit) == nodes.Iir_Kind.Entity_Declaration:
                entityName = self.getIdentifier(libraryUnit)
                self.assertEqual(
                    entityName,
                    "entity_1",
                    "expected entity name 'e1', got '{}'".format(entityName),
                )

            elif nodes.Get_Kind(libraryUnit) == nodes.Iir_Kind.Architecture_Body:
                architectureName = self.getIdentifier(libraryUnit)
                self.assertEqual(
                    architectureName,
                    "behav",
                    "expected architecture name 'behav', got '{}'".format(
                        architectureName
                    ),
                )

            else:
                self.fail("Unknown unit.")

            designUnit = nodes.Get_Chain(designUnit)
