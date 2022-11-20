from pathlib import Path
from unittest import TestCase

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import name_table, files_map, errorout_console, flags
from pyGHDL.libghdl import file_comments
from pyGHDL.libghdl.vhdl import nodes, sem_lib


if __name__ == "__main__":
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Instantiate(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _filename: Path = _root / "DesignComment.vhdl"

    @staticmethod
    def getIdentifier(node) -> str:
        """Return the Python string from node :obj:`node` identifier."""
        return name_table.Get_Name_Ptr(nodes.Get_Identifier(node))

    def checkComments(self, node, name) -> None:
        f = files_map.Location_To_File(nodes.Get_Location(node))
        idx = file_comments.Find_First_Comment(f, node)
        while idx != file_comments.No_Comment_Index:
            s = file_comments.Get_Comment(f, idx)
            self.assertTrue(s.find(':'+name+':') >= 0,
                            "no :{}: in '{}'".format(name, s))
            idx = file_comments.Get_Next_Comment(f, idx)

    def test_Comments(self) -> None:
        """Initialization: set options and then load libaries."""
        libghdl.initialize()

        # Print error messages on the console.
        errorout_console.Install_Handler()

        # Set options. This must be done before analyze_init()
        flags.Flag_Gather_Comments.value = True
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
                self.checkComments(designUnit, entityName)
                port = nodes.Get_Port_Chain(libraryUnit)
                while port != nodes.Null_Iir:
                    self.checkComments(port, self.getIdentifier(port))
                    port = nodes.Get_Chain(port)

            elif nodes.Get_Kind(libraryUnit) == nodes.Iir_Kind.Architecture_Body:
                architectureName = self.getIdentifier(libraryUnit)

            else:
                self.fail("Unknown unit.")

            designUnit = nodes.Get_Chain(designUnit)
