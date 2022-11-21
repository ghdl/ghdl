from pathlib import Path
from unittest import TestCase, skip, expectedFailure

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import name_table, files_map, errorout_console, flags
from pyGHDL.libghdl import file_comments
from pyGHDL.libghdl.vhdl import nodes, sem_lib


if __name__ == "__main__":
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Instantiate(TestCase):
    _root = Path(__file__).resolve().parent

    @staticmethod
    def getIdentifier(node) -> str:
        """Return the Python string from node :obj:`node` identifier."""
        return name_table.Get_Name_Ptr(nodes.Get_Identifier(node))

    @classmethod
    def setUpClass(cls):
        libghdl.initialize()

        # Print error messages on the console.
        errorout_console.Install_Handler()

        # Set options. This must be done before analyze_init()
        flags.Flag_Gather_Comments.value = True
        libghdl.set_option("--std=08")

        # Finish initialization. This will load the standard package.
        if libghdl.analyze_init_status() != 0:
            self.fail("libghdl initialization error")

    def checkComments(self, node, name) -> None:
        f = files_map.Location_To_File(nodes.Get_Location(node))
        idx = file_comments.Find_First_Comment(f, node)
        while idx != file_comments.No_Comment_Index:
            s = file_comments.Get_Comment(f, idx)
            self.assertTrue(s.find(':'+name+':') >= 0,
                            "no :{}: in '{}'".format(name, s))
            idx = file_comments.Get_Next_Comment(f, idx)

    def checkInterfaces(self, first) -> None:
        inter = first
        while inter != nodes.Null_Iir:
            self.checkComments(inter, self.getIdentifier(inter))
            inter = nodes.Get_Chain(inter)

    def checkFile(self, filename) -> None:
        # Load the file
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail("Cannot read file '{!s}'".format(filename))

        # Parse
        file = sem_lib.Load_File(sfe)

        # Display all design units
        designUnit = nodes.Get_First_Design_Unit(file)
        while designUnit != nodes.Null_Iir:
            libraryUnit = nodes.Get_Library_Unit(designUnit)

            k = nodes.Get_Kind(libraryUnit)
            name = self.getIdentifier(libraryUnit)

            if k == nodes.Iir_Kind.Entity_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(libraryUnit, name)
                self.checkInterfaces(nodes.Get_Generic_Chain(libraryUnit))
                self.checkInterfaces(nodes.Get_Port_Chain(libraryUnit))

            elif k == nodes.Iir_Kind.Architecture_Body:
                self.checkComments(designUnit, name)
                self.checkComments(libraryUnit, name)

            elif k == nodes.Iir_Kind.Package_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(libraryUnit, name)

            elif k == nodes.Iir_Kind.Context_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(libraryUnit, name)

            elif k == nodes.Iir_Kind.Configuration_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(libraryUnit, name)

            else:
                self.fail("Unknown unit.")

            designUnit = nodes.Get_Chain(designUnit)

    def test_Comments(self) -> None:
        """Very first test"""
        self.checkFile(self._root / "DesignComment.vhdl")

    @skip("not yet handled")
    def test_Comment2(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Comment2.vhdl")

    def test_entity_before(self) -> None:
        self.checkFile(self._root / "ent_bef.vhdl")

    def test_arch_before(self) -> None:
        self.checkFile(self._root / "arch_bef.vhdl")

    def test_pkg_before(self) -> None:
        self.checkFile(self._root / "pkg_bef.vhdl")

    def test_ctxt_before(self) -> None:
        self.checkFile(self._root / "ctxt_bef.vhdl")

    def test_conf_before(self) -> None:
        self.checkFile(self._root / "conf_bef.vhdl")

    def test_entity_inside(self) -> None:
        self.checkFile(self._root / "ent_inside.vhdl")

    @expectedFailure
    def test_arch_inside_fail(self) -> None:
        self.checkFile(self._root / "arch_inside_fail.vhdl")

    def test_arch_inside(self) -> None:
        self.checkFile(self._root / "arch_inside.vhdl")

    @expectedFailure
    def test_pkg_inside_fail(self) -> None:
        self.checkFile(self._root / "pkg_inside_fail.vhdl")

    def test_pkg_inside(self) -> None:
        self.checkFile(self._root / "pkg_inside.vhdl")

    @expectedFailure
    def test_ctxt_inside_fail(self) -> None:
        self.checkFile(self._root / "ctxt_inside_fail.vhdl")

    def test_ctxt_inside(self) -> None:
        self.checkFile(self._root / "ctxt_inside.vhdl")

    @expectedFailure
    def test_conf_inside_fail(self) -> None:
        self.checkFile(self._root / "conf_inside_fail.vhdl")

    def test_conf_inside(self) -> None:
        self.checkFile(self._root / "conf_inside.vhdl")
