from pathlib import Path
from unittest import TestCase, expectedFailure
from pytest import mark

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import name_table, files_map, errorout_console, flags
from pyGHDL.libghdl import file_comments
from pyGHDL.libghdl.vhdl import nodes, flists, sem_lib


if __name__ == "__main__":
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Base(TestCase):
    _root = Path(__file__).resolve().parent / "examples"

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
            cls.fail("libghdl initialization error")

    def checkComments(self, node, name) -> None:
        f = files_map.Location_To_File(nodes.Get_Location(node))
        idx = file_comments.Find_First_Comment(f, node)
        while idx != file_comments.No_Comment_Index:
            commentText = file_comments.Get_Comment(f, idx)
            commentStart = file_comments.Get_Comment_Start(f, idx)
            commentEnd = file_comments.Get_Comment_Last(f, idx)
            self.assertTrue(commentText.find(':'+name+':') >= 0, f"no :{name}: in [{commentStart}..{commentEnd}] '{commentText}'({len(commentText)})")
            idx = file_comments.Get_Next_Comment(f, idx)

    def checkFlist(self, flist) -> None:
        for i in range(flists.Length(flist)):
            e = flists.Get_Nth_Element(flist, i)
            self.checkComments(e, self.getIdentifier(e))

    def checkDecls(self, first) -> None:
        decl = first
        while decl != nodes.Null_Iir:
            k = nodes.Get_Kind(decl)
            if (k not in nodes.Iir_Kinds.Specification
                and k not in nodes.Iir_Kinds.Clause):
                self.checkComments(decl, self.getIdentifier(decl))
            if k == nodes.Iir_Kind.Type_Declaration:
                tdef = nodes.Get_Type_Definition(decl)
                defk = nodes.Get_Kind(tdef)
                if defk == nodes.Iir_Kind.Record_Type_Definition:
                    self.checkFlist(nodes.Get_Elements_Declaration_List(tdef))
                elif defk == nodes.Iir_Kind.Enumeration_Type_Definition:
                    self.checkFlist(nodes.Get_Enumeration_Literal_List(tdef))
            elif k in nodes.Iir_Kinds.Subprogram_Declaration:
                self.checkDecls(nodes.Get_Interface_Declaration_Chain(decl))
            decl = nodes.Get_Chain(decl)

    def checkConc(self, first) -> None:
        stmt = first
        while stmt != nodes.Null_Iir:
            k = nodes.Get_Kind(stmt)
            if k in nodes.Iir_Kinds.Process_Statement:
                self.checkDecls(nodes.Get_Declaration_Chain(stmt))
                id = nodes.Get_Identifier(stmt)
                if id != name_table.Null_Identifier:
                    self.checkComments(stmt, name_table.Get_Name_Ptr(id))
            stmt = nodes.Get_Chain(stmt)

    def checkFile(self, filename) -> None:
        # Load the file
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail(f"Cannot read file '{filename!s}'")

        # Parse
        file = sem_lib.Load_File(sfe)

        # Display all design units
        designUnit = nodes.Get_First_Design_Unit(file)
        while designUnit != nodes.Null_Iir:
            unit = nodes.Get_Library_Unit(designUnit)

            k = nodes.Get_Kind(unit)
            name = self.getIdentifier(unit)

            if k == nodes.Iir_Kind.Entity_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)
                self.checkDecls(nodes.Get_Generic_Chain(unit))
                self.checkDecls(nodes.Get_Port_Chain(unit))

            elif k == nodes.Iir_Kind.Architecture_Body:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)
                self.checkDecls(nodes.Get_Declaration_Chain(unit))
                self.checkConc(nodes.Get_Concurrent_Statement_Chain(unit))

            elif k == nodes.Iir_Kind.Package_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)
                self.checkDecls(nodes.Get_Declaration_Chain(unit))

            elif k == nodes.Iir_Kind.Context_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)

            elif k == nodes.Iir_Kind.Configuration_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)

            else:
                self.fail("Unknown unit.")

            designUnit = nodes.Get_Chain(designUnit)


class CommentAssociation(Base):
    def test_Comments(self) -> None:
        """Very first test"""
        self.checkFile(self._root / "comments/DesignComment.vhdl")

    def test_entity_before(self) -> None:
        self.checkFile(self._root / "comments/ent_bef.vhdl")

    def test_arch_before(self) -> None:
        self.checkFile(self._root / "comments/arch_bef.vhdl")

    def test_pkg_before(self) -> None:
        self.checkFile(self._root / "comments/pkg_bef.vhdl")

    def test_ctxt_before(self) -> None:
        self.checkFile(self._root / "comments/ctxt_bef.vhdl")

    def test_conf_before(self) -> None:
        self.checkFile(self._root / "comments/conf_bef.vhdl")

    def test_entity_inside(self) -> None:
        self.checkFile(self._root / "comments/ent_inside.vhdl")

    def test_entity_arch(self) -> None:
        self.checkFile(self._root / "comments/ent_arch.vhdl")

    @expectedFailure
    def test_arch_inside_fail(self) -> None:
        self.checkFile(self._root / "comments/arch_inside_fail.vhdl")

    def test_arch_inside(self) -> None:
        self.checkFile(self._root / "comments/arch_inside.vhdl")

    def test_arch_inside_2(self) -> None:
        self.checkFile(self._root / "comments/arch_inside_2.vhdl")

    @expectedFailure
    def test_pkg_inside_fail(self) -> None:
        self.checkFile(self._root / "comments/pkg_inside_fail.vhdl")

    @expectedFailure
    def test_pkg_inside_fail2(self) -> None:
        self.checkFile(self._root / "comments/pkg_inside_fail2.vhdl")

    def test_pkg_inside(self) -> None:
        self.checkFile(self._root / "comments/pkg_inside.vhdl")

    @expectedFailure
    def test_ctxt_inside_fail(self) -> None:
        self.checkFile(self._root / "comments/ctxt_inside_fail.vhdl")

    def test_ctxt_inside(self) -> None:
        self.checkFile(self._root / "comments/ctxt_inside.vhdl")

    @expectedFailure
    def test_conf_inside_fail(self) -> None:
        self.checkFile(self._root / "comments/conf_inside_fail.vhdl")

    def test_conf_inside(self) -> None:
        self.checkFile(self._root / "comments/conf_inside.vhdl")

    @expectedFailure
    def test_const_fail(self) -> None:
        self.checkFile(self._root / "comments/const_fail.vhdl")

    def test_const(self) -> None:
        self.checkFile(self._root / "comments/const.vhdl")

    @expectedFailure
    def test_sig_fail(self) -> None:
        self.checkFile(self._root / "comments/sig_fail.vhdl")

    def test_sig(self) -> None:
        self.checkFile(self._root / "comments/sig.vhdl")

    def test_sig_2(self) -> None:
        self.checkFile(self._root / "comments/sig_2.vhdl")

    @expectedFailure
    def test_var_fail(self) -> None:
        self.checkFile(self._root / "comments/var_fail.vhdl")

    def test_var(self) -> None:
        self.checkFile(self._root / "comments/var.vhdl")

    @expectedFailure
    def test_type_fail(self) -> None:
        self.checkFile(self._root / "comments/type_fail.vhdl")

    def test_type(self) -> None:
        self.checkFile(self._root / "comments/type.vhdl")

    def test_array(self) -> None:
        self.checkFile(self._root / "comments/array.vhdl")

    @expectedFailure
    def test_record_fail(self) -> None:
        self.checkFile(self._root / "comments/record_fail.vhdl")

    def test_record(self) -> None:
        self.checkFile(self._root / "comments/record.vhdl")

    @expectedFailure
    def test_elements_fail(self) -> None:
        self.checkFile(self._root / "comments/elements_fail.vhdl")

    def test_element_1(self) -> None:
        self.checkFile(self._root / "comments/element_1.vhdl")

    def test_element_2(self) -> None:
        self.checkFile(self._root / "comments/element_2.vhdl")

    def test_element_3(self) -> None:
        self.checkFile(self._root / "comments/element_3.vhdl")

    def test_element_4(self) -> None:
        self.checkFile(self._root / "comments/element_4.vhdl")

    @expectedFailure
    def test_enum_fail(self) -> None:
        self.checkFile(self._root / "comments/enum_fail.vhdl")

    def test_enum(self) -> None:
        self.checkFile(self._root / "comments/enum.vhdl")

    @expectedFailure
    def test_enumlit_fail(self) -> None:
        self.checkFile(self._root / "comments/enumlit_fail.vhdl")

    def test_enumlit_1(self) -> None:
        self.checkFile(self._root / "comments/enumlit_1.vhdl")

    def test_enumlit_2(self) -> None:
        self.checkFile(self._root / "comments/enumlit_2.vhdl")

    def test_enumlit_3(self) -> None:
        self.checkFile(self._root / "comments/enumlit_3.vhdl")

    @expectedFailure
    def test_func_fail(self) -> None:
        self.checkFile(self._root / "comments/func_fail.vhdl")

    def test_func(self) -> None:
        self.checkFile(self._root / "comments/func.vhdl")

    @expectedFailure
    def test_func_param_fail(self) -> None:
        self.checkFile(self._root / "comments/func_param_fail.vhdl")

    def test_func_param(self) -> None:
        self.checkFile(self._root / "comments/func_param.vhdl")

    @expectedFailure
    def test_process_fail(self) -> None:
        self.checkFile(self._root / "comments/process_fail.vhdl")

    def test_process(self) -> None:
        self.checkFile(self._root / "comments/process.vhdl")

    def test_multi1(self) -> None:
        self.checkFile(self._root / "comments/multi1.vhdl")

    def test_line1(self) -> None:
        self.checkFile(self._root / "comments/line1.vhdl")


class Complex(Base):
    @mark.xfail(reason="not yet handled")
    def test_Comment2(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Complex.vhdl")

# Empty line before to easy cut & put
