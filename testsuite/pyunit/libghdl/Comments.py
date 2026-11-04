from pathlib import Path
from re import search
from unittest import TestCase, expectedFailure

import pyGHDL.libghdl as libghdl
from pyGHDL.libghdl import name_table, files_map, errorout_console, flags
from pyGHDL.libghdl import file_comments
from pyGHDL.libghdl.vhdl import nodes, flists, sem_lib, utils


if __name__ == "__main__":
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Base(TestCase):
    _root = Path(__file__).resolve().parent / "examples"

    @staticmethod
    def getIdentifier(node) -> str:
        """Return the Python string from node :obj:`node` identifier."""
        return utils.Get_Source_Identifier_Str(node)

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
            self._attached.add(commentStart)
            idx = file_comments.Get_Next_Comment(f, idx)

    def getComments(self, sfe, node) -> list:
        """Return the comments of node :obj:`node`."""
        texts = []
        idx = file_comments.Find_First_Comment(sfe, node)
        while idx != file_comments.No_Comment_Index:
            texts.append(file_comments.Get_Comment(sfe, idx))
            idx = file_comments.Get_Next_Comment(sfe, idx)

        return texts

    def checkFlist(self, flist) -> None:
        for i in range(flists.Length(flist)):
            e = flists.Get_Nth_Element(flist, i)
            self.checkComments(e, self.getIdentifier(e))

    def checkDecls(self, first) -> None:
        decl = first
        while decl != nodes.Null_Iir:
            k = nodes.Get_Kind(decl)
            if k in nodes.Iir_Kinds.Subprogram_Body:
                self.checkDecls(nodes.Get_Declaration_Chain(decl))
                decl = nodes.Get_Chain(decl)
                continue
            if (k not in nodes.Iir_Kinds.Specification
                and k not in nodes.Iir_Kinds.Clause):
                self.checkComments(decl, self.getIdentifier(decl))
            elif k == nodes.Iir_Kind.Attribute_Specification:
                self.checkComments(decl, self.getIdentifier(nodes.Get_Attribute_Designator(decl)))
            elif k == nodes.Iir_Kind.Configuration_Specification:
                self.checkComments(decl, self.getIdentifier(nodes.Get_Component_Name(decl)))
            if k in (nodes.Iir_Kind.Type_Declaration, nodes.Iir_Kind.Anonymous_Type_Declaration):
                tdef = nodes.Get_Type_Definition(decl)
                defk = nodes.Get_Kind(tdef) if tdef != nodes.Null_Iir else None
                if defk == nodes.Iir_Kind.Record_Type_Definition:
                    self.checkFlist(nodes.Get_Elements_Declaration_List(tdef))
                elif defk == nodes.Iir_Kind.Enumeration_Type_Definition:
                    self.checkFlist(nodes.Get_Enumeration_Literal_List(tdef))
                elif defk == nodes.Iir_Kind.Physical_Type_Definition:
                    self.checkDecls(nodes.Get_Unit_Chain(tdef))
                elif defk == nodes.Iir_Kind.Protected_Type_Declaration:
                    self.checkDecls(nodes.Get_Declaration_Chain(tdef))
            elif k == nodes.Iir_Kind.Protected_Type_Body:
                self.checkDecls(nodes.Get_Declaration_Chain(decl))
            elif k == nodes.Iir_Kind.Component_Declaration:
                self.checkDecls(nodes.Get_Generic_Chain(decl))
                self.checkDecls(nodes.Get_Port_Chain(decl))
            elif k in nodes.Iir_Kinds.Subprogram_Declaration:
                self.checkDecls(nodes.Get_Interface_Declaration_Chain(decl))
            elif k == nodes.Iir_Kind.Mode_View_Declaration:
                self.checkDecls(nodes.Get_Elements_Definition_Chain(decl))
            decl = nodes.Get_Chain(decl)

    def checkConc(self, first) -> None:
        stmt = first
        while stmt != nodes.Null_Iir:
            k = nodes.Get_Kind(stmt)
            if k in nodes.Iir_Kinds.Process_Statement:
                self.checkDecls(nodes.Get_Declaration_Chain(stmt))
                id = nodes.Get_Identifier(stmt)
                if id != name_table.Null_Identifier:
                    self.checkComments(stmt, self.getIdentifier(stmt))
            elif k == nodes.Iir_Kind.Block_Statement:
                self.checkComments(stmt, self.getIdentifier(stmt))
                self.checkDecls(nodes.Get_Declaration_Chain(stmt))
                self.checkConc(nodes.Get_Concurrent_Statement_Chain(stmt))
            stmt = nodes.Get_Chain(stmt)

    def checkFile(self, filename) -> None:
        self._attached = set()

        # Load the file
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail(f"Cannot read file '{filename!s}'")

        # Parse
        file = sem_lib.Load_File(sfe)

        # The header of the file is attached to the design file
        self.checkComments(file, "header")

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
                header = nodes.Get_Package_Header(unit)
                if header != nodes.Null_Iir:
                    self.checkDecls(nodes.Get_Generic_Chain(header))
                self.checkDecls(nodes.Get_Declaration_Chain(unit))

            elif k == nodes.Iir_Kind.Package_Body:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)
                self.checkDecls(nodes.Get_Declaration_Chain(unit))

            elif k == nodes.Iir_Kind.Context_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)

            elif k == nodes.Iir_Kind.Configuration_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)

            elif k == nodes.Iir_Kind.Package_Instantiation_Declaration:
                self.checkComments(designUnit, name)
                self.checkComments(unit, name)

            else:
                self.fail("Unknown unit.")

            designUnit = nodes.Get_Chain(designUnit)

    def checkFileAllAttached(self, filename) -> None:
        """Like :meth:`checkFile`, and every comment with a ``:name:`` marker must be attached (to node *name*)."""
        self.checkFile(filename)
        markers = [line for line in filename.read_text().splitlines() if search(r"--.*:\w+:", line)]
        self.assertEqual(len(markers), len(self._attached), "comments with a marker that are not attached")


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

    def test_pkg_inside_2(self) -> None:
        self.checkFile(self._root / "comments/pkg_inside_2.vhdl")

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

    def test_process_2(self) -> None:
        self.checkFile(self._root / "comments/process_2.vhdl")

    def test_multi1(self) -> None:
        self.checkFile(self._root / "comments/multi1.vhdl")

    def test_line1(self) -> None:
        self.checkFile(self._root / "comments/line1.vhdl")


class Association(Base):
    def test_LineContinuationEndsAtToken(self) -> None:
        self.checkFileAllAttached(self._root / "comments/linecont_token.vhdl")

    def test_LineContinuationSameColumn(self) -> None:
        self.checkFileAllAttached(self._root / "comments/linecont_column.vhdl")

    def test_ObjectListFirstIdentifier(self) -> None:
        self.checkFileAllAttached(self._root / "comments/objlist.vhdl")

    def test_SameLineAfterDeclaration(self) -> None:
        self.checkFileAllAttached(self._root / "comments/samelinedecl.vhdl")

    def test_FileHeader(self) -> None:
        self.checkFileAllAttached(self._root / "comments/header.vhdl")

    def test_FileHeaderContextClause(self) -> None:
        self.checkFileAllAttached(self._root / "comments/header_context.vhdl")

    def test_FileHeaderContextReference(self) -> None:
        self.checkFileAllAttached(self._root / "comments/header_context_ref.vhdl")

    def test_FileHeaderContextDeclaration(self) -> None:
        self.checkFileAllAttached(self._root / "comments/header_context_decl.vhdl")


class Continuation(Base):
    """A same-line comment is continued by the comments that directly follow it at the same column."""

    def getPortComments(self, filename) -> dict:
        """Return the comments of each port of the entity in file :obj:`filename`, by port name."""
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail(f"Cannot read file '{filename!s}'")

        file = sem_lib.Load_File(sfe)
        entity = nodes.Get_Library_Unit(nodes.Get_First_Design_Unit(file))

        comments = {}
        port = nodes.Get_Port_Chain(entity)
        while port != nodes.Null_Iir:
            texts = []
            idx = file_comments.Find_First_Comment(sfe, port)
            while idx != file_comments.No_Comment_Index:
                texts.append(file_comments.Get_Comment(sfe, idx))
                idx = file_comments.Get_Next_Comment(sfe, idx)
            comments[self.getIdentifier(port)] = texts
            port = nodes.Get_Chain(port)

        return comments

    def test_SameColumn(self) -> None:
        comments = self.getPortComments(self._root / "comments/linecont_same_column.vhdl")

        self.assertEqual(["-- first documentation line", "-- second documentation line"], comments["a"])
        self.assertEqual(["-- first documentation line"], comments["b"])
        self.assertEqual(["-- not a continuation (another column)"], comments["c"])

    def test_Consecutive(self) -> None:
        comments = self.getPortComments(self._root / "comments/linecont_consecutive.vhdl")

        self.assertEqual(["-- first documentation line"], comments["a"])
        self.assertEqual(["-- not part of the documentation", "-- first documentation line"], comments["b"])
        self.assertEqual(["-- some comment", "-- some other comment"], comments["c"])

    def test_EmptyComment(self) -> None:
        comments = self.getPortComments(self._root / "comments/linecont_empty_comment.vhdl")

        self.assertEqual(
            ["-- first documentation line", "--", "-- third documentation line"],
            comments["a"],
        )
        self.assertEqual([], comments["b"])

    def test_EmptyLine(self) -> None:
        comments = self.getPortComments(self._root / "comments/linecont_empty_line.vhdl")

        self.assertEqual(["-- first documentation line"], comments["a"])
        self.assertEqual(["-- first documentation line for b"], comments["b"])


class BlockHeader(Base):
    """A comment on the line of the header of a block construct belongs to the construct."""

    def test_Header(self) -> None:
        self.checkFileAllAttached(self._root / "comments/block_header.vhdl")

    def test_FollowingComments(self) -> None:
        """The continuation rule applies to the header line, the following comments keep the block rule."""
        filename = self._root / "comments/block_header_following.vhdl"
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail(f"Cannot read file '{filename!s}'")

        file = sem_lib.Load_File(sfe)
        package = nodes.Get_Library_Unit(nodes.Get_First_Design_Unit(file))

        types = {}
        elements = {}
        decl = nodes.Get_Declaration_Chain(package)
        while decl != nodes.Null_Iir:
            name = self.getIdentifier(decl)
            types[name] = self.getComments(sfe, decl)
            element = flists.Get_Nth_Element(nodes.Get_Elements_Declaration_List(nodes.Get_Type_Definition(decl)), 0)
            elements[name] = self.getComments(sfe, element)
            decl = nodes.Get_Chain(decl)

        self.assertEqual(["-- first line"], types["another_column"])
        self.assertEqual(["-- for x"], elements["another_column"])

        self.assertEqual(["-- first line", "--", "-- third line"], types["empty_comment"])
        self.assertEqual([], elements["empty_comment"])

        self.assertEqual(["-- first line"], types["empty_line"])
        self.assertEqual(["-- for x"], elements["empty_line"])

        self.assertEqual(["-- first line", "-- followed by an empty line"], types["block_rule"])
        self.assertEqual(["-- for x"], elements["block_rule"])

        self.assertEqual(["-- first line"], types["interrupted"])
        self.assertEqual(["-- another column", "-- not a continuation"], elements["interrupted"])


class Association2019(Base):
    @classmethod
    def setUpClass(cls):
        libghdl.initialize()
        errorout_console.Install_Handler()
        flags.Flag_Gather_Comments.value = True
        libghdl.set_option("--std=19")
        if libghdl.analyze_init_status() != 0:
            cls.fail("libghdl initialization error")

    def test_FreedNodes(self) -> None:
        self.checkFileAllAttached(self._root / "comments/freed.vhdl")

    def test_Gathering(self) -> None:
        self.checkFileAllAttached(self._root / "comments/gather.vhdl")


class SingleLine(Base):
    def test_Complex(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Complex_SingleLine.vhdl")


class MultiLine(Base):
    def test_Complex(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Complex_MultiLine.vhdl")

# Empty line before to easy cut & put
