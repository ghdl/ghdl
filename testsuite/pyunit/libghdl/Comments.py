from pathlib import Path
from re import DOTALL, findall, search
from tempfile import TemporaryDirectory
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
                self.checkSeq(nodes.Get_Sequential_Statement_Chain(decl))
                decl = nodes.Get_Chain(decl)
                continue
            if (k not in nodes.Iir_Kinds.Specification
                and k not in nodes.Iir_Kinds.Clause
                and k != nodes.Iir_Kind.Psl_Default_Clock):
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

    def getLabel(self, stmt) -> str:
        """Return the label of statement :obj:`stmt`, or ``nolabel``."""
        if nodes.Get_Identifier(stmt) == name_table.Null_Identifier:
            return "nolabel"

        return self.getIdentifier(stmt)

    def checkSeq(self, first) -> None:
        stmt = first
        while stmt != nodes.Null_Iir:
            k = nodes.Get_Kind(stmt)
            name = self.getLabel(stmt)
            self.checkComments(stmt, name)
            if k == nodes.Iir_Kind.If_Statement:
                self.checkSeq(nodes.Get_Sequential_Statement_Chain(stmt))
                clause = nodes.Get_Else_Clause(stmt)
                while clause != nodes.Null_Iir:
                    self.checkComments(clause, f"{name}_else")
                    self.checkSeq(nodes.Get_Sequential_Statement_Chain(clause))
                    clause = nodes.Get_Else_Clause(clause)
            elif k == nodes.Iir_Kind.Case_Statement:
                assoc = nodes.Get_Case_Statement_Alternative_Chain(stmt)
                while assoc != nodes.Null_Iir:
                    self.checkComments(assoc, f"{name}_when")
                    self.checkSeq(nodes.Get_Associated_Chain(assoc))
                    assoc = nodes.Get_Chain(assoc)
            elif k in (nodes.Iir_Kind.For_Loop_Statement, nodes.Iir_Kind.While_Loop_Statement):
                self.checkSeq(nodes.Get_Sequential_Statement_Chain(stmt))
            stmt = nodes.Get_Chain(stmt)

    def checkBody(self, body, name) -> None:
        """Check the comments of a generate statement body, its comments are for :obj:`name`."""
        self.checkComments(body, name)
        self.checkDecls(nodes.Get_Declaration_Chain(body))
        self.checkConc(nodes.Get_Concurrent_Statement_Chain(body))

    def checkConc(self, first) -> None:
        stmt = first
        while stmt != nodes.Null_Iir:
            k = nodes.Get_Kind(stmt)
            name = self.getLabel(stmt)
            self.checkComments(stmt, name)
            if k in nodes.Iir_Kinds.Process_Statement:
                self.checkDecls(nodes.Get_Declaration_Chain(stmt))
                self.checkSeq(nodes.Get_Sequential_Statement_Chain(stmt))
            elif k == nodes.Iir_Kind.Block_Statement:
                self.checkDecls(nodes.Get_Declaration_Chain(stmt))
                self.checkConc(nodes.Get_Concurrent_Statement_Chain(stmt))
            elif k == nodes.Iir_Kind.For_Generate_Statement:
                self.checkBody(nodes.Get_Generate_Statement_Body(stmt), name)
            elif k == nodes.Iir_Kind.If_Generate_Statement:
                self.checkBody(nodes.Get_Generate_Statement_Body(stmt), name)
                clause = nodes.Get_Generate_Else_Clause(stmt)
                while clause != nodes.Null_Iir:
                    self.checkComments(clause, f"{name}_else")
                    self.checkBody(nodes.Get_Generate_Statement_Body(clause), name)
                    clause = nodes.Get_Generate_Else_Clause(clause)
            elif k == nodes.Iir_Kind.Case_Generate_Statement:
                assoc = nodes.Get_Case_Statement_Alternative_Chain(stmt)
                while assoc != nodes.Null_Iir:
                    self.checkComments(assoc, f"{name}_when")
                    body = nodes.Get_Associated_Block(assoc)
                    if body != nodes.Null_Iir:
                        self.checkBody(body, name)
                    assoc = nodes.Get_Chain(assoc)
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
                self.checkConc(nodes.Get_Concurrent_Statement_Chain(unit))

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
        comments = findall(r"--[^\n]*|/\*.*?\*/", filename.read_text(), DOTALL)
        markers = [comment for comment in comments if search(r":\w+:", comment)]
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

    def test_End(self) -> None:
        """A comment on the line of the end of a block construct belongs to it, not to the next node."""
        self.checkFileAllAttached(self._root / "comments/block_end.vhdl")

    def test_End_NoNewline(self) -> None:
        """The comment on the last line of a file without a final newline."""
        self.checkFileAllAttached(self._root / "comments/block_end_no_newline.vhdl")

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


class Statements(Base):
    """Statements collect comments like declarations in line mode: before them, and on their last line."""

    def test_Concurrent(self) -> None:
        self.checkFileAllAttached(self._root / "comments/stmt_concurrent.vhdl")

    def test_Generate(self) -> None:
        self.checkFileAllAttached(self._root / "comments/stmt_generate.vhdl")

    def test_Sequential(self) -> None:
        self.checkFileAllAttached(self._root / "comments/stmt_sequential.vhdl")

    def test_NoLeak(self) -> None:
        """Comments before an 'end' or a 'begin' are not for the next node, and a comment on 'begin' for no statement."""
        self.checkFileAllAttached(self._root / "comments/stmt_noleak.vhdl")


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


class Delimited(Base):
    """A delimited comment is one comment, the line where it starts decides the same-line rule."""

    def load(self, name) -> tuple:
        """Load file :obj:`name` and return its source file entry and its first library unit."""
        return self.loadPath(self._root / "comments" / name)

    def loadPath(self, filename: Path) -> tuple:
        """Load file :obj:`filename` and return its source file entry and its first library unit."""
        file_id = name_table.Get_Identifier(str(filename))
        sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
        if sfe == files_map.No_Source_File_Entry:
            self.fail(f"Cannot read file '{filename!s}'")

        file = sem_lib.Load_File(sfe)
        return sfe, file, nodes.Get_Library_Unit(nodes.Get_First_Design_Unit(file))

    def test_Association(self) -> None:
        self.checkFileAllAttached(self._root / "comments/delimited.vhdl")

    def test_Text(self) -> None:
        """The text of a delimited comment is as written, also over several lines."""
        sfe, file, entity = self.load("delimited_text.vhdl")

        self.assertEqual(["/* The header,\n   over two lines. */"], self.getComments(sfe, file))
        self.assertEqual(["/* doc of e */"], self.getComments(sfe, nodes.Get_Design_Unit(entity)))

        port = nodes.Get_Port_Chain(entity)
        self.assertEqual(["/* doc of a,\n       over two lines */", "/* line of a,\n       over two lines */"], self.getComments(sfe, port))

        port = nodes.Get_Chain(port)
        self.assertEqual(["/* */"], self.getComments(sfe, port))

    def test_LineEndings(self) -> None:
        """A line break inside a delimited comment is a LF, whether the file stores it as CR+LF or CR."""
        source = (self._root / "comments" / "delimited_text.vhdl").read_bytes().replace(b"\r\n", b"\n")
        with TemporaryDirectory() as directory:
            for name, lineEnding in (("crlf", b"\r\n"), ("cr", b"\r")):
                with self.subTest(lineEnding=name):
                    path = Path(directory) / f"delimited_{name}.vhdl"
                    path.write_bytes(source.replace(b"\n", lineEnding))
                    sfe, file, entity = self.loadPath(path)

                    self.assertEqual(["/* The header,\n   over two lines. */"], self.getComments(sfe, file))
                    port = nodes.Get_Port_Chain(entity)
                    self.assertEqual(
                        ["/* doc of a,\n       over two lines */", "/* line of a,\n       over two lines */"],
                        self.getComments(sfe, port),
                    )
                    port = nodes.Get_Chain(port)
                    self.assertEqual(11, files_map.Location_File_To_Line(nodes.Get_Location(port), sfe))

    def test_LineNumbers(self) -> None:
        """The newlines inside a delimited comment are counted."""
        sfe, file, entity = self.load("delimited_text.vhdl")

        port = nodes.Get_Chain(nodes.Get_Port_Chain(entity))
        self.assertEqual(11, files_map.Location_File_To_Line(nodes.Get_Location(port), sfe))

    def test_Continuation(self) -> None:
        """The column rule uses the start column of the comment."""
        sfe, file, entity = self.load("delimited_continuation.vhdl")

        comments = {}
        port = nodes.Get_Port_Chain(entity)
        while port != nodes.Null_Iir:
            comments[self.getIdentifier(port)] = self.getComments(sfe, port)
            port = nodes.Get_Chain(port)

        self.assertEqual(["/* first */", "/* second */", "-- third"], comments["a"])
        self.assertEqual(["/* first,\n                 on two lines */", "/* second */"], comments["b"])
        self.assertEqual(["/* first */"], comments["c"])
        self.assertEqual(["/* another column */", "/* first */"], comments["d"])
        self.assertEqual(["/* empty line after */"], comments["e"])
        self.assertEqual([], comments["f"])
        self.assertEqual(
            ["/* not a continuation */", "/* inside */", "/* after the token */", "/* and its continuation */"],
            comments["g"],
        )
        self.assertEqual(["/* first */", "/* same line */", "-- also the same line"], comments["h"])
        self.assertEqual(["/* ends on the line\n    */", "/* of the next one */"], comments["i"])


class SingleLine(Base):
    def test_Complex(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Complex_SingleLine.vhdl")


class MultiLine(Base):
    def test_Complex(self) -> None:
        """More exhaustive"""
        self.checkFile(self._root / "Complex_MultiLine.vhdl")

# Empty line before to easy cut & put
