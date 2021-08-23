import ctypes
import logging
import os
import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.files_map as files_map
import pyGHDL.libghdl.files_map_editor as files_map_editor
import pyGHDL.libghdl.libraries as libraries
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.sem_lib as sem_lib
import pyGHDL.libghdl.vhdl.sem as sem
import pyGHDL.libghdl.vhdl.formatters as formatters

from . import symbols, references

log = logging.getLogger(__name__)


class Document(object):
    # The encoding used for the files.
    # Unfortunately this is not fully reliable.  The client can read the
    # file using its own view of the encoding.  It then pass the document
    # to the server using unicode(utf-8).  Then the document is converted
    # back to bytes using this encoding.  And we hope the result would be
    # the same as the file.  Because VHDL uses the iso 8859-1 character
    # set, we use the same encoding.  The client should also use 8859-1.
    encoding = "iso-8859-1"

    initial_gap_size = 4096

    def __init__(self, uri, sfe=None, version=None):
        self.uri = uri
        self.version = version
        self._fe = sfe
        self.gap_size = Document.initial_gap_size
        self._tree = nodes.Null_Iir

    @staticmethod
    def load(source, dirname, filename):
        # Write text to file buffer.
        src_bytes = source.encode(Document.encoding, "replace")
        src_len = len(src_bytes)
        buf_len = src_len + Document.initial_gap_size
        fileid = name_table.Get_Identifier(filename)
        if os.path.isabs(filename):
            dirid = name_table.Null_Identifier
        else:
            dirid = name_table.Get_Identifier(dirname)
        sfe = files_map.Reserve_Source_File(dirid, fileid, buf_len)
        files_map_editor.Fill_Text(sfe, ctypes.c_char_p(src_bytes), src_len)
        return sfe

    def __extend_source_buffer(self, new_size):
        self.gap_size *= 2
        fileid = files_map.Get_File_Name(self._fe)
        dirid = files_map.Get_Directory_Name(self._fe)
        buf_len = files_map.Get_File_Length(self._fe) + new_size + self.gap_size
        files_map.Discard_Source_File(self._fe)
        new_sfe = files_map.Reserve_Source_File(dirid, fileid, buf_len)
        files_map_editor.Copy_Source_File(new_sfe, self._fe)
        files_map.Free_Source_File(self._fe)
        self._fe = new_sfe

    def reload(self, source):
        """Reload the source of a document."""
        src_bytes = source.encode(Document.encoding, "replace")
        l = len(src_bytes)
        if l >= files_map.Get_Buffer_Length(self._fe):
            self.__extend_source_buffer(l)
        files_map_editor.Fill_Text(self._fe, ctypes.c_char_p(src_bytes), l)

    def __str__(self):
        return str(self.uri)

    def apply_change(self, change):
        """Apply a change to the document."""
        text = change["text"]
        change_range = change.get("range")

        text_bytes = text.encode(Document.encoding, "replace")

        if not change_range:
            # The whole file has changed
            raise AssertionError
            # if len(text_bytes) < libghdl.Files_Map.Get_Buffer_Length(self._fe):
            #    xxxx_replace
            # else:
            #    xxxx_free
            #    xxxx_allocate
            # return

        start_line = change_range["start"]["line"]
        start_col = change_range["start"]["character"]
        end_line = change_range["end"]["line"]
        end_col = change_range["end"]["character"]

        status = files_map_editor._Replace_Text(
            self._fe,
            start_line + 1,
            start_col,
            end_line + 1,
            end_col,
            ctypes.c_char_p(text_bytes),
            len(text_bytes),
        )
        if status:
            return

        # Failed to replace text.
        # Increase size
        self.__extend_source_buffer(len(text_bytes))
        status = files_map_editor._Replace_Text(
            self._fe,
            start_line + 1,
            start_col,
            end_line + 1,
            end_col,
            ctypes.c_char_p(text_bytes),
            len(text_bytes),
        )
        assert status

    def check_document(self, text):
        log.debug("Checking document: %s", self.uri)

        text_bytes = text.encode(Document.encoding, "replace")

        files_map_editor.Check_Buffer_Content(self._fe, ctypes.c_char_p(text_bytes), len(text_bytes))

    @staticmethod
    def add_to_library(tree):
        # Detach the chain of units.
        unit = nodes.Get_First_Design_Unit(tree)
        nodes.Set_First_Design_Unit(tree, nodes.Null_Iir)
        # FIXME: free the design file ?
        tree = nodes.Null_Iir
        # Analyze unit after unit.
        while unit != nodes.Null_Iir:
            # Pop the first unit.
            next_unit = nodes.Get_Chain(unit)
            nodes.Set_Chain(unit, nodes.Null_Iir)
            lib_unit = nodes.Get_Library_Unit(unit)
            if lib_unit != nodes.Null_Iir and nodes.Get_Identifier(unit) != name_table.Null_Identifier:
                # Put the unit (only if it has a library unit) in the library.
                libraries.Add_Design_Unit_Into_Library(unit, False)
                tree = nodes.Get_Design_File(unit)
            unit = next_unit
        return tree

    def parse_document(self):
        """Parse a document and put the units in the library."""
        assert self._tree == nodes.Null_Iir
        tree = sem_lib.Load_File(self._fe)
        if tree == nodes.Null_Iir:
            return
        self._tree = Document.add_to_library(tree)
        log.debug("add_to_library(%u) -> %u", tree, self._tree)
        if self._tree == nodes.Null_Iir:
            return
        nodes.Set_Design_File_Source(self._tree, self._fe)

    def compute_diags(self):
        log.debug("parse doc %d %s", self._fe, self.uri)
        self.parse_document()
        if self._tree == nodes.Null_Iir:
            # No units, nothing to add.
            return
        # Semantic analysis.
        unit = nodes.Get_First_Design_Unit(self._tree)
        while unit != nodes.Null_Iir:
            sem.Semantic(unit)
            nodes.Set_Date_State(unit, nodes.DateStateType.Analyze)
            unit = nodes.Get_Chain(unit)

    def flatten_symbols(self, syms, parent):
        res = []
        for s in syms:
            s["location"] = {"uri": self.uri, "range": s["range"]}
            del s["range"]
            s.pop("detail", None)
            if parent is not None:
                s["containerName"] = parent
            res.append(s)
            children = s.pop("children", None)
            if children is not None:
                res.extend(self.flatten_symbols(children, s))
        return res

    def document_symbols(self):
        log.debug("document_symbols")
        if self._tree == nodes.Null_Iir:
            return []
        syms = symbols.get_symbols_chain(self._fe, nodes.Get_First_Design_Unit(self._tree))
        return self.flatten_symbols(syms, None)

    def position_to_location(self, position):
        pos = files_map.File_Line_To_Position(self._fe, position["line"] + 1)
        return files_map.File_Pos_To_Location(self._fe, pos) + position["character"]

    def goto_definition(self, position):
        loc = self.position_to_location(position)
        return references.goto_definition(self._tree, loc)

    def format_range(self, rng):
        first_line = rng["start"]["line"] + 1
        last_line = rng["end"]["line"] + (1 if rng["end"]["character"] != 0 else 0)
        if last_line < first_line:
            return None
        if self._tree == nodes.Null_Iir:
            return None
        hand = formatters.Allocate_Handle()
        formatters.Indent_String(self._tree, hand, first_line, last_line)
        buffer = formatters.Get_C_String(hand)
        buf_len = formatters.Get_Length(hand)
        newtext = buffer[:buf_len].decode(Document.encoding)
        res = [
            {
                "range": {
                    "start": {"line": first_line - 1, "character": 0},
                    "end": {"line": last_line, "character": 0},
                },
                "newText": newtext,
            }
        ]
        formatters.Free_Handle(hand)
        return res
