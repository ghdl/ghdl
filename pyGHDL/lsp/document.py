# =============================================================================
#               ____ _   _ ____  _       _
#  _ __  _   _ / ___| | | |  _ \| |     | |___ _ __
# | '_ \| | | | |  _| |_| | | | | |     | / __| '_ \
# | |_) | |_| | |_| |  _  | |_| | |___ _| \__ \ |_) |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|___/ .__/
# |_|    |___/                                |_|
# =============================================================================
# Authors:
#   Tristan Gingold
#
# Package module:   A document (source file) known to the language server.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2023 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
"""
A source file known to the language server.

A :class:`Document` owns the source buffer *libghdl* analyzes and the IIR tree that came out of it, and applies the
edits an editor sends without reloading the file from disk.
"""

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
import pyGHDL.libghdl.vhdl.prints as prints
import pyGHDL.libghdl.file_comments as file_comments

from . import symbols, references

log = logging.getLogger(__name__)


class Document(object):
    """
    A source file known to the language server.

    The document owns the source buffer *libghdl* analyzes and the IIR tree of the last successful analysis. An
    editor's edits are applied to the buffer in place, so the file on disk is never re-read.
    """

    # The encoding used for the files.
    # Unfortunately this is not fully reliable.  The client can read the
    # file using its own view of the encoding.  It then pass the document
    # to the server using unicode(utf-8).  Then the document is converted
    # back to bytes using this encoding.  And we hope the result would be
    # the same as the file.  Because VHDL uses the iso 8859-1 character
    # set, we use the same encoding.  The client should also use 8859-1.
    encoding = "iso-8859-1"

    initial_gap_size = 4096

    def __init__(self, uri, sfe=None, lib=None, version=None):
        """
        Initialize a document from the source file *libghdl* has reserved for it.

        :param uri:     The URI the client identifies this document by.
        :param sfe:     The source file entry holding the text, as returned by :meth:`load`.
        :param lib:     The name of the library the units are analyzed into, or ``None`` for ``work``.
        :param version: The version the client attached to the document, used to detect stale edits.
        """
        self.uri = uri
        self.version = version
        self._fe = sfe
        self.library = lib
        self.gap_size = Document.initial_gap_size
        self._tree = nodes.Null_Iir

    @staticmethod
    def load(src_bytes, dirname, filename):
        """
        Reserve a source file in *libghdl* and fill it with the given text.

        The buffer is over-allocated by :attr:`initial_gap_size`, so the first edits an editor sends fit without
        reallocating it.

        :param src_bytes: The source text, already encoded with :attr:`encoding`.
        :param dirname:   The directory the file lives in. It is ignored when ``filename`` is absolute.
        :param filename:  The name of the file.
        :returns:         The source file entry the text was written to.
        """
        # Write text to file buffer.
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
        """
        Move the source to a larger buffer, because an edit no longer fits in the current one.

        The gap is doubled on every extension, so a document that keeps growing is not reallocated on every
        keystroke. The old source file is copied and freed, which invalidates the previous source file entry.

        :param new_size: The number of bytes the edit needs on top of the current file length.
        """
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
        """
        Replace the whole source of the document.

        :param source: The new source text. It is encoded with :attr:`encoding`, replacing characters that cannot
                       be represented.
        """
        src_bytes = source.encode(Document.encoding, "replace")
        l = len(src_bytes)
        if l >= files_map.Get_Buffer_Length(self._fe):
            self.__extend_source_buffer(l)
        files_map_editor.Fill_Text(self._fe, ctypes.c_char_p(src_bytes), l)

    def __str__(self) -> str:
        """
        Returns the document's URI, which is how the language server protocol identifies a document.

        :returns: A string representation of this document.
        """
        return str(self.uri)

    def apply_change(self, change):
        """
        Apply one incremental edit to the source buffer.

        The replacement is retried once against a larger buffer, because a text longer than the gap fails the first
        time. Line numbers are converted from the protocol's 0-based counting to *libghdl*'s 1-based counting.

        :param change:          A ``TextDocumentContentChangeEvent`` with a ``range`` and the replacing ``text``.
        :raises AssertionError: If the change has no ``range``, which is how a client asking to replace the whole
                                document is rejected.
        """
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
        """
        Compare the server's buffer against the client's text, to catch edits that were applied differently.

        A mismatch is reported by *libghdl* as an internal error; the buffer is left as it is.

        :param text: The document contents as the client sees them.
        """
        log.debug("Checking document: %s", self.uri)

        text_bytes = text.encode(Document.encoding, "replace")

        files_map_editor.Check_Buffer_Content(self._fe, ctypes.c_char_p(text_bytes), len(text_bytes))

    @staticmethod
    def add_to_library(tree, library):
        """
        Move the design units of a parsed file into a library.

        The units are detached from the design file and added one by one, because the library owns them afterwards
        and may replace an older unit of the same name. A unit without a library unit or without an identifier is
        dropped rather than added.

        :param tree:    The design file the parser produced.
        :param library: The name of the target library, or ``None`` for ``work``.
        :returns:       The design file the library holds the units in, or ``Null_Iir`` if none were added.
        """
        # Set the target library
        if library is None:
            library = "work"
        libraries.Work_Library_Name.value = name_table.Get_Identifier(library)
        libraries.Load_Work_Library(False)
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
        """
        Parse the source buffer and put the units it declares into the library.

        The tree is left as ``Null_Iir`` when the file declares no unit, which is not an error - an empty file or a
        file holding only comments reaches this point.

        :raises AssertionError: If the document already has a tree, because a document must be flushed before it is
                                parsed again.
        """
        assert self._tree == nodes.Null_Iir
        tree = sem_lib.Load_File(self._fe)
        if tree == nodes.Null_Iir:
            return
        self._tree = Document.add_to_library(tree, self.library)
        log.debug("add_to_library(%u, '%s') -> %u", tree, self.library, self._tree)
        if self._tree == nodes.Null_Iir:
            return
        nodes.Set_Design_File_Source(self._tree, self._fe)

    def compute_diags(self):
        """
        Parse the document and analyze every unit in it, so the errors reported are semantic ones too.

        The diagnostics themselves are not returned; they are collected by the error handler the workspace
        installed while this runs.
        """
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
        """
        Turn the tree of ``DocumentSymbol`` into the flat list of ``SymbolInformation``.

        The two shapes differ in more than nesting: a location replaces the range, ``detail`` has no counterpart,
        and the nesting is expressed by naming the container. A client that asked for the flat form gets it here.

        :param syms:   The symbols to flatten. They are modified in place.
        :param parent: The symbol the given ones are nested in, or ``None`` at the top level.
        :returns:      The symbols and all their children, in one list.
        """
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
        """
        Answer ``textDocument/documentSymbol`` for this document.

        :returns: The symbols declared in the file, flattened. An empty list if the document has no tree, which is
                  how a file that failed to parse is answered.
        """
        log.debug("document_symbols")
        if self._tree == nodes.Null_Iir:
            return []
        syms = symbols.get_symbols_chain(self._fe, nodes.Get_First_Design_Unit(self._tree))
        return self.flatten_symbols(syms, None)

    def position_to_location(self, position):
        """
        Convert a position in the protocol's coordinates to a *libghdl* location.

        The character offset is added to the location of the line, which assumes one byte per character - the same
        assumption :attr:`encoding` makes.

        :param position: A ``Position``, with its 0-based ``line`` and ``character``.
        :returns:        The location in the source file.
        """
        pos = files_map.File_Line_To_Position(self._fe, position["line"] + 1)
        return files_map.File_Pos_To_Location(self._fe, pos) + position["character"]

    def find_definition(self, position):
        """
        Find the declaration the name under a position refers to.

        :param position: The position the client asked about.
        :returns:        The declaration node, or ``None`` if the position is not on a name.
        """
        loc = self.position_to_location(position)
        return references.find_definition_by_loc(self._tree, loc)

    def hover(self, position):
        """
        Answer ``textDocument/hover`` by reprinting the declaration under a position.

        The comments written above the declaration are shown before it, separated by a rule, so the documentation
        the author wrote is what the reader sees first.

        :param position: The position the client asked about.
        :returns:        A ``Hover`` holding markdown, or ``None`` if there is no declaration at that position.
        """
        loc = self.position_to_location(position)
        t = references.find_definition_by_loc(self._tree, loc)
        if t is None:
            # At least vscode sends an hover request even on spaces.
            log.debug("hover: definition not found at {}.{}".format(position["line"], position["character"]))
            return None

        # Regenerate the declaration
        hand = prints.Allocate_Handle()
        prints.Print_String(t, hand)
        buffer = prints.Get_C_String(hand)
        buf_len = prints.Get_Length(hand)
        if buf_len == 0:
            # Not expected.
            log.info("hover: no string")
            res = None
        else:
            txt = ""
            # Extract comments
            t_loc = nodes.Get_Location(t)
            t_fe = files_map.Location_To_File(t_loc)
            comm = file_comments.Find_First_Comment(t_fe, t)
            while comm != file_comments.No_Comment_Index:
                # Add a comment in 'preformatted' mode
                txt += "    " + file_comments.Get_Comment(t_fe, comm) + "\n"
                comm = file_comments.Get_Next_Comment(t_fe, comm)
            if txt:
                # Add a separation line between comments and declaration.
                txt += "---\n"
            newtext = buffer[:buf_len].decode(Document.encoding)
            txt += "```vhdl\n" + newtext + "\n```"
            res = {"contents": {"kind": "markdown", "value": txt}}
        prints.Free_Handle(hand)
        return res

    def format_range(self, rng):
        """
        Re-indent a range of lines.

        Only whole lines are formatted. A range ending at character 0 does not extend into the line it ends on,
        which is how a client selecting whole lines is handled.

        :param rng: The ``Range`` to format.
        :returns:   A list holding the single ``TextEdit`` that replaces those lines, or ``None`` if the range is
                    empty or the document has no tree.
        """
        first_line = rng["start"]["line"] + 1
        last_line = rng["end"]["line"] + (1 if rng["end"]["character"] != 0 else 0)
        if last_line < first_line:
            return None
        if self._tree == nodes.Null_Iir:
            return None
        hand = prints.Allocate_Handle()
        formatters.Indent_String(self._tree, hand, first_line, last_line)
        buffer = prints.Get_C_String(hand)
        buf_len = prints.Get_Length(hand)
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
        prints.Free_Handle(hand)
        return res
