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
# Package module:   The workspace: the set of documents and the project file describing them.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2026 Tristan Gingold
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
The set of source files the language server knows about.

A :class:`Workspace` maps between the URIs an editor uses and the documents behind them, keeps the project
configuration read from :file:`hdl-prj.json`, and re-analyzes what an edit invalidated.
"""

import logging
import os
import json
from ctypes import byref
import pyGHDL.libghdl as libghdl
import pyGHDL.libghdl.errorout_memory as errorout_memory
import pyGHDL.libghdl.flags as flags
import pyGHDL.libghdl.errorout as errorout
import pyGHDL.libghdl.files_map as files_map
import pyGHDL.libghdl.libraries as libraries
import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.lists as lists
import pyGHDL.libghdl.vhdl.std_package as std_package
import pyGHDL.libghdl.vhdl.parse as parse
import pyGHDL.libghdl.vhdl.sem_lib as sem_lib
import pyGHDL.libghdl.utils as pyutils

from . import lsp
from pyGHDL.lsp import LSPException

from . import document, symbols

log = logging.getLogger(__name__)


class ProjectError(LSPException):
    """The exception is raised in case of an unrecoverable error in the project file."""


class InitError(LSPException):
    """The exception is raised when the workspace could not be initialized."""


class Workspace(object):
    """
    The set of source files the language server knows about.

    The workspace maps between the URIs an editor uses and the :class:`~pyGHDL.lsp.document.Document` objects behind
    them, holds the project configuration read from :file:`hdl-prj.json`, and re-analyzes the documents an edit
    invalidated.
    """

    def __init__(self, root_uri, server):
        """
        Set up *libghdl* for interactive use and load the project.

        The analysis flags differ from a batch run of GHDL: locations and comments are kept because the server
        answers questions about them, and analysis continues past an error because a file being edited is
        incomplete most of the time. The unused, missing-association and sensitivity warnings are enabled, as they
        are the ones worth showing while typing.

        :param root_uri:   The URI of the directory the client opened.
        :param server:     The server the notifications are sent through.
        :raises InitError: If *libghdl* could not be initialized, which leaves the server unable to analyze
                           anything.
        """
        self._root_uri = root_uri
        self._server = server
        self._root_path = lsp.path_from_uri(self._root_uri)
        self._docs = {}  # uri -> doc
        self._fe_map = {}  # fe -> doc
        self._prj = {}
        self._last_linted_doc = None
        errorout_memory.Install_Handler()
        flags.Flag_Elocations.value = True
        # flags.Verbose.value = True
        # Gather comments
        flags.Flag_Gather_Comments.value = True
        # We do analysis even in case of errors.
        parse.Flag_Parse_Parenthesis.value = True
        # Force analysis to get more feedback + navigation even in case
        # of errors.
        flags.Flag_Force_Analysis.value = True
        # Do not consider analysis order issues.
        flags.Flag_Elaborate_With_Outdated.value = True
        libghdl.errorout.Enable_Warning(errorout.Msgid.Warnid_Unused, True)
        libghdl.errorout.Enable_Warning(errorout.Msgid.Warnid_Missing_Assoc, True)
        libghdl.errorout.Enable_Warning(errorout.Msgid.Warnid_Sensitivity, True)
        self.read_project()
        self.set_options_from_project()
        if libghdl.analyze_init_status() != 0:
            log.error("cannot initialize libghdl")
            raise InitError
        self._diags_set = set()  # URIs of the documents diagnostics were last published for.
        self.read_files_from_project()
        self.gather_diagnostics(None)

    @property
    def documents(self):
        """
        Read-only: The documents of this workspace, indexed by their URI.
        """
        return self._docs

    @property
    def root_path(self):
        """
        Read-only: The path of the directory the client opened.
        """
        return self._root_path

    @property
    def root_uri(self):
        """
        Read-only: The URI of the directory the client opened.
        """
        return self._root_uri

    def _create_document(self, doc_uri, sfe, lib, version=None):
        """
        Create a document and register it under both the URI and the source file it uses.

        Both indices are needed: the client names a document by its URI, while *libghdl* reports an error against a
        source file.

        :param doc_uri: The URI the client identifies the document by.
        :param sfe:     The source file entry holding the text.
        :param lib:     The name of the library the units are analyzed into, or ``None`` for ``work``.
        :param version: The version the client attached to the document.
        :returns:       The new document.
        """
        doc = document.Document(doc_uri, sfe, lib, version)
        self._docs[doc_uri] = doc
        self._fe_map[sfe] = doc
        return doc

    def create_document_from_sfe(self, sfe, abspath, lib):
        """
        Create a document for a source file *libghdl* already holds.

        This is how a file that the client never opened becomes reportable: an error in it names a source file, and
        a diagnostic can only be published against a URI.

        :param sfe:     The source file entry the text is already in.
        :param abspath: The absolute path of that file.
        :param lib:     The name of the library the units are analyzed into, or ``None`` for ``work``.
        :returns:       The new document.
        """
        # A filename has been given without a corresponding document.
        # Create the document.
        # Common case: an error message was reported in a non-open document.
        #  Create a document so that it could be reported to the client.
        doc_uri = lsp.path_to_uri(os.path.normpath(abspath))
        return self._create_document(doc_uri, sfe, lib)

    def create_document_from_uri(self, doc_uri, source=None, version=None):
        """
        Create a document for a URI the workspace does not know yet.

        :param doc_uri:            The URI to load. It is assumed to name a readable local file.
        :param source:             The text the client sent, or ``None`` to read the file from disk.
        :param version:            The version the client attached to the document.
        :returns:                  The new document. It is not parsed yet.
        :raises FileNotFoundError: If no source was given and the file cannot be opened.
        """
        # A document is referenced by an uri but not known.  Load it.
        # We assume the path is correct.
        path = lsp.path_from_uri(doc_uri)
        if source is None:
            source = open(path, "rb").read()
        else:
            source = source.encode(document.Document.encoding, "replace")
        sfe = document.Document.load(source, os.path.dirname(path), os.path.basename(path))
        return self._create_document(doc_uri, sfe, None)

    def get_or_create_document(self, doc_uri):
        """
        Get a document, loading and parsing it if the workspace does not know it yet.

        :param doc_uri: The URI of the document.
        :returns:       The document, parsed either now or earlier.
        """
        res = self.get_document(doc_uri)
        if res is not None:
            return res
        res = self.create_document_from_uri(doc_uri)
        res.parse_document()
        return res

    def get_document(self, doc_uri):
        """
        Look up a document by its URI.

        :param doc_uri: The URI of the document.
        :returns:       The document, or ``None`` if the workspace does not know it.
        """
        return self._docs.get(doc_uri)

    def put_document(self, doc_uri, source, version=None):
        """
        Take the client's version of a document, creating it if needed.

        A document loaded from the project is overwritten rather than kept, because the client has been editing it
        and its buffer is the newer one.

        :param doc_uri: The URI of the document.
        :param source:  The text the client sent.
        :param version: The version the client attached to the document.
        :returns:       The document holding that text.
        """
        doc = self.get_document(doc_uri)
        if doc is None:
            doc = self.create_document_from_uri(doc_uri, source=source, version=version)
        else:
            # The document may already be present (loaded from a project)
            # In that case, overwrite it as the client may have a more
            # recent version.
            doc.reload(source)
        return doc

    def sfe_to_document(self, sfe):
        """
        Find the document a source file belongs to, creating one if the file came from outside the workspace.

        :param sfe:             The source file entry, as it appears in an error record.
        :returns:               The document holding that source file.
        :raises AssertionError: If the source file entry is 0, which is not a file.
        """
        assert sfe != 0
        doc = self._fe_map.get(sfe, None)
        if doc is None:
            # Could be a document from outside...
            filename = pyutils.name_image(files_map.Get_File_Name(sfe))
            if not os.path.isabs(filename):
                dirname = pyutils.name_image(files_map.Get_Directory_Name(sfe))
                filename = os.path.join(dirname, filename)
            doc = self.create_document_from_sfe(sfe, filename, None)
        return doc

    def add_vhdl_file(self, name, lib):
        """
        Load and parse one VHDL file listed in the project.

        A file that cannot be read is reported to the client and skipped, so one bad entry in the project does not
        stop the rest of it from loading.

        :param name: The file name, relative to the root path or absolute.
        :param lib:  The name of the library the units are analyzed into, or ``None`` for ``work``.
        """
        log.info("loading %s in library %s", name, lib)
        if os.path.isabs(name):
            absname = name
        else:
            absname = os.path.join(self._root_path, name)
        # Create a document for this file.
        try:
            fd = open(absname, "rb")
            sfe = document.Document.load(fd.read(), self._root_path, name)
            fd.close()
        except OSError as err:
            self._server.show_message(lsp.MessageType.Error, f"cannot load {name}: {err.strerror}")
            return
        doc = self.create_document_from_sfe(sfe, absname, lib)
        doc.parse_document()

    def read_project(self):
        """
        Read :file:`hdl-prj.json` from the root path, if there is one.

        A missing project file is normal - the server then works on the open documents alone. A file that exists
        but cannot be read or parsed is reported to the client, and the workspace keeps its empty configuration.
        """
        prj_file = os.path.join(self.root_path, "hdl-prj.json")
        if not os.path.exists(prj_file):
            log.info("project file %s does not exist", prj_file)
            return
        try:
            f = open(prj_file)
        except OSError as err:
            self._server.show_message(
                lsp.MessageType.Error,
                f"cannot open project file {prj_file}: {err.strerror}",
            )
            return
        log.info("reading project file %s", prj_file)
        try:
            self._prj = json.load(f)
        except json.decoder.JSONDecodeError as e:
            log.info("error in project file")
            self._server.show_message(
                lsp.MessageType.Error,
                f"json error in project file {prj_file}:{e.lineno}:{e.colno}",
            )
        f.close()

    def set_options_from_project(self):
        """
        Pass the ``options.ghdl_analysis`` entries of the project to *libghdl* as analysis options.

        A malformed project or a rejected option is reported to the client rather than raised, because the server
        stays useful with the options it did accept.
        """
        try:
            if self._prj is None:
                return
            if not isinstance(self._prj, dict):
                raise ProjectError("project file is not a dictionnary")
            opts = self._prj.get("options", None)
            if opts is None:
                return
            if not isinstance(opts, dict):
                raise ProjectError("'options' is not a dictionnary")
            ghdl_opts = opts.get("ghdl_analysis", None)
            if ghdl_opts is None:
                return
            log.info("Using options: %s", ghdl_opts)
            for opt in ghdl_opts:
                if not libghdl.set_option(opt):
                    self._server.show_message(lsp.MessageType.Error, f"error with option: {opt}")
        except ProjectError as e:
            self._server.show_message(lsp.MessageType.Error, f"error in project file: {e}")

    def read_files_from_project(self):
        """
        Load every VHDL file the project lists.

        Files of another language are skipped rather than rejected. A malformed ``files`` entry stops the loading
        and is reported to the client.
        """
        try:
            files = self._prj.get("files", [])
            if not isinstance(files, list):
                raise ProjectError("'files' is not a list")
            for f in files:
                if not isinstance(f, dict):
                    raise ProjectError("an element of 'files' is not a dict")
                name = f.get("file")
                if not isinstance(name, str):
                    raise ProjectError("a 'file' is not a string")
                lang = f.get("language", "vhdl")
                lib = f.get("library", None)
                if lang == "vhdl":
                    self.add_vhdl_file(name, lib)
        except ProjectError as e:
            self._server.show_message(lsp.MessageType.Error, f"error in project file: {e}")

    def get_configuration(self):
        """
        Ask the client for the ``vhdl.maxNumberOfProblems`` setting.

        The reply arrives as a separate message, so nothing is returned here.
        """
        self._server.configuration([{"scopeUri": "", "section": "vhdl.maxNumberOfProblems"}])

    def gather_diagnostics(self, doc):
        """
        Turn the messages *libghdl* collected into diagnostics and publish them, one notification per file.

        A message belonging to a group beyond the main one is not a diagnostic of its own; it is attached to the
        preceding one as related information, which is how the two halves of "declaration is here, use is there"
        stay together. The messages are cleared once they have been read.

        A client keeps the diagnostics of a file until it is sent new ones, so every document that had diagnostics
        published for it and has none now is sent an empty list. The documents that carry diagnostics are tracked in
        :attr:`_diags_set` for that, because analyzing one document can equally clear a diagnostic in another.

        :param doc: The document that was analyzed, so an empty list can be published for it when its errors are
                    gone. ``None`` when the whole project was analyzed.
        """
        # Gather messages (per file)
        nbr_msgs = errorout_memory.Get_Nbr_Messages()
        diags = {}
        diag = {}
        for i in range(nbr_msgs):
            hdr = errorout_memory.Get_Error_Record(i + 1)
            msg = errorout_memory.Get_Error_Message(i + 1)
            if hdr.file == 0:
                # Possible for error limit reached.
                continue
            err_range = {
                "start": {"line": hdr.line - 1, "character": hdr.offset},
                "end": {"line": hdr.line - 1, "character": hdr.offset + hdr.length},
            }
            if hdr.group <= errorout_memory.Msg_Main:
                if hdr.id <= errorout.Msgid.Msgid_Note:
                    severity = lsp.DiagnosticSeverity.Information
                elif hdr.id <= errorout.Msgid.Msgid_Warning:
                    severity = lsp.DiagnosticSeverity.Warning
                else:
                    severity = lsp.DiagnosticSeverity.Error
                diag = {
                    "source": "ghdl",
                    "range": err_range,
                    "message": msg,
                    "severity": severity,
                }
                if hdr.group == errorout_memory.Msg_Main:
                    diag["relatedInformation"] = []
                fdiag = diags.get(hdr.file, None)
                if fdiag is None:
                    diags[hdr.file] = [diag]
                else:
                    fdiag.append(diag)
            else:
                assert diag
                relatedDocument = self.sfe_to_document(hdr.file)
                diag["relatedInformation"].append(
                    {
                        "location": {"uri": relatedDocument.uri, "range": err_range},
                        "message": msg,
                    }
                )
        errorout_memory.Clear_Errors()
        # Publish diagnostics
        publishedURIs = set()
        for sfe, diag in diags.items():
            diagnosedDocument = self.sfe_to_document(sfe)
            self.publish_diagnostics(diagnosedDocument.uri, diag)
            publishedURIs.add(diagnosedDocument.uri)
        # Clear previous diagnostics of the analyzed document and of every document that has none left.
        staleURIs = set(self._diags_set)
        if doc is not None:
            staleURIs.add(doc.uri)
        for uri in staleURIs - publishedURIs:
            self.publish_diagnostics(uri, [])
        self._diags_set = publishedURIs

    def obsolete_dependent_units(self, unit, antideps):
        """
        Mark every unit that depends on the given one as no longer analyzed, transitively.

        A unit is put back into the state it had before analysis, and its dependence list is freed. Its position in
        the source is written into the node first, because that is what is left to find it by once its tree is
        gone.

        The recursion is broken by clearing the entry as it is taken, so a cycle between two units does not loop
        forever.

        :param unit:     The unit whose dependents are to be obsoleted.
        :param antideps: The anti-dependencies, as returned by :meth:`compute_anti_dependences`. It is modified.
        """
        udeps = antideps.get(unit, None)
        if udeps is None:
            # There are no units.
            return
        # Avoid infinite recursion
        antideps[unit] = None
        for un in udeps:
            log.debug("obsolete %d %s", un, pyutils.name_image(nodes.Get_Identifier(un)))
            # Recurse
            self.obsolete_dependent_units(un, antideps)
            if nodes.Get_Date_State(un) == nodes.DateStateType.Disk:
                # Already obsolete!
                continue
            # FIXME: just de-analyze ?
            nodes.Set_Date_State(un, nodes.DateStateType.Disk)
            sem_lib.Free_Dependence_List(un)
            loc = nodes.Get_Location(un)
            fil = files_map.Location_To_File(loc)
            pos = files_map.Location_File_To_Pos(loc, fil)
            line = files_map.Location_File_To_Line(loc, fil)
            col = files_map.Location_File_Line_To_Offset(loc, fil, line)
            nodes.Set_Design_Unit_Source_Pos(un, pos)
            nodes.Set_Design_Unit_Source_Line(un, line)
            nodes.Set_Design_Unit_Source_Col(un, col)

    def obsolete_doc(self, doc):
        """
        Throw away the analysis of a document, and of everything that was analyzed against it.

        Dropping the units of one file is not enough: another file that used a package from it holds references
        into the tree being freed, so those units are obsoleted too. The design file is then purged from its
        library, which leaves the document ready to be parsed again.

        :param doc: The document to obsolete. A document without a tree is left alone.
        """
        if doc._tree == nodes.Null_Iir:
            return
        # Free old tree
        assert nodes.Get_Kind(doc._tree) == nodes.Iir_Kind.Design_File
        if self._last_linted_doc == doc:
            antideps = None
        else:
            antideps = self.compute_anti_dependences()
        unit = nodes.Get_First_Design_Unit(doc._tree)
        while unit != nodes.Null_Iir:
            if antideps is not None:
                self.obsolete_dependent_units(unit, antideps)
            # FIXME: free unit; it is not referenced.
            unit = nodes.Get_Chain(unit)
        libraries.Purge_Design_File(doc._tree)
        doc._tree = nodes.Null_Iir

    def lint(self, doc_uri):
        """
        Re-analyze a document from its current buffer and publish what that found.

        :param doc_uri: The URI of the document to check.
        """
        doc = self.get_document(doc_uri)
        self.obsolete_doc(doc)
        doc.compute_diags()
        self.gather_diagnostics(doc)

    def apply_changes(self, doc_uri, contentChanges, new_version):
        """
        Apply the edits of a ``textDocument/didChange`` and re-analyze the document.

        An edit that does not fit moves the text to a new source file, so the map from source files to documents is
        corrected afterwards.

        :param doc_uri:         The URI of the document that changed.
        :param contentChanges:  The edits, applied in the order the client sent them.
        :param new_version:     The version the client attached to the document.
        :raises AssertionError: If the document is not loaded, because an edit can only be applied to a buffer that
                                exists.
        """
        doc = self.get_document(doc_uri)
        assert doc is not None, "try to modify a non-loaded document"
        self.obsolete_doc(doc)
        prev_sfe = doc._fe
        for change in contentChanges:
            doc.apply_change(change)
        if doc._fe != prev_sfe:
            del self._fe_map[prev_sfe]
            self._fe_map[doc._fe] = doc
        # Like lint
        doc.compute_diags()
        self.gather_diagnostics(doc)

    def check_document(self, doc_uri, source):
        """
        Compare the buffer of a document against the client's text.

        :param doc_uri:   The URI of the document to check.
        :param source:    The document contents as the client sees them.
        :raises KeyError: If the workspace does not know that URI.
        """
        self._docs[doc_uri].check_document(source)

    def rm_document(self, doc_uri):
        """
        Drop the diagnostics of a document the client closed.

        The client does not discard them on its own, so an empty list has to be published.

        :param doc_uri: The URI of the closed document.
        """
        # Clear diagnostics as it's not done automatically.
        self.publish_diagnostics(doc_uri, [])
        self._diags_set.discard(doc_uri)

    def apply_edit(self, edit):
        """
        Ask the client to apply an edit to the workspace.

        :param edit: The ``WorkspaceEdit`` to apply.
        :returns:    The identifier of the request, which the reply will carry.
        """
        return self._server.request("workspace/applyEdit", {"edit": edit})

    def publish_diagnostics(self, doc_uri, diagnostics):
        """
        Send the diagnostics of one document to the client.

        :param doc_uri:     The URI the diagnostics belong to.
        :param diagnostics: The diagnostics, replacing the ones sent before. An empty list clears them.
        """
        self._server.notify(
            "textDocument/publishDiagnostics",
            params={"uri": doc_uri, "diagnostics": diagnostics},
        )

    def show_message(self, message, msg_type=lsp.MessageType.Info):
        """
        Show a message in the client's user interface.

        :param message:  The text to show.
        :param msg_type: How the client should present it.
        """
        self._server.notify("window/showMessage", params={"type": msg_type, "message": message})

    def declaration_to_location(self, decl, decl_name):
        """
        Convert a declaration to a location the client can jump to.

        The range covers the declared name, whose length comes from the identifier. The two nodes are separate
        because a jump may target one node while naming another - going to the implementation of a subprogram
        lands on the body but measures the name of the declaration.

        :param decl:      The node whose location is used.
        :param decl_name: The node holding the identifier.
        :returns:         A ``Location``, or ``None`` for a declaration that has no place in a file - the ``std``
                          library and the library declarations themselves are virtual.
        """
        decl_loc = nodes.Get_Location(decl)
        if decl_loc == std_package.Std_Location.value:
            # There is no real file for the std.standard package.
            return None
        if decl_loc == libraries.Library_Location.value:
            # Libraries declaration are virtual.
            return None
        fe = files_map.Location_To_File(decl_loc)
        doc = self.sfe_to_document(fe)
        res = {"uri": doc.uri}
        nid = nodes.Get_Identifier(decl_name)
        res["range"] = {
            "start": symbols.location_to_position(fe, decl_loc),
            "end": symbols.location_to_position(fe, decl_loc + name_table.Get_Name_Length(nid)),
        }
        return res

    def goto_definition(self, doc_uri, position):
        """
        Answer ``textDocument/definition``.

        :param doc_uri:  The URI of the document the client asked from.
        :param position: The position of the name to resolve.
        :returns:        A list holding the single location of the declaration, or ``None`` if there is no name at
                         that position or its declaration is virtual.
        """
        decl = self._docs[doc_uri].find_definition(position)
        if decl is None:
            return None
        decl_loc = self.declaration_to_location(decl, decl)
        if decl_loc is None:
            return None
        return [decl_loc]

    def goto_implementation(self, doc_uri, position):
        """
        Answer ``textDocument/implementation``, which asks for the body behind a declaration.

        A subprogram leads to its body, and a component to an entity of the same name. The component case is a
        guess - a configuration may bind the component to something else - but it is the answer a reader expects.
        Where no body is known yet, the declaration itself is returned rather than nothing.

        :param doc_uri:  The URI of the document the client asked from.
        :param position: The position of the name to resolve.
        :returns:        A list holding the single location of the implementation, or ``None`` if there is none.
        """
        decl = self._docs[doc_uri].find_definition(position)
        if decl is None:
            return None
        k = nodes.Get_Kind(decl)
        bod = decl
        if k == nodes.Iir_Kind.Component_Declaration:
            # The implementation of a component is an entity of the same name
            # Not strictly correct but good enough.
            ent = libraries.Find_Entity_For_Component(nodes.Get_Identifier(decl))
            if ent != nodes.Null_Iir:
                decl = nodes.Get_Library_Unit(ent)
                bod = decl
        elif k in nodes.Iir_Kinds.Subprogram_Declaration:
            bod = nodes.Get_Subprogram_Body(decl)
            if bod == nodes.Null_Iir:
                # Body not yet known, simply refer to the decl
                bod = decl

        if bod == nodes.Null_Iir or decl == nodes.Null_Iir:
            return None
        decl_loc = self.declaration_to_location(bod, decl)
        if decl_loc is None:
            return None
        return [decl_loc]

    def hover(self, doc_uri, position):
        """
        Answer ``textDocument/hover``.

        :param doc_uri:  The URI of the document the client asked from.
        :param position: The position the client is hovering over.
        :returns:        A ``Hover``, or ``None`` if there is no declaration at that position.
        """
        return self._docs[doc_uri].hover(position)

    def x_show_all_files(self):
        """
        List every source file *libghdl* holds, for the ``workspace/xShowAllFiles`` extension.

        This is a debugging aid: it shows the files the server loaded, including the ones no client ever opened,
        which is why the URI may be ``None``.

        :returns: One entry per source file, with its entry number, URI, name and directory.
        """
        res = []
        for fe in range(1, files_map.Get_Last_Source_File_Entry() + 1):
            doc = self._fe_map.get(fe, None)
            res.append(
                {
                    "fe": fe,
                    "uri": doc.uri if doc is not None else None,
                    "name": pyutils.name_image(files_map.Get_File_Name(fe)),
                    "dir": pyutils.name_image(files_map.Get_Directory_Name(fe)),
                }
            )
        return res

    def x_get_all_entities(self):
        """
        List every entity of every library, for the ``workspace/xGetAllEntities`` extension.

        A client uses this to offer the entities that can be instantiated.

        :returns: One entry per entity, with its name and the library it lives in.
        """
        res = []
        lib = libraries.Get_Libraries_Chain()
        while lib != nodes.Null_Iir:
            files = nodes.Get_Design_File_Chain(lib)
            ents = []
            while files != nodes.Null_Iir:
                units = nodes.Get_First_Design_Unit(files)
                while units != nodes.Null_Iir:
                    unitlib = nodes.Get_Library_Unit(units)
                    if nodes.Get_Kind(unitlib) == nodes.Iir_Kind.Entity_Declaration:
                        ents.append(unitlib)
                    units = nodes.Get_Chain(units)
                files = nodes.Get_Chain(files)
            ents = [pyutils.name_image(nodes.Get_Identifier(e)) for e in ents]
            lib_name = pyutils.name_image(nodes.Get_Identifier(lib))
            res.extend([{"name": n, "library": lib_name} for n in ents])
            lib = nodes.Get_Chain(lib)
        return res

    def x_get_entity_interface(self, library, name):
        """
        Report the generics and ports of one entity, for the ``workspace/xGetEntityInterface`` extension.

        A client uses this to write the instantiation of an entity the user picked.

        :param library: The name of the library the entity lives in.
        :param name:    The name of the entity.
        :returns:       The entity with its generics and ports, or ``None`` if either the library or the entity is
                        unknown.
        """

        def create_interfaces(inters):
            """
            Collect the names of an interface chain.

            :param inters: The first interface of the chain.
            :returns:      One entry per interface, holding its name.
            """
            res = []
            while inters != nodes.Null_Iir:
                res.append({"name": name_table.Get_Name_Ptr(nodes.Get_Identifier(inters))})
                inters = nodes.Get_Chain(inters)
            return res

        # Find library
        lib_id = name_table.Get_Identifier(library)
        lib = libraries.Get_Library_No_Create(lib_id)
        if lib == name_table.Null_Identifier:
            return None
        # Find entity
        ent_id = name_table.Get_Identifier(name)
        unit = libraries.Find_Primary_Unit(lib, ent_id)
        if unit == nodes.Null_Iir:
            return None
        ent = nodes.Get_Library_Unit(unit)
        return {
            "library": library,
            "entity": name,
            "generics": create_interfaces(nodes.Get_Generic_Chain(ent)),
            "ports": create_interfaces(nodes.Get_Port_Chain(ent)),
        }

    def compute_anti_dependences(self):
        """
        Build the reverse of the dependency graph: which units were analyzed against each unit.

        A unit records what it depends on, and re-analysis needs the opposite question answered - editing a package
        has to invalidate its users. Only analyzed units are walked, because that is when the dependence list is
        filled in.

        :returns:               A mapping from a design unit to the units depending on it. A unit nothing depends
                                on is absent rather than mapped to an empty list.
        :raises AssertionError: If a dependence is neither a design unit nor an entity aspect, which would mean the
                                dependence list holds something this does not know how to follow.
        """
        res = {}
        lib = libraries.Get_Libraries_Chain()
        while lib != nodes.Null_Iir:
            files = nodes.Get_Design_File_Chain(lib)
            while files != nodes.Null_Iir:
                units = nodes.Get_First_Design_Unit(files)
                while units != nodes.Null_Iir:
                    if nodes.Get_Date_State(units) == nodes.DateStateType.Analyze:
                        # The unit has been analyzed, so the dependencies are know.
                        deps = nodes.Get_Dependence_List(units)
                        assert deps != nodes.Null_Iir_List
                        deps_it = lists.Iterate(deps)
                        while lists.Is_Valid(byref(deps_it)):
                            el = lists.Get_Element(byref(deps_it))
                            if nodes.Get_Kind(el) == nodes.Iir_Kind.Design_Unit:
                                ent = el
                            elif nodes.Get_Kind(el) == nodes.Iir_Kind.Entity_Aspect_Entity:
                                # Extract design unit from entity aspect
                                # Do not care about the architecture.
                                ent = nodes.Get_Entity_Name(el)
                                ent = nodes.Get_Named_Entity(ent)
                                ent = nodes.Get_Design_Unit(ent)
                            else:
                                assert False, pyutils.kind_image(nodes.Get_Kind(el))
                            assert nodes.Get_Kind(ent) == nodes.Iir_Kind.Design_Unit
                            if res.get(ent, None):
                                res[ent].append(units)
                            else:
                                res[ent] = [units]
                            lists.Next(byref(deps_it))
                    units = nodes.Get_Chain(units)
                files = nodes.Get_Chain(files)
            lib = nodes.Get_Chain(lib)
        return res
