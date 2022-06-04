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
from . import document, symbols

log = logging.getLogger(__name__)


class ProjectError(Exception):
    """Exception raised in case of unrecoverable error in the project file."""

    def __init__(self, msg):
        super().__init__()
        self.msg = msg


class InitError(Exception):
    pass


class Workspace(object):
    def __init__(self, root_uri, server):
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
        # We do analysis even in case of errors.
        parse.Flag_Parse_Parenthesis.value = True
        # Force analysis to get more feedback + navigation even in case
        # of errors.
        flags.Flag_Force_Analysis.value = True
        # Do not consider analysis order issues.
        flags.Flag_Elaborate_With_Outdated.value = True
        libghdl.errorout.Enable_Warning(errorout.Msgid.Warnid_Unused, True)
        libghdl.errorout.Enable_Warning(errorout.Msgid.Warnid_No_Assoc, True)
        self.read_project()
        self.set_options_from_project()
        if libghdl.analyze_init_status() != 0:
            log.error("cannot initialize libghdl")
            raise InitError
        self._diags_set = set()  # Documents with at least one diagnostic.
        self.read_files_from_project()
        self.gather_diagnostics(None)

    @property
    def documents(self):
        return self._docs

    @property
    def root_path(self):
        return self._root_path

    @property
    def root_uri(self):
        return self._root_uri

    def _create_document(self, doc_uri, sfe, version=None):
        """Create a document and put it in this workspace."""
        doc = document.Document(doc_uri, sfe, version)
        self._docs[doc_uri] = doc
        self._fe_map[sfe] = doc
        return doc

    def create_document_from_sfe(self, sfe, abspath):
        # A filename has been given without a corresponding document.
        # Create the document.
        # Common case: an error message was reported in a non-open document.
        #  Create a document so that it could be reported to the client.
        doc_uri = lsp.path_to_uri(os.path.normpath(abspath))
        return self._create_document(doc_uri, sfe)

    def create_document_from_uri(self, doc_uri, source=None, version=None):
        # A document is referenced by an uri but not known.  Load it.
        # We assume the path is correct.
        path = lsp.path_from_uri(doc_uri)
        if source is None:
            source = open(path).read()
        sfe = document.Document.load(source, os.path.dirname(path), os.path.basename(path))
        return self._create_document(doc_uri, sfe)

    def get_or_create_document(self, doc_uri):
        res = self.get_document(doc_uri)
        if res is not None:
            return res
        res = self.create_document_from_uri(doc_uri)
        res.parse_document()
        return res

    def get_document(self, doc_uri):
        """
        Get a document from :param doc_uri:

        Note that the document may not exist, and this function may return None.
        """
        return self._docs.get(doc_uri)

    def put_document(self, doc_uri, source, version=None):
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
        Get the document correspond to :param sfe: source file.

        Can create the document if needed.
        """
        assert sfe != 0
        doc = self._fe_map.get(sfe, None)
        if doc is None:
            # Could be a document from outside...
            filename = pyutils.name_image(files_map.Get_File_Name(sfe))
            if not os.path.isabs(filename):
                dirname = pyutils.name_image(files_map.Get_Directory_Name(sfe))
                filename = os.path.join(dirname, filename)
            doc = self.create_document_from_sfe(sfe, filename)
        return doc

    def add_vhdl_file(self, name):
        log.info("loading %s", name)
        if os.path.isabs(name):
            absname = name
        else:
            absname = os.path.join(self._root_path, name)
        # Create a document for this file.
        try:
            fd = open(absname)
            sfe = document.Document.load(fd.read(), self._root_path, name)
            fd.close()
        except OSError as err:
            self._server.show_message(lsp.MessageType.Error, "cannot load {}: {}".format(name, err.strerror))
            return
        doc = self.create_document_from_sfe(sfe, absname)
        doc.parse_document()

    def read_project(self):
        prj_file = os.path.join(self.root_path, "hdl-prj.json")
        if not os.path.exists(prj_file):
            log.info("project file %s does not exist", prj_file)
            return
        try:
            f = open(prj_file)
        except OSError as err:
            self._server.show_message(
                lsp.MessageType.Error,
                "cannot open project file {}: {}".format(prj_file, err.strerror),
            )
            return
        log.info("reading project file %s", prj_file)
        try:
            self._prj = json.load(f)
        except json.decoder.JSONDecodeError as e:
            log.info("error in project file")
            self._server.show_message(
                lsp.MessageType.Error,
                "json error in project file {}:{}:{}".format(prj_file, e.lineno, e.colno),
            )
        f.close()

    def set_options_from_project(self):
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
                    self._server.show_message(lsp.MessageType.Error, "error with option: {}".format(opt))
        except ProjectError as e:
            self._server.show_message(lsp.MessageType.Error, "error in project file: {}".format(e.msg))

    def read_files_from_project(self):
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
                if lang == "vhdl":
                    self.add_vhdl_file(name)
        except ProjectError as e:
            self._server.show_message(lsp.MessageType.Error, "error in project file: {}".format(e.msg))

    def get_configuration(self):
        self._server.configuration([{"scopeUri": "", "section": "vhdl.maxNumberOfProblems"}])

    def gather_diagnostics(self, doc):
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
                if True:
                    doc = self.sfe_to_document(hdr.file)
                    diag["relatedInformation"].append(
                        {
                            "location": {"uri": doc.uri, "range": err_range},
                            "message": msg,
                        }
                    )
        errorout_memory.Clear_Errors()
        # Publish diagnostics
        for sfe, diag in diags.items():
            doc = self.sfe_to_document(sfe)
            self.publish_diagnostics(doc.uri, diag)
        if doc is not None and doc._fe not in diags:
            # Clear previous diagnostics for the doc.
            self.publish_diagnostics(doc.uri, [])

    def obsolete_dependent_units(self, unit, antideps):
        """Obsolete units that depends of :param unit:."""
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
        doc = self.get_document(doc_uri)
        self.obsolete_doc(doc)
        doc.compute_diags()
        self.gather_diagnostics(doc)

    def apply_changes(self, doc_uri, contentChanges, new_version):
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
        self._docs[doc_uri].check_document(source)

    def rm_document(self, doc_uri):
        pass

    def apply_edit(self, edit):
        return self._server.request("workspace/applyEdit", {"edit": edit})

    def publish_diagnostics(self, doc_uri, diagnostics):
        self._server.notify(
            "textDocument/publishDiagnostics",
            params={"uri": doc_uri, "diagnostics": diagnostics},
        )

    def show_message(self, message, msg_type=lsp.MessageType.Info):
        self._server.notify("window/showMessage", params={"type": msg_type, "message": message})

    def declaration_to_location(self, decl):
        """Convert declaration :param decl: to an LSP Location."""
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
        nid = nodes.Get_Identifier(decl)
        res["range"] = {
            "start": symbols.location_to_position(fe, decl_loc),
            "end": symbols.location_to_position(fe, decl_loc + name_table.Get_Name_Length(nid)),
        }
        return res

    def goto_definition(self, doc_uri, position):
        decl = self._docs[doc_uri].goto_definition(position)
        if decl is None:
            return None
        decl_loc = self.declaration_to_location(decl)
        if decl_loc is None:
            return None
        res = [decl_loc]
        if nodes.Get_Kind(decl) == nodes.Iir_Kind.Component_Declaration:
            ent = libraries.Find_Entity_For_Component(nodes.Get_Identifier(decl))
            if ent != nodes.Null_Iir:
                res.append(self.declaration_to_location(nodes.Get_Library_Unit(ent)))
        return res

    def x_show_all_files(self):
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
        def create_interfaces(inters):
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
        """Return a dictionnary of anti dependencies for design unit."""
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
                                if res.get(el, None):
                                    res[el].append(units)
                                else:
                                    res[el] = [units]
                            else:
                                assert False
                            lists.Next(byref(deps_it))
                    units = nodes.Get_Chain(units)
                files = nodes.Get_Chain(files)
            lib = nodes.Get_Chain(lib)
        return res
