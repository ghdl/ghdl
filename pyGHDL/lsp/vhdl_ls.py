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
# Package module:   The VHDL language server: dispatching Language Server Protocol requests.
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
The VHDL side of GHDL's language server.

Each request of the Language Server Protocol is a ``lsp_*`` method on :class:`VhdlLanguageServer`, which
:class:`~pyGHDL.lsp.lsp.LanguageProtocolServer` dispatches to by name. The answers are read from the
:class:`~pyGHDL.lsp.workspace.Workspace`; nothing here parses VHDL itself.
"""

import logging

from . import lsp
from .workspace import Workspace

log = logging.getLogger(__name__)


class VhdlLanguageServer(object):
    """
    The VHDL side of the language server.

    Each ``lsp_*`` method implements one Language Server Protocol request, and
    :class:`~pyGHDL.lsp.lsp.LanguageProtocolServer` dispatches to it by name. The answers come from the:
    :class:`~pyGHDL.lsp.workspace.Workspace`, which holds the analyzed sources.:

    """

    def __init__(self):
        self.workspace = None
        self.lsp = None
        self._shutdown = False
        self.dispatcher = {
            "initialize": self.initialize,
            "initialized": self.initialized,
            "shutdown": self.shutdown,
            "$/setTraceNotification": self.setTraceNotification,
            "$/setTrace": self.setTrace,
            "$/cancelRequest": self.cancelRequest,
            "textDocument/didOpen": self.textDocument_didOpen,
            "textDocument/didChange": self.textDocument_didChange,
            "textDocument/didClose": self.textDocument_didClose,
            "textDocument/didSave": self.textDocument_didSave,
            "textDocument/hover": self.textDocument_hover,
            "textDocument/definition": self.textDocument_definition,
            "textDocument/implementation": self.textDocument_implementation,
            "textDocument/documentSymbol": self.textDocument_documentSymbol,
            "textDocument/codeAction": self.textDocument_codeAction,
            # 'textDocument/completion': self.completion,
            "textDocument/rangeFormatting": self.textDocument_rangeFormatting,
            "workspace/xShowAllFiles": self.workspace_xShowAllFiles,
            "workspace/xGetAllEntities": self.workspace_xGetAllEntities,
            "workspace/xGetEntityInterface": self.workspace_xGetEntityInterface,
        }

    def set_lsp(self, server):
        self.lsp = server

    def shutdown(self):
        self.lsp.shutdown()

    def setTraceNotification(self, value):
        pass

    def setTrace(self, value):
        pass

    def cancelRequest(self, id):
        pass

    def capabilities(self):
        server_capabilities = {
            "textDocumentSync": {
                "openClose": True,
                "change": lsp.TextDocumentSyncKind.INCREMENTAL,
                "save": {"includeText": True},
            },
            "hoverProvider": True,
            #            'completionProvider': False,
            #            'signatureHelpProvider': {
            #                'triggerCharacters': ['(', ',']
            #            },
            "definitionProvider": True,
            "implementationProvider": True,
            "referencesProvider": False,
            "documentHighlightProvider": False,
            "documentSymbolProvider": True,
            "codeActionProvider": False,
            "documentFormattingProvider": False,
            "documentRangeFormattingProvider": True,
            "renameProvider": False,
        }
        return server_capabilities

    def initialize(self, processId, rootPath, capabilities, rootUri=None, initializationOptions=None, **_):
        log.debug(
            "Language server initialize: pid=%s uri=%s path=%s options=%s",
            processId,
            rootUri,
            rootPath,
            initializationOptions,
        )
        if rootUri is None:
            rootUri = lsp.path_to_uri(rootPath) if rootPath is not None else ""
        self.workspace = Workspace(rootUri, self.lsp)

        # Get our capabilities
        return {"capabilities": self.capabilities()}

    def initialized(self):
        # Event when the client is fully initialized.
        return None

    def textDocument_didOpen(self, textDocument=None):
        doc_uri = textDocument["uri"]
        self.workspace.put_document(doc_uri, textDocument["text"], version=textDocument.get("version"))
        self.lint(doc_uri)

    def textDocument_didChange(self, textDocument=None, contentChanges=None, **_kwargs):
        doc_uri = textDocument["uri"]
        new_version = textDocument.get("version")
        self.workspace.apply_changes(doc_uri, contentChanges, new_version)

    def lint(self, doc_uri):
        self.workspace.lint(doc_uri)

    def textDocument_didClose(self, textDocument=None, **_kwargs):
        self.workspace.rm_document(textDocument["uri"])

    def textDocument_didSave(self, textDocument=None, text=None, **_kwargs):
        if text is not None:
            # Sanity check: check we have the same content for the document.
            self.workspace.check_document(textDocument["uri"], text)
        else:
            log.debug("did save - no text")
        self.lint(textDocument["uri"])

    def textDocument_definition(self, textDocument=None, position=None):
        return self.workspace.goto_definition(textDocument["uri"], position)

    def textDocument_implementation(self, textDocument=None, position=None):
        return self.workspace.goto_implementation(textDocument["uri"], position)

    def textDocument_documentSymbol(self, textDocument=None):
        doc = self.workspace.get_or_create_document(textDocument["uri"])
        return doc.document_symbols()

    def textDocument_rangeFormatting(self, textDocument=None, range=None, options=None):
        doc_uri = textDocument["uri"]
        doc = self.workspace.get_document(doc_uri)
        assert doc is not None, "Try to format a non-loaded document"
        res = doc.format_range(range)
        if res is not None:
            self.lint(doc_uri)
        return res

    def textDocument_hover(self, textDocument=None, position=None):
        return self.workspace.hover(textDocument["uri"], position)

    def textDocument_codeAction(self, textDocument=None, range=None, context=None):
        # Not yet implemented.
        # * reorder associations (but keep comments !)
        # * add missing associations (all or only IN)
        # * add formal in assocs
        return None

    def m_workspace__did_change_configuration(self, _settings=None):
        for doc_uri in self.workspace.documents:
            self.lint(doc_uri)

    def m_workspace__did_change_watched_files(self, **_kwargs):
        # Externally changed files may result in changed diagnostics
        for doc_uri in self.workspace.documents:
            self.lint(doc_uri)

    def workspace_xShowAllFiles(self):
        return self.workspace.x_show_all_files()

    def workspace_xGetAllEntities(self):
        return self.workspace.x_get_all_entities()

    def workspace_xGetEntityInterface(self, library, name):
        return self.workspace.x_get_entity_interface(library, name)
