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

Each request of the Language Server Protocol is a method of :class:`VhdlLanguageServer`, named in the
:attr:`~VhdlLanguageServer.dispatcher` that :class:`~pyGHDL.lsp.lsp.LanguageProtocolServer` looks the incoming
method up in. The answers are read from the :class:`~pyGHDL.lsp.workspace.Workspace`; nothing here parses VHDL
itself.
"""

import logging

from . import lsp
from .workspace import Workspace

log = logging.getLogger(__name__)


class VhdlLanguageServer(object):
    """
    The VHDL side of the language server.

    One method implements one request of the protocol. Which method that is comes from :attr:`dispatcher`, not from
    the method's name: the name only follows the request by convention, and a request that is not a key of that
    mapping is answered as unknown however the method is called. The answers come from the
    :class:`~pyGHDL.lsp.workspace.Workspace`, which holds the analyzed sources.
    """

    def __init__(self):
        """
        Build the dispatcher, mapping each supported request to the method answering it.

        The workspace is not created here. It needs the root the client sends, so it is built by
        :meth:`initialize`.
        """
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
        """
        Attach the protocol server the answers are sent through.

        :param server: The :class:`~pyGHDL.lsp.lsp.LanguageProtocolServer` serving this handler.
        """
        self.lsp = server

    def shutdown(self):
        """
        Answer ``shutdown`` by asking the protocol server to stop reading requests.
        """
        self.lsp.shutdown()

    def setTraceNotification(self, value):
        """
        Accept ``$/setTraceNotification`` and do nothing, because the server has no trace levels to set.

        :param value: The trace level the client asked for.
        """
        pass

    def setTrace(self, value):
        """
        Accept ``$/setTrace`` and do nothing, because the server has no trace levels to set.

        :param value: The trace level the client asked for.
        """
        pass

    def cancelRequest(self, id):
        """
        Accept ``$/cancelRequest`` and do nothing.

        Requests are answered in the order they arrive, so by the time a cancellation is read the request it names
        has already been answered.

        :param id: The identifier of the request the client wants cancelled.
        """
        pass

    def capabilities(self):
        """
        Report what this server can do, as the reply to ``initialize`` requires.

        The document synchronization is incremental: the client sends the edits rather than the whole file, which
        is what :meth:`~pyGHDL.lsp.workspace.Workspace.apply_changes` expects. A capability that is ``False`` is
        listed rather than omitted, so what is not implemented can be read off this one place.

        :returns: The ``ServerCapabilities`` of this server.
        """
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
        """
        Answer ``initialize`` by creating the workspace for the root the client opened.

        The root may arrive as a path or as a URI, depending on how old the client is; the path is converted when
        only it was given. A client sending neither gets an empty root, and the workspace then works on the open
        documents alone.

        :param processId:             The identifier of the process that started the server.
        :param rootPath:              The root as a path, superseded by ``rootUri`` in newer clients.
        :param capabilities:          What the client can do. This server does not vary its answers by it.
        :param rootUri:               The root as a URI.
        :param initializationOptions: Options from the client. They are logged, not read.
        :param _:                     Further members of the request, ignored.
        :returns:                     The capabilities of this server.
        :raises InitError:            If *libghdl* could not be initialized.
        """
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
        """
        Accept the ``initialized`` notification, which says the client has finished starting up.

        :returns: ``None``, as a notification is not answered.
        """
        # Event when the client is fully initialized.
        return None

    def textDocument_didOpen(self, textDocument=None):
        """
        Take a document the client opened and check it.

        :param textDocument: A ``TextDocumentItem`` with the URI, the text and the version.
        """
        doc_uri = textDocument["uri"]
        self.workspace.put_document(doc_uri, textDocument["text"], version=textDocument.get("version"))
        self.lint(doc_uri)

    def textDocument_didChange(self, textDocument=None, contentChanges=None, **_kwargs):
        """
        Apply the edits the client made.

        The diagnostics are published by :meth:`~pyGHDL.lsp.workspace.Workspace.apply_changes` itself, which is why
        this does not lint afterwards.

        :param textDocument:   A ``VersionedTextDocumentIdentifier`` naming the document.
        :param contentChanges: The edits, in the order they are to be applied.
        :param _kwargs:        Further members of the notification, ignored.
        """
        doc_uri = textDocument["uri"]
        new_version = textDocument.get("version")
        self.workspace.apply_changes(doc_uri, contentChanges, new_version)

    def lint(self, doc_uri):
        """
        Re-analyze a document and publish its diagnostics.

        :param doc_uri: The URI of the document to check.
        """
        self.workspace.lint(doc_uri)

    def textDocument_didClose(self, textDocument=None, **_kwargs):
        """
        Drop the diagnostics of a document the client closed.

        The document itself stays in the workspace, as other documents may have been analyzed against it.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param _kwargs:      Further members of the notification, ignored.
        """
        self.workspace.rm_document(textDocument["uri"])

    def textDocument_didSave(self, textDocument=None, text=None, **_kwargs):
        """
        Re-check a document the client saved.

        When the client sends the text along, it is first compared against the buffer the server has been editing.
        A difference there means the edits were applied differently on the two sides, which would make every
        position reported from now on wrong.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param text:         The saved text, if the client was asked to include it.
        :param _kwargs:      Further members of the notification, ignored.
        """
        if text is not None:
            # Sanity check: check we have the same content for the document.
            self.workspace.check_document(textDocument["uri"], text)
        else:
            log.debug("did save - no text")
        self.lint(textDocument["uri"])

    def textDocument_definition(self, textDocument=None, position=None):
        """
        Answer ``textDocument/definition``.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param position:     The position of the name to resolve.
        :returns:            A list holding the location of the declaration, or ``None``.
        """
        return self.workspace.goto_definition(textDocument["uri"], position)

    def textDocument_implementation(self, textDocument=None, position=None):
        """
        Answer ``textDocument/implementation``.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param position:     The position of the name to resolve.
        :returns:            A list holding the location of the body, or ``None``.
        """
        return self.workspace.goto_implementation(textDocument["uri"], position)

    def textDocument_documentSymbol(self, textDocument=None):
        """
        Answer ``textDocument/documentSymbol``.

        The document is loaded if the client never opened it, because an editor may ask for the symbols of a file
        it is only showing in an outline.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :returns:            The symbols declared in the document.
        """
        doc = self.workspace.get_or_create_document(textDocument["uri"])
        return doc.document_symbols()

    def textDocument_rangeFormatting(self, textDocument=None, range=None, options=None):
        """
        Answer ``textDocument/rangeFormatting`` by re-indenting a range of lines.

        The document is re-checked after a successful format, because the edit changes the positions everything
        else was reported at.

        :param textDocument:    A ``TextDocumentIdentifier`` naming the document.
        :param range:           The range to format.
        :param options:         The formatting options of the client. The indentation follows the source, not these.
        :returns:               The edits to apply, or ``None`` if there is nothing to format.
        :raises AssertionError: If the document is not loaded.
        """
        doc_uri = textDocument["uri"]
        doc = self.workspace.get_document(doc_uri)
        assert doc is not None, "Try to format a non-loaded document"
        res = doc.format_range(range)
        if res is not None:
            self.lint(doc_uri)
        return res

    def textDocument_hover(self, textDocument=None, position=None):
        """
        Answer ``textDocument/hover``.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param position:     The position the client is hovering over.
        :returns:            A ``Hover``, or ``None`` if there is no declaration there.
        """
        return self.workspace.hover(textDocument["uri"], position)

    def textDocument_codeAction(self, textDocument=None, range=None, context=None):
        """
        Answer ``textDocument/codeAction`` with nothing, as no action is implemented.

        :param textDocument: A ``TextDocumentIdentifier`` naming the document.
        :param range:        The range the actions would apply to.
        :param context:      The diagnostics the client wants addressed.
        :returns:            ``None``.
        """
        # Not yet implemented.
        # * reorder associations (but keep comments !)
        # * add missing associations (all or only IN)
        # * add formal in assocs
        return None

    def m_workspace__did_change_configuration(self, _settings=None):
        """
        Re-check every document, as a setting may change what is reported.

        :param _settings: The new settings. They are not read; the server asks the client for what it needs.
        """
        for doc_uri in self.workspace.documents:
            self.lint(doc_uri)

    def m_workspace__did_change_watched_files(self, **_kwargs):
        """
        Re-check every document, as a file changed outside the editor may change what is reported.

        :param _kwargs: The events the client sent, ignored. Every document is re-checked either way.
        """
        # Externally changed files may result in changed diagnostics
        for doc_uri in self.workspace.documents:
            self.lint(doc_uri)

    def workspace_xShowAllFiles(self):
        """
        Answer the ``workspace/xShowAllFiles`` extension.

        :returns: One entry per source file *libghdl* holds.
        """
        return self.workspace.x_show_all_files()

    def workspace_xGetAllEntities(self):
        """
        Answer the ``workspace/xGetAllEntities`` extension.

        :returns: One entry per entity, with its name and library.
        """
        return self.workspace.x_get_all_entities()

    def workspace_xGetEntityInterface(self, library, name):
        """
        Answer the ``workspace/xGetEntityInterface`` extension.

        :param library: The name of the library the entity lives in.
        :param name:    The name of the entity.
        :returns:       The entity with its generics and ports, or ``None`` if it is unknown.
        """
        return self.workspace.x_get_entity_interface(library, name)
