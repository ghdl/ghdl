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
# Package module:   Language Server Protocol: message framing and the JSON-RPC connection.
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
import os
from pathlib import Path
import logging
import json
from urllib.parse import unquote, urlparse

log = logging.getLogger("ghdl-ls")

from pyGHDL.lsp import LSPException

is_windows = os.name == "nt"


class ProtocolError(LSPException):
    """The exception is raised when the Language Server Protocol stream is malformed."""


class LSPConn:
    def __init__(self, reader, writer):
        """
        Initializes the connection from a reader and a writer.

        :param reader: The binary stream messages are read from.
        :param writer: The binary stream messages are written to.
        """
        self.reader = reader
        self.writer = writer

    def readline(self):
        """
        Read one line, which is how a header line is consumed.

        :returns: The line, decoded as UTF-8.
        """
        data = self.reader.readline()
        return data.decode("utf-8")

    def read(self, size):
        """
        Read a message body of a known length.

        :param size: The number of bytes to read, from the ``Content-Length`` header.
        :returns:    The body, decoded as UTF-8.
        """
        data = self.reader.read(size)
        return data.decode("utf-8")

    def write(self, out):
        """
        Write a message and flush, so the client sees it immediately.

        :param out: The text to write.
        """
        self.writer.write(out.encode())
        self.writer.flush()


def path_from_uri(uri):
    # Convert file uri to path (strip html like head part)
    # This is needed to get the root path and to load a document when the
    # textual source is not present.
    """
    Convert a ``file:`` URI to a path.

    A URI that does not name a local file is returned unchanged, which is how a client sending something else is
    tolerated rather than crashing the server.

    :param uri: The URI to convert.
    :returns:   The path the URI names, or the URI itself if it is not a ``file:`` URI.
    """
    if not uri.startswith("file://"):
        # No scheme
        return uri

    path = unquote(urlparse(uri).path)
    # On windows, absolute files start like "/C:/aa/bbb".
    # Remove the first "/".
    if is_windows:
        path = path[1:]

    # Path.resolve used to ensure consistent capitalization
    # on Windows, as GHDL-ada will fail if it is inconsistent.
    return Path(path).resolve().as_posix()


def path_to_uri(path):
    """
    Convert a path to a ``file:`` URI, resolving it first.

    :param path: The path to convert.
    :returns:    The absolute ``file:`` URI of that path.
    """
    return Path(path).resolve().as_uri()


def normalize_rpc_file_uris(rpc):
    # Normalize all file URIs inside an RPC to have consistent capitalization.
    # Fixes a crash on windows where the underlying ada crashes
    # if paths to the same file are given with inconsistent
    # capitalization.
    """
    Normalize the capitalization of every ``file:`` URI in a message.

    Clients differ in how they capitalize a drive letter on Windows, so the same document can arrive under two
    spellings and be looked up as two different files.

    :param rpc: The decoded message to normalize, modified in place.
    :returns:   The same message.
    """
    for key, val in rpc.items():
        # recurse into all leaf elements.
        if isinstance(val, dict):
            normalize_rpc_file_uris(val)
        elif key == "rootUri" or key == "uri":
            # normalize URI
            rpc[key] = path_to_uri(path_from_uri(val))


class LanguageProtocolServer(object):
    def __init__(self, handler, conn):
        """
        Initializes the server with the object that handles the requests.

        :param handler: The object whose ``lsp_*`` methods implement the protocol requests.
        :param conn:    The connection to serve on, or ``None`` to build one from stdin and stdout.
        """
        self.conn = conn
        self.handler = handler
        if handler is not None:
            handler.set_lsp(self)
        self.running = True
        self._next_id = 0

    def read_request(self):
        """
        Read one message: the headers, then the body of the length they announce.

        :returns:              The message body, or ``None`` at end of input.
        :raises ProtocolError: If a header line is malformed or ``Content-Length`` is missing.
        """
        headers = {}
        while True:
            # Read a line
            line = self.conn.readline()
            # Return on EOF.
            if not line:
                return None
            if line[-2:] != "\r\n":
                raise ProtocolError("invalid end of line in header")
            line = line[:-2]
            if not line:
                # End of headers.
                log.debug("Headers: %r", headers)
                length = headers.get("Content-Length", None)
                if length is not None:
                    body = self.conn.read(int(length))
                    return body
                else:
                    raise ProtocolError("missing Content-Length in header")
            else:
                key, value = line.split(": ", 1)
                headers[key] = value

    def run(self):
        """
        Serve requests until the client disconnects or asks the server to exit.
        """
        while self.running:
            body = self.read_request()
            if body is None:
                # EOF
                break

            # Text to JSON
            msg = json.loads(body)
            log.debug("Read msg: %s", msg)

            reply = self.handle(msg)
            if reply is not None:
                self.write_output(reply)

    def handle(self, msg):
        """
        Dispatch one decoded message to the handler named by its ``method``.

        A request is answered with a response, a notification is not answered at all, and an unknown method is
        reported as ``MethodNotFound`` rather than raising.

        :param msg:            The decoded message.
        :raises ProtocolError: If the message is not JSON-RPC 2.0.
        """
        if msg.get("jsonrpc", None) != "2.0":
            raise ProtocolError("invalid jsonrpc version")
        tid = msg.get("id", None)
        method = msg.get("method", None)
        if method is None:
            # This is a reply.
            log.error("Unexpected reply for %s", tid)
            return
        params = msg.get("params", None)
        # Fix capitalization issues on windows.
        if is_windows:
            normalize_rpc_file_uris(msg)
        fmethod = self.handler.dispatcher.get(method, None)
        if fmethod:
            if params is None:
                params = {}
            try:
                response = fmethod(**params)
            except Exception:
                log.exception("Caught exception while handling %s with params %s:", method, params)
                self.show_message(
                    MessageType.Error,
                    f"Caught exception while handling {method}, see VHDL language server output for details.",
                )
                response = None
            if tid is None:
                # If this was just a notification, discard it
                return None
            log.debug("Response: %s", response)
            rbody = {
                "jsonrpc": "2.0",
                "id": tid,
                "result": response,
            }
        else:
            # Unknown method.
            log.error("Unknown method %s", method)
            # If this was just a notification, discard it
            if tid is None:
                return None
            # Otherwise create an error.
            rbody = {
                "jsonrpc": "2.0",
                "id": tid,
                "error": {
                    "code": JSONErrorCodes.MethodNotFound,
                    "message": f"unknown method {method}",
                },
            }
        return rbody

    def write_output(self, body):
        """
        Encode a message and write it with its ``Content-Length`` header.

        :param body: The message to send.
        """
        output = json.dumps(body, separators=(",", ":"))
        self.conn.write(f"Content-Length: {len(output)}\r\n")
        self.conn.write("\r\n")
        self.conn.write(output)

    def notify(self, method, params):
        """
        Send a notification, which the client does not answer.

        :param method: The protocol method to notify.
        :param params: The parameters of the notification.
        """
        body = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }
        self.write_output(body)

    def send_request(self, method, params):
        """
        Send a request to the client and wait for its answer.

        :param method: The protocol method to call.
        :param params: The parameters of the request.
        :returns:      The client's result.
        """
        self._next_id += 1
        body = {
            "jsonrpc": "2.0",
            "id": self._next_id,
            "method": method,
            "params": params,
        }
        self.write_output(body)

    def shutdown(self):
        """Prepare to shutdown the server."""
        self.running = False

    def show_message(self, typ, message):
        """
        Ask the client to show a message to the user.

        :param typ:     The severity, from :class:`MessageType`.
        :param message: The text to show.
        """
        self.notify("window/showMessage", {"type": typ, "message": message})

    def configuration(self, items):
        """
        Ask the client for configuration values.

        :param items: The configuration items to request.
        :returns:     The client's answer.
        """
        return self.send_request("workspace/configuration", {"items": items})


# ----------------------------------------------------------------------
#  Standard defines and object types
#


class JSONErrorCodes(object):
    # Defined by JSON RPC
    ParseError = -32700
    InvalidRequest = -32600
    MethodNotFound = -32601
    InvalidParams = -32602
    InternalError = -32603
    serverErrorStart = -32099
    serverErrorEnd = -32000
    ServerNotInitialized = -32002
    UnknownErrorCode = -32001

    # Defined by the protocol.
    RequestCancelled = -32800
    ContentModified = -32801


class CompletionKind(object):
    Text = 1
    Method = 2
    Function = 3
    Constructor = 4
    Field = 5
    Variable = 6
    Class = 7
    Interface = 8
    Module = 9
    Property = 10
    Unit = 11
    Value = 12
    Enum = 13
    Keyword = 14
    Snippet = 15
    Color = 16
    File = 17
    Reference = 18


class DiagnosticSeverity(object):
    Error = 1
    Warning = 2
    Information = 3
    Hint = 4


class TextDocumentSyncKind(object):
    NONE = (0,)
    FULL = 1
    INCREMENTAL = 2


class MessageType(object):
    Error = 1
    Warning = 2
    Info = 3
    Log = 4


class SymbolKind(object):
    File = 1
    Module = 2
    Namespace = 3
    Package = 4
    Class = 5
    Method = 6
    Property = 7
    Field = 8
    Constructor = 9
    Enum = 10
    Interface = 11
    Function = 12
    Variable = 13
    Constant = 14
    String = 15
    Number = 16
    Boolean = 17
    Array = 18
