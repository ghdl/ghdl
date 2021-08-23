import os
import logging
import json
from urllib.parse import unquote, quote

log = logging.getLogger("ghdl-ls")

is_windows = os.name == "nt"


class ProtocolError(Exception):
    pass


class LSPConn:
    def __init__(self, reader, writer):
        self.reader = reader
        self.writer = writer

    def readline(self):
        data = self.reader.readline()
        return data.decode("utf-8")

    def read(self, size):
        data = self.reader.read(size)
        return data.decode("utf-8")

    def write(self, out):
        self.writer.write(out.encode())
        self.writer.flush()


def path_from_uri(uri):
    # Convert file uri to path (strip html like head part)
    # This is needed to get the root path and to load a document when the
    # textual source is not present.
    if not uri.startswith("file://"):
        # No scheme
        return uri
    _, path = uri.split("file://", 1)
    if is_windows and path.startswith("/"):
        # On windows, absolute files start like "/C:/aa/bbb".
        # Remove the first "/".
        path = path[1:]
    return os.path.normpath(unquote(path))


def path_to_uri(path):
    # Convert path to file uri (add html like head part)
    # :param path: is an absolute path.
    if is_windows:
        # On windows, do not quote the colon after the driver letter, as
        # it is not quoted in uri from the client.
        path = path.replace("\\", "/")
        return "file:///{0}{1}".format(path[:2], quote(path[2:]))
    return "file://{0}".format(quote(path))


class LanguageProtocolServer(object):
    def __init__(self, handler, conn):
        self.conn = conn
        self.handler = handler
        if handler is not None:
            handler.set_lsp(self)
        self.running = True
        self._next_id = 0

    def read_request(self):
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
        if msg.get("jsonrpc", None) != "2.0":
            raise ProtocolError("invalid jsonrpc version")
        tid = msg.get("id", None)
        method = msg.get("method", None)
        if method is None:
            # This is a reply.
            log.error("Unexpected reply for %s", tid)
            return
        params = msg.get("params", None)
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
                    ("Caught exception while handling {}, see VHDL language server output for details.").format(method),
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
                    "message": "unknown method {}".format(method),
                },
            }
        return rbody

    def write_output(self, body):
        output = json.dumps(body, separators=(",", ":"))
        self.conn.write("Content-Length: {}\r\n".format(len(output)))
        self.conn.write("\r\n")
        self.conn.write(output)

    def notify(self, method, params):
        """Send a notification."""
        body = {
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }
        self.write_output(body)

    def send_request(self, method, params):
        """Send a request."""
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
        self.notify("window/showMessage", {"type": typ, "message": message})

    def configuration(self, items):
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
