from pyTooling.Decorators import export

from pyGHDL import GHDLBaseException


@export
class LSPException(GHDLBaseException):
    """
    The exception is raised for every failure of the language server.

    It is the base-class of the language server's exceptions, so ``except LSPException`` catches them
    without also catching failures of the analyzer or of the document object model.
    """


class LSPConnTrace(object):
    """Wrapper class to save in and out packets"""

    def __init__(self, basename, conn):
        self.conn = conn
        self.trace_in = open(basename + ".in", "w")
        self.trace_out = open(basename + ".out", "w")

    def readline(self):
        res = self.conn.readline()
        self.trace_in.write(res)
        return res

    def read(self, size):
        res = self.conn.read(size)
        self.trace_in.write(res)
        self.trace_in.flush()
        return res

    def write(self, out):
        self.conn.write(out)
        self.trace_out.write(out)
        self.trace_out.flush()
