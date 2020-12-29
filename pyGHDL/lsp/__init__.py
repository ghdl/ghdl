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
