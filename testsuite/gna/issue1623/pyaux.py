from sys import platform
from pathlib import Path
import ctypes
import _ctypes
from sys import stdout, stderr


def dlopen(path):
    if not Path(path).exists():
        print('Executable binary not found: ' + path)
        exit(1)
    try:
        return ctypes.CDLL(path)
    except OSError:
        print('Loading executables dynamically seems not to be supported on this platform')
        exit(1)


def dlclose(obj):
    if platform == "win32":
        _ctypes.FreeLibrary(obj._handle)
    else:
        _ctypes.dlclose(obj._handle)


def enc_args(args):
    xargs = (ctypes.POINTER(ctypes.c_char) * len(args))()
    for idx, arg in enumerate(args):
        xargs[idx] = ctypes.create_string_buffer(arg.encode('utf-8'))
    return xargs


def run(path, argc, argv):
    print("PY RUN ENTER")
    ghdl = dlopen(path)
    _ret = ghdl.entry(argc, argv)
    stdout.flush()
    stderr.flush()
    dlclose(ghdl)
    print("PY RUN EXIT <%d>" % _ret)
    stdout.flush()
    stderr.flush()
