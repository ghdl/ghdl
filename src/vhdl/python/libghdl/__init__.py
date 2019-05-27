import ctypes
import os.path
from libghdl.config import libghdl_filename

# Load the DLL.
_basedir = os.path.dirname(__file__)
libghdl = ctypes.CDLL(os.path.join(_basedir, libghdl_filename))

# Low-level initialization (elaboration).
libghdl.libghdl_init()

# Set the default prefix.
if False:
    _prefix = os.path.join(_basedir, "ghdl")
    _prefix_opt = ("--PREFIX=" + _prefix).encode('utf-8')
    libghdl.libghdl__set_option(
        ctypes.c_char_p(_prefix_opt), len(_prefix_opt))

# libghdl

_set_option = libghdl.libghdl__set_option
_analyze_file = libghdl.libghdl__analyze_file


def set_option(opt):
    return _set_option(ctypes.c_char_p(opt), len(opt))


def analyze_init():
    return libghdl.libghdl__analyze_init()


def analyze_file(filename):
    return _analyze_file(ctypes.c_char_p(filename), len(filename))
