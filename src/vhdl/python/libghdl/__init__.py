import ctypes
import sys
import os.path

_ext = {'linux': '.so',
        'linux2': '.so',
        'darwin': '.dylib',
        'win32': '.dll',
        'cygwin': '.dll'}

# Load the DLL.
_basedir = os.path.join(os.path.dirname(__file__), '..')
libghdl = ctypes.CDLL(os.path.join(
    _basedir, 'libghdl' + _ext.get(sys.platform, '.so')))

# Low-level initialization (elaboration).
libghdl.libghdl_init()

# Set the default prefix.
_prefix = os.path.join(_basedir, "ghdl")
_prefix_opt = "--PREFIX=" + _prefix
libghdl.libghdl__set_option(
    ctypes.c_char_p(_prefix_opt), len(_prefix_opt))
