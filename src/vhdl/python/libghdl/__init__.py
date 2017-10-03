import ctypes
import sys

dll_ext = {'linux': '.so',
           'linux2': '.so',
           'darwin': '.dylib',
           'win32': '.dll',
           'cygwin': '.dll'}

libghdl = ctypes.CDLL("libghdl" + dll_ext.get(sys.platform, '.so'))
libghdl.libghdl_init()
