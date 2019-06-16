import ctypes
#from os import environ
from os.path import dirname, join
from shutil import which
from libghdl.config import __libghdl__


def _to_char_p(arg):
    return ctypes.c_char_p(arg), len(arg)


#executable = environ.get("GHDL", "ghdl")

_basedir = which('ghdl')
if _basedir is not None:
    _basedir = join(dirname(_basedir), '..', 'lib')

libghdl = ctypes.CDLL(join(_basedir, __libghdl__))


# Low-level initialization (elaboration).
libghdl.libghdl_init()

_set_option = libghdl.libghdl__set_option
_analyze_file = libghdl.libghdl__analyze_file


def set_option(opt):
    arg = _to_char_p(opt)
    return _set_option(arg[0], arg[1])


def analyze_init():
    return libghdl.libghdl__analyze_init()


def analyze_file(fname):
    arg = _to_char_p(fname)
    return _analyze_file(arg[0], arg[1])


# Set the default prefix.
if True:
    set_option(('--PREFIX=%s' % join(_basedir, 'ghdl')).encode('utf-8'))
