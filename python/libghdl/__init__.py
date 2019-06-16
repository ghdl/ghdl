import ctypes
from os import environ
from os.path import dirname, join
from shutil import which
from libghdl.config import __libghdl__


def _to_char_p(arg):
    return ctypes.c_char_p(arg), len(arg)


def get_ghdl_path():
    _dir = None
    for envvar in [environ.get(item) for item in ['GHDL_BIN_PATH', 'VUNIT_GHDL_PATH']]:
        if envvar:
            _dir = envvar
            break
    if not _dir:
        _dir = which(environ.get('GHDL', 'ghdl'))
    if _dir:
        _dir = join(dirname(_dir), '..', 'lib')
    return _dir


_basedir = get_ghdl_path() or dirname(__file__)

libghdl = ctypes.CDLL(join(_basedir, __libghdl__))

libghdl.libghdl_init()


def set_option(opt):
    arg = _to_char_p(opt)
    return libghdl.libghdl__set_option(arg[0], arg[1])


def analyze_init():
    return libghdl.libghdl__analyze_init()


def analyze_file(fname):
    arg = _to_char_p(fname)
    return libghdl.libghdl__analyze_file(arg[0], arg[1])


_prefix = environ.get("LIBGHDL_PREFIX") or '--PREFIX=%s' % join(_basedir, 'ghdl')
set_option(_prefix.encode('utf-8'))
