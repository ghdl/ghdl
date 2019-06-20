import ctypes
from os import environ
from os.path import dirname, join, exists
from shutil import which
from libghdl.config import __libghdl__


def _to_char_p(arg):
    return ctypes.c_char_p(arg), len(arg)


def check_libghdl_libdir(libdir):
    """Return libghdl path in :param libdir" if found or None"""
    if libdir is None:
        return None
    # print('libghdl: check in {}'.format(libdir))
    res = join(libdir, __libghdl__)
    if exists(res):
        return res
    return None

def check_libghdl_bindir(bindir):
    """Return libghdl path in :param bindir" if found or None"""
    if bindir is None:
        return None
    return check_libghdl_libdir(join(bindir, '..', 'lib'))

def get_libghdl_path():
    """Locate the directory where the shared library is"""
    # Try VUNIT_GHDL_PATH (path of the ghdl binary when using VUnit).
    r = check_libghdl_bindir (environ.get('VUNIT_GHDL_PATH'))
    if r is not None:
        return r
    # Try GHDL (name/path of the ghdl binary)
    r = environ.get('GHDL', 'ghdl')
    r = which(r)
    if r is not None:
        r = check_libghdl_bindir(dirname(r))
        if r is not None:
            return r
    # Try within libghdl/ python installation
    r = __file__
    r = check_libghdl_bindir(dirname(r))
    if r is not None:
        return r
    # Try when running from the build directory
    r = join(dirname(__file__), '..', '..', 'lib')
    r = check_libghdl_libdir(r)
    if r is not None:
        return r
    # Failed.
    return None

libghdl_path = get_libghdl_path()

if libghdl_path is None:
    raise Exception('Cannot find libghdl')

libghdl = ctypes.CDLL(libghdl_path)

libghdl.libghdl_init()


def set_option(opt):
    return libghdl.libghdl__set_option(*_to_char_p(opt))


def analyze_init():
    return libghdl.libghdl__analyze_init()


def analyze_file(fname):
    return libghdl.libghdl__analyze_file(*_to_char_p(fname))


if False:
    _prefix = environ.get("LIBGHDL_PREFIX") or '--PREFIX=%s' % join(dirname(libghdl_path), 'ghdl')
    print('ghdl prefix: {}'.format(_prefix))
    set_option(_prefix.encode('utf-8'))
