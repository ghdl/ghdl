import ctypes
import os
import sys
from os.path import dirname, join, exists, normpath
from shutil import which
from pyGHDL.libghdl.version import __version__


def _to_char_p(arg):
    return ctypes.c_char_p(arg), len(arg)


def _get_libghdl_name():
    """Get the name of the libghdl library (with version and extension)"""
    ver = __version__.replace("-", "_").replace(".", "_")
    ext = {"win32": "dll", "cygwin": "dll", "darwin": "dylib"}.get(sys.platform, "so")
    return "libghdl-{version}.{ext}".format(version=ver, ext=ext)


def _check_libghdl_libdir(libdir, basename):
    """Return libghdl path in :param libdir" if found or None"""
    if libdir is None:
        return None
    # print('libghdl: check in {}'.format(libdir))
    res = join(libdir, basename)
    if exists(res):
        return res
    return None


def _check_libghdl_bindir(bindir, basename):
    if bindir is None:
        return None
    return _check_libghdl_libdir(normpath(join(bindir, "..", "lib")), basename)


def _get_libghdl_path():
    """Locate the directory where the shared library is"""
    basename = _get_libghdl_name()

    # Try GHDL_PREFIX
    r = os.environ.get("GHDL_PREFIX")
    if r is not None:
        # GHDL_PREFIX is the prefix of the vhdl libraries, so remove the
        # last path component.
        r = _check_libghdl_libdir(dirname(r), basename)
        if r is not None:
            return r
    # Try VUNIT_GHDL_PATH (path of the ghdl binary when using VUnit).
    r = _check_libghdl_bindir(os.environ.get("VUNIT_GHDL_PATH"), basename)
    if r is not None:
        return r
    # Try GHDL (name/path of the ghdl binary)
    r = os.environ.get("GHDL", "ghdl")
    r = which(r)
    if r is not None:
        r = _check_libghdl_bindir(dirname(r), basename)
        if r is not None:
            return r
    # Try within libghdl/ python installation
    r = __file__
    r = _check_libghdl_bindir(dirname(r), basename)
    if r is not None:
        return r
    # Try when running from the build directory
    r = normpath(join(dirname(__file__), "..", "..", "lib"))
    r = _check_libghdl_libdir(r, basename)
    if r is not None:
        return r
    # Failed.
    raise Exception("Cannot find libghdl {}".format(basename))


# Load the shared library
_libghdl_path = _get_libghdl_path()
# print("Load {}".format(_libghdl_path))
libghdl = ctypes.CDLL(_libghdl_path)

# Initialize it.
# First Ada elaboration (must be the first call)
libghdl.libghdl_init()
# Then 'normal' initialization (set hooks)
libghdl.libghdl__set_hooks_for_analysis()

# Set the prefix in order to locate the VHDL libraries.
libghdl.libghdl__set_exec_prefix(
    *_to_char_p(dirname(dirname(_libghdl_path)).encode("utf-8"))
)

def finalize() -> None:
    """Free all the memory, be ready for a new initialization."""
    libghdl.options__finalize()


def initialize() -> None:
    """Initialize or re-initialize the shared library."""
    libghdl.options__initialize()


def set_option(opt: bytes) -> bool:
    """Set option :param:`opt`. Return true, if the option is known and handled."""
    return libghdl.libghdl__set_option(*_to_char_p(opt)) == 0


def analyze_init() -> None:
    """
    Initialize the analyzer.

    .. deprecated:: 1.0.0
       Deprecated as it may raise an exception. Use :func:`analyze_init_status`.
    """
    libghdl.libghdl__analyze_init()

def analyze_init_status() -> int:
    """Initialize the analyzer. Returns 0 in case of success."""
    return libghdl.libghdl__analyze_init_status()

def analyze_file(fname: bytes):
    """"Analyze a given filename :param:`fname`."""
    return libghdl.libghdl__analyze_file(*_to_char_p(fname))


def disp_config():
    """"Display the configured prefixes for libghdl."""
    return libghdl.ghdllocal__disp_config_prefixes()
