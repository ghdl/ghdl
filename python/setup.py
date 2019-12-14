#!/usr/bin/env python

import distutils.command.build_py
from distutils.core import setup
import re

def get_version():
    # Try from version.py.  Reads it to avoid to load the shared library.
    r = re.compile("^__version__ = '(.*)'\n")
    try:
        l = open('libghdl/version.py').read()
        m = r.match(l)
        if m:
            return m.group(1)
    except:
        pass
    raise Exception("Cannot find version")

# Extract the version now, as setup() may change the current directory.
version=get_version()

setup(
    name='libghdl',
    version=version,
    description='Interface to ghdl, a VHDL analyzer',
    long_description="""GHDL is a vhdl simulator and libghdl provides a low-level
interface to the parser. This library gives access to the AST so that you can
write tools like linters.
""",
    author='Tristan Gingold',
    author_email='tgingold@free.fr',
    url='http://github.com/ghdl/ghdl',
    license='GPL-2.0-or-later',
    package_dir={
        'libghdl': './libghdl'
    },
    packages=[
        'libghdl',
        'libghdl.thin',
        'libghdl.thin.vhdl'
    ]
)
