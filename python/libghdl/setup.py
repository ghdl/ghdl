#!/usr/bin/env python

from distutils.core import setup
from config import __version__

setup(
    name='libghdl',
    version=__version__,
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
        'libghdl': './'
    },
    packages=[
        'libghdl',
        'libghdl.thin',
        'libghdl.thin.vhdl'
    ]
)
