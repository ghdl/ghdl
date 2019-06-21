#!/usr/bin/env python

import distutils.command.build_py
from distutils.core import setup
import re

def get_version():
    # Try from config.py.  Reads it to avoid to load the shared library.
    r = re.compile("^__version__ = '(.*)'\n")
    try:
        l = open('libghdl/config.py').read()
        m = r.match(l)
        if m:
            return m.group(1)
    except:
        pass
    # Try to extract from configure
    r = re.compile('^ghdl_version="(.*)"')
    try:
        for l in open('../configure').readlines():
            m = r.match(l)
            if m:
                return m.group(1)
    except:
        pass
    raise Exception("Cannot find version")

# Extract the version now, as setup() may change the current directory.
version=get_version()

class MyBuildPy(distutils.command.build_py.build_py):
    def run(self):
        with open('libghdl/config.py', 'w') as f:
            f.write("__version__ = '{}'\n".format(version))
        super(MyBuildPy, self).run()

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
    cmdclass={
        'build_py': MyBuildPy
    },
    package_dir={
        'libghdl': './libghdl'
    },
    packages=[
        'libghdl',
        'libghdl.thin',
        'libghdl.thin.vhdl'
    ]
)
