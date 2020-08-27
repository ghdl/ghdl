#!/usr/bin/env python

from setuptools import setup
import re


def get_version():
    # Try from version.py.  Reads it to avoid loading the shared library.
    r = re.compile('^__version__ = "(.*)"\n')
    try:
        l = open("libghdl/version.py").read()
        m = r.match(l)
        if m:
            return m.group(1)
    except:
        pass
    raise Exception("Cannot find version")


# Extract the version now, as setup() may change the current directory.
version = get_version()

setup(
    name="pyghdl",
    version=version,
    description="VHDL Language Server and interface to ghdl, a VHDL analyzer",
    long_description=open("README").read(),
    author="Tristan Gingold",
    author_email="tgingold@free.fr",
    url="http://github.com/ghdl/ghdl",
    license="GPL-2.0-or-later",
    package_dir={"libghdl": "libghdl", "vhdl_langserver": "vhdl_langserver"},
    packages=["libghdl", "libghdl.thin", "libghdl.thin.vhdl", "vhdl_langserver"],
    # List run-time dependencies here. For an analysis of "install_requires"
    # vs pip's requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=["attrs"],
    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.
    entry_points={
        "console_scripts": [
            "ghdl-ls = vhdl_langserver.main:main",
        ]
    },
)
