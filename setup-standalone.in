# == -*- python -*- ===========================================================
#               ____ _   _ ____  _
#  _ __  _   _ / ___| | | |  _ \| |
# | '_ \| | | | |  _| |_| | | | | |
# | |_) | |_| | |_| |  _  | |_| | |___
# | .__/ \__, |\____|_| |_|____/|_____|
# |_|    |___/
# =============================================================================
#  Authors:
#    Tristan Gingold
#    Patrick Lehmann
#    Unai Martinez-Corral
#
# Package installer:  Python binding for GHDL and high-level APIs.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
#
from setuptools import setup, find_namespace_packages, Distribution

from pathlib             import Path
from pyTooling.Packaging import loadRequirementsFile
import glob

requirementsFile = Path("dist-wheel/pyGHDL/requirements.txt")
install_requires=list(set(loadRequirementsFile(requirementsFile)))

#@__init__.py variables

# Extend the Distribution class to force platform specific wheel
class BinaryDistribution (Distribution):
    def has_ext_modules(self):
        return True

# Package data files
dist_dir="dist-wheel/pyGHDL"
package_data=[]
package_data.extend(glob.glob(dist_dir+"/lib/ghdl/**/*.vhdl", recursive=True))
package_data.extend(glob.glob(dist_dir+"/lib/ghdl/**/*.cf", recursive=True))
package_data.extend(glob.glob(dist_dir+"/lib/lib*" + soext))
package_data.extend(glob.glob(dist_dir+"/bin/lib*" + soext))
package_data=[x[len(dist_dir)+1:] for x in package_data]

setup(
    name="pyGHDL",
    version=__version__,
    author=__author__,
    author_email=__email__,
    license="GPL-2.0-or-later",
    description="Python binding for GHDL and high-level APIs (incl. LSP).",
    long_description=open("dist-wheel/pyGHDL/README.md","r").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/ghdl/ghdl",
    project_urls={"Documentation": "https://ghdl.github.io/ghdl",
                  "Source Code": "https://github.com/ghdl/ghdl",
                  "Issue Tracker": "https://github.com/ghdl/ghdl/issues"},
    packages=find_namespace_packages("dist-wheel"),
    package_dir={"": "dist-wheel"},
    classifiers=[
        "Programming Language :: Python :: 3 :: Only",
        "Development Status :: Beta",
        "License :: OSI Approved :: GNU General Public License v2 or later (GPLv2+)",
        "Operating System :: MacOS",
        "Operating System :: Microsoft :: Windows :: Windows 10",
        "Operating System :: POSIX :: Linux",
        "Intended Audience :: Developers",
        "Topic :: Scientific/Engineering :: Electronic Design Automation (EDA)",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Testing",
        "Topic :: Utilities",
    ],
    keywords=__keywords__,
    entry_points={
        'console_scripts': [
            "ghdl-ls=pyGHDL.cli.lsp:main",
            "ghdl-dom=pyGHDL.cli.dom:main"
            ]
    },
    python_requires=">=3.8",
    install_requires=install_requires,
    package_data={'pyGHDL': package_data},
    include_package_data=True,
    distclass = BinaryDistribution,
    )
