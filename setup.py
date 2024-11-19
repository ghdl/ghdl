# =============================================================================
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
from pathlib                  import Path
from setuptools               import setup, Distribution

from pyTooling.Licensing      import GPL_2_0_or_later
from pyTooling.Packaging      import DescribePythonPackageHostedOnGitHub

gitHubNamespace = "ghdl"
packageName =  "pyGHDL"
packageDirectory = packageName
packageInformationFile = Path(f"{packageDirectory}/__init__.py")
requirementsFile = Path(f"{packageDirectory}/requirements.txt")


# Package data files
sourceDirectory = Path().cwd() / "pyGHDL"
dataFiles = [sourceDirectory / "py.typed"]
additional = {}

# libGHDL shared object and pre-analyzed files
libGHDLDirectory = sourceDirectory / "lib"
if libGHDLDirectory.exists():
    dataFiles.extend(libGHDLDirectory.rglob("lib*.dll"))
    dataFiles.extend(libGHDLDirectory.rglob("lib*.so"))
    ghdlDirectory = libGHDLDirectory / "ghdl"
    dataFiles.extend(ghdlDirectory.rglob("**/*.vhdl"))
    dataFiles.extend(ghdlDirectory.rglob("**/*.cf"))

    # Extend the Distribution class to force platform specific wheel
    class BinaryDistribution(Distribution):
        def has_ext_modules(self):
            return True

    additional["include_package_data"] = True
    additional["distclass"] = BinaryDistribution

setup(
    **DescribePythonPackageHostedOnGitHub(
        packageName=packageName,
        description="Python binding for GHDL and high-level APIs (incl. LSP).",
        license=GPL_2_0_or_later,
        gitHubNamespace=gitHubNamespace,
        sourceFileWithVersion=packageInformationFile,
        requirementsFile=requirementsFile,
        developmentStatus="beta",
        classifiers=[
            "Operating System :: MacOS",
            "Operating System :: Microsoft :: Windows :: Windows 11",
            "Operating System :: POSIX :: Linux",
            "Intended Audience :: Developers",
            "Topic :: Scientific/Engineering :: Electronic Design Automation (EDA)",
            "Topic :: Software Development :: Code Generators",
            "Topic :: Software Development :: Compilers",
            "Topic :: Software Development :: Testing",
            "Topic :: Utilities",
        ],
        consoleScripts={
            "ghdl-ls":  "pyGHDL.cli.lsp:main",
            "ghdl-dom": "pyGHDL.cli.dom:main"
        },
        dataFiles={
            packageName: [str(file.relative_to(sourceDirectory)) for file in dataFiles]
        }
    ),
    **additional,
)
