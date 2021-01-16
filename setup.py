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

from pathlib    import Path
from re         import compile as re_compile
from setuptools import setup as setuptools_setup, find_packages as setuptools_find_packages

gitHubNamespace = "ghdl"
projectName = "ghdl"
packageName = "pyGHDL"
packagePath = Path(packageName)

# Read (local) README for upload to PyPI
readmeFile = packagePath / "README.md"
with readmeFile.open("r") as file:
	long_description = file.read()

# Read requirements file and add them to package dependency list
requirementsFile = packagePath / "requirements.txt"
with requirementsFile.open("r") as file:
	requirements = [line for line in file.readlines()]

def get_version():
	# Try from version.py.  Reads it to avoid loading the shared library.
	pattern = re_compile('^__version__ = "(.*)"\n')
	try:
		line = open("pyGHDL/libghdl/version.py").read()
		match = pattern.match(line)
		if match:
			return match.group(1)
	except:
		pass

	raise Exception("Cannot find version")

# Derive URLs
sourceCodeURL =     "https://github.com/{namespace}/{projectName}".format(namespace=gitHubNamespace, projectName=projectName)
documentationURL =  "https://{namespace}.github.io/{projectName}/using/py/index.html".format(namespace=gitHubNamespace, projectName=projectName)

# Assemble all package information
setuptools_setup(
	name=packageName,
	version=get_version(),

	author="Tristan Gingold",
	author_email="tgingold@free.fr",
	license="GPL-2.0-or-later",
	description="Python binding for GHDL and high-level APIs (incl. LSP).",
	long_description=long_description,
	long_description_content_type="text/markdown",

	url=sourceCodeURL,
	project_urls={
		'Documentation': documentationURL,
		'Source Code':   sourceCodeURL,
		'Issue Tracker': sourceCodeURL + "/issues"
	},

	python_requires='>=3.6',
	install_requires=requirements,
	packages=setuptools_find_packages(exclude=("tests",)),
	entry_points={
		'console_scripts': [
			"ghdl-ls = pyGHDL.cli.lsp:main"
		]
	},

	keywords="Python3 VHDL Parser Compiler Simulator GHDL",
	classifiers=[
		"License :: OSI Approved :: GNU General Public License v2 or later (GPLv2+)",
		"Operating System :: MacOS",
		"Operating System :: Microsoft :: Windows :: Windows 10",
		"Operating System :: POSIX :: Linux",
		"Programming Language :: Python :: 3 :: Only",
		"Programming Language :: Python :: 3.6",
		"Programming Language :: Python :: 3.7",
		"Programming Language :: Python :: 3.8",
		"Programming Language :: Python :: 3.9",
		"Development Status :: 4 - Beta",
#		"Development Status :: 5 - Production/Stable",
		"Intended Audience :: Developers",
		"Topic :: Scientific/Engineering :: Electronic Design Automation (EDA)",
		"Topic :: Software Development :: Code Generators",
		"Topic :: Software Development :: Compilers",
		"Topic :: Software Development :: Testing",
		"Topic :: Utilities",
	]
)
