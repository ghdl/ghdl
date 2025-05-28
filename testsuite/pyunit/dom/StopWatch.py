# =============================================================================
#               ____ _   _ ____  _          _
#  _ __  _   _ / ___| | | |  _ \| |      __| | ___  _ __ ___
# | '_ \| | | | |  _| |_| | | | | |     / _` |/ _ \| '_ ` _ \
# | |_) | |_| | |_| |  _  | |_| | |___ | (_| | (_) | | | | | |
# | .__/ \__, |\____|_| |_|____/|_____(_)__,_|\___/|_| |_| |_|
# |_|    |___/
# =============================================================================
# Authors:
#   Patrick Lehmann
#
# Testsuite:        Check libghdl IIR translation with a simple package.
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
from time import perf_counter_ns as time_perf_counter
from pathlib import Path
from textwrap import dedent
from unittest import TestCase

from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.formatting.GraphML import (
    DependencyGraphFormatter,
    HierarchyGraphFormatter,
    CompileOrderGraphFormatter,
)  # , ObjectGraphFormatter


if __name__ == "__main__":
    print("ERROR: you called a testcase declaration file as an executable module.")
    print("Use: 'python -m unitest <testcase module>'")
    exit(1)


class Designs(TestCase):
    _root = Path(__file__).resolve().parent.parent
    _encoderSourceDirectory: Path = _root / "dom/examples/Encoder"
    _displaySourceDirectory: Path = _root / "dom/examples/Display"
    _stopwatchSourceDirectory: Path = _root / "dom/examples/StopWatch"

    _utilityPackageFiles = (
        ("lib_Utilities", Path("lib_Utilities/Utilities.pkg.vhdl")),
        ("lib_Utilities", Path("lib_Utilities/Utilities.ctx.vhdl")),
    )
    _utilityCounterFiles = _utilityPackageFiles + (("lib_Utilities", Path("lib_Utilities/Counter.vhdl")),)
    _utilityEntityFiles = _utilityCounterFiles + (
        ("lib_Utilities", Path("lib_Utilities/sync_Bits.vhdl")),
        ("lib_Utilities", Path("lib_Utilities/Debouncer.vhdl")),
    )
    _displayPackageFiles = (
        ("lib_Display", Path("lib_Display/Display.pkg.vhdl")),
        ("lib_Display", Path("lib_Display/Display.ctx.vhdl")),
    )
    _encoderEntityFiles = (("lib_Display", Path("lib_Display/seg7_Encoder.vhdl")),)
    _displayEntityFiles = (
        _displayPackageFiles
        + _encoderEntityFiles
        + (
            ("lib_Display", Path("lib_Display/seg7_Display.vhdl")),
            ("lib_Display", Path("lib_Display/seg7_Display.cfg.vhdl")),
        )
    )
    _stopwatchPackageFiles = (
        ("lib_StopWatch", Path("lib_StopWatch/StopWatch.pkg.vhdl")),
        ("lib_StopWatch", Path("lib_StopWatch/StopWatch.ctx.vhdl")),
    )
    _stopwatchEntityFiles = _stopwatchPackageFiles + (("lib_StopWatch", Path("lib_StopWatch/StopWatch.vhdl")),)
    _encoderFiles = _encoderEntityFiles + (("lib_Pretty", Path("toplevel.Encoder.vhdl")),)
    _displayFiles = _utilityCounterFiles + _displayEntityFiles + (("lib_StopWatch", Path("toplevel.Display.vhdl")),)
    _stopwatchFiles = (
        _utilityEntityFiles
        + _displayEntityFiles
        + _stopwatchEntityFiles
        + (("lib_StopWatch", Path("toplevel.StopWatch.vhdl")),)
    )


class Display(Designs):
    def test_Encoder(self):
        print()

        design = Design()
        for lib, file in self._encoderFiles:
            library = design.GetLibrary(lib)
            document = Document(self._encoderSourceDirectory / file)
            design.AddDocument(document, library)
            print(f"{document.Path}:")
            for warning in document._warnings:
                print(f"  {warning}")

        self.assertEqual(len(self._encoderFiles), len(design.Documents))

    def test_Display(self):
        print()

        design = Design()
        for lib, file in self._displayFiles:
            library = design.GetLibrary(lib)
            print(file)
            document = Document(self._displaySourceDirectory / file)
            design.AddDocument(document, library)
            print(f"{document.Path}:")
            for warning in document._warnings:
                print(f"  {warning}")

        self.assertEqual(len(self._displayFiles), len(design.Documents))

    def test_StopWatch(self):
        print()

        design = Design()
        for lib, file in self._stopwatchFiles:
            library = design.GetLibrary(lib)
            document = Document(self._stopwatchSourceDirectory / file)
            design.AddDocument(document, library)
            print(f"{document.Path}:")
            for warning in document._warnings:
                print(f"  {warning}")

        self.assertEqual(len(self._stopwatchFiles), len(design.Documents))


class CompileOrder(Designs):
    def test_Encoder(self):
        print()

        design = Design()
        design.LoadDefaultLibraries()
        t1 = time_perf_counter()
        for lib, file in self._stopwatchFiles:
            library = design.GetLibrary(lib)
            document = Document(self._stopwatchSourceDirectory / file)
            design.AddDocument(document, library)
            print(dedent(f"""\
                file: {document.Path}
                  libghdl processing time: {document.LibGHDLProcessingTime * 10**6:5.3f} us
                  DOM translation time:    {document.DOMTranslationTime * 10**6:5.3f} us
                """
            ))
            for warning in document._warnings:
                print(f"  {warning}")
        pyGHDLTime = time_perf_counter() - t1

        design.Analyze()

        toplevel = [root.Value.Identifier for root in design.HierarchyGraph.IterateRoots()]

        print(dedent(f"""
            pyGHDL:
              sum:                       {pyGHDLTime * 10**6:5.3f} us
            Analysis:
              default library load time: {design._loadDefaultLibraryTime * 10**6:5.3f} us
              dependency analysis time:  {design._analyzeTime * 10**6:5.3f} us
            Toplevel:                    {", ".join(toplevel)}
            Compile order:\
            """
        ))
        for i, document in enumerate(design.IterateDocumentsInCompileOrder()):
            print(f"  {i:<2}: {document.Path.relative_to(Path.cwd())}")

        graphML = Path("dependencies.graphml")
        dependencyFormatter = DependencyGraphFormatter(design.DependencyGraph)
        dependencyFormatter.WriteGraphML(graphML)

        graphML = Path("hierarchy.graphml")
        hierarchyFormatter = HierarchyGraphFormatter(design.HierarchyGraph)
        hierarchyFormatter.WriteGraphML(graphML)

        graphML = Path("compileorder.graphml")
        compileOrderFormatter = CompileOrderGraphFormatter(design.CompileOrderGraph)
        compileOrderFormatter.WriteGraphML(graphML)

        # graphML = Path("objects.graphml")
        # objectGraphFormatter = ObjectGraphFormatter(design.ObjectGraph)
        # objectGraphFormatter.WriteGraphML(graphML)

        # PP = PrettyPrint()
        # buffer = []
        # buffer.append("Design:")
        # for line in PP.formatDesign(design, 1):
        #     buffer.append(line)
        # print("\n".join(buffer))
