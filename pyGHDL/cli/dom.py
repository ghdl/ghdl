#!/usr/bin/env python3
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
#   Unai Martinez-Corral
#
# Package module:   DOM: Interface items (e.g. generic or port)
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
from argparse import RawDescriptionHelpFormatter
from pathlib import Path
from platform import system as platform_system
from textwrap import wrap, dedent

from pyTooling.Decorators import export
from pyTooling.Attributes import Attribute
from pyTooling.Attributes.ArgParse import ArgParseHelperMixin, DefaultHandler, CommandHandler, CommandLineArgument
from pyTooling.Attributes.ArgParse.Argument import StringArgument
from pyTooling.Attributes.ArgParse.Flag import FlagArgument
from pyTooling.TerminalUI import TerminalApplication, Severity

from pyGHDL import GHDLBaseException, __version__, __copyright__, __author__
from pyGHDL.libghdl import LibGHDLException
from pyGHDL.dom import DOMException
from pyGHDL.dom.NonStandard import Design, Document
from pyGHDL.dom.formatting.prettyprint import PrettyPrint, PrettyPrintException

__maintainer__ = "Tristan Gingold"
__status__ = "Alpha"
__license__ = ""


class SourceAttribute(Attribute):
    def __call__(self, func):
        self._AppendAttribute(
            func,
            CommandLineArgument(
                "-f",
                "--file",
                action="append",
                metavar="file",
                dest="Files",
                type=Path,
                help="The filename to parse (can be used multiple times).",
            ),
        )
        self._AppendAttribute(
            func,
            CommandLineArgument(
                "-F",
                "--files",
                metavar="files",
                dest="Files",
                type=Path,
                nargs="+",
                help="List of filenames to parse.",
            ),
        )
        self._AppendAttribute(
            func,
            CommandLineArgument(
                "-D",
                "--directory",
                metavar="dir",
                dest="Directory",
                type=Path,
                help="The directory to parse.",
            ),
        )
        self._AppendAttribute(
            func,
            CommandLineArgument(
                "-L",
                "--library",
                metavar="lib",
                dest="DefaultLibrary",
                type=str,
                help="Default library for files in the root directory.",
            ),
        )
        return func


@export
class Application(TerminalApplication, ArgParseHelperMixin):
    HeadLine = "pyGHDL.dom - Test Application"

    # load platform information (Windows, Linux, Darwin, ...)
    __PLATFORM = platform_system()

    _design: Design

    def __init__(self):
        super().__init__()

        # Initialize DOM with an empty design
        # --------------------------------------------------------------------------
        self._design = Design()

        # Call the constructor of the ArgParseMixin
        # --------------------------------------------------------------------------
        textWidth = min(max(self.Width, 80), 160)
        description = dedent(
            """\
            Application to test pyGHDL's DOM API.
            """
        )
        epilog = "\n".join(
            wrap(
                dedent(
                    """\
                    pyGHDL is a Python binding for libghdl.
                    """
                ),
                textWidth,
                replace_whitespace=False,
            )
        )

        class HelpFormatter(RawDescriptionHelpFormatter):
            def __init__(self, *args, **kwargs):
                kwargs["max_help_position"] = 30
                kwargs["width"] = textWidth
                super().__init__(*args, **kwargs)

        ArgParseHelperMixin.__init__(
            self,
            description=description,
            epilog=epilog,
            formatter_class=HelpFormatter,
            add_help=False,
        )

        # Change error and warning reporting
        # --------------------------------------------------------------------------
        self._LOG_MESSAGE_FORMAT__[Severity.Fatal] = "{DARK_RED}[FATAL] {message}{NOCOLOR}"
        self._LOG_MESSAGE_FORMAT__[Severity.Error] = "{RED}[ERROR] {message}{NOCOLOR}"
        self._LOG_MESSAGE_FORMAT__[Severity.Warning] = "{YELLOW}[WARNING] {message}{NOCOLOR}"
        self._LOG_MESSAGE_FORMAT__[Severity.Normal] = "{GRAY}{message}{NOCOLOR}"

    # class properties
    # ============================================================================
    @property
    def Platform(self):
        return self.__PLATFORM

    def PrintHeadline(self):
        self.WriteNormal(
            dedent(
                """\
                {HEADLINE}{line}
                {headline: ^80s}
                {line}"""
            ).format(line="=" * 80, headline=self.HeadLine, **TerminalApplication.Foreground)
        )

    # ============================================================================
    # Common commands
    # ============================================================================
    # common arguments valid for all commands
    # ----------------------------------------------------------------------------
    @FlagArgument("-d", "--debug", dest="debug", help="Enable debug mode.")
    @FlagArgument("-v", "--verbose", dest="verbose", help="Print out detailed messages.")
    @FlagArgument("-q", "--quiet", dest="quiet", help="Reduce messages to a minimum.")
    def Run(self):
        ArgParseHelperMixin.Run(self)

    @DefaultHandler()
    def HandleDefault(self, _):
        self.PrintHeadline()
        self.MainParser.print_help()

        self.WriteNormal("")
        self.Exit()

    # ----------------------------------------------------------------------------
    # create the sub-parser for the "help" command
    # ----------------------------------------------------------------------------
    @CommandHandler("help", help="Display help page(s) for the given command name.")
    @StringArgument(metaName="Command", dest="Command", optional=True, help="Print help page(s) for a command.")
    def HandleHelp(self, args):
        self.PrintHeadline()

        if args.Command is None:
            self.MainParser.print_help()
        elif args.Command == "help":
            self.WriteError("This is a recursion ...")
        else:
            try:
                self.SubParsers[args.Command].print_help()
            except KeyError:
                self.WriteError(f"Command {args.Command} is unknown.")

        self.WriteNormal("")
        self.Exit()

    # ----------------------------------------------------------------------------
    # create the sub-parser for the "version" command
    # ----------------------------------------------------------------------------
    @CommandHandler("version", help="Display tool and version information.")
    def HandleInfo(self, args):
        self.PrintHeadline()

        copyrights = __copyright__.split("\n", 1)
        self.WriteNormal(f"Copyright:  {copyrights[0]}")
        for copyright in copyrights[1:]:
            self.WriteNormal(f"            {copyright}")
        self.WriteNormal(f"License:    {__license__}")
        authors = __author__.split(", ")
        self.WriteNormal(f"Authors:    {authors[0]}")
        for author in authors[1:]:
            self.WriteNormal(f"            {author}")
        self.WriteNormal(f"Version:    {__version__}")
        self.Exit()

    # ----------------------------------------------------------------------------
    # Create the sub-parser for the "pretty" command
    # ----------------------------------------------------------------------------
    @CommandHandler(
        "pretty",
        help="Pretty-print the DOM to console.",
        description="Translate a source file into a DOM and pretty-print the DOM.",
    )
    @SourceAttribute()
    def HandlePretty(self, args):
        self.PrintHeadline()

        if args.Files is not None:
            for file in args.Files:
                if not file.exists():
                    self.WriteError(f"File '{file!s}' does not exist.")
                    continue

                self.WriteNormal(f"Parsing file '{file!s}'")
                document = self.addFile(file, "pretty")
                self.WriteInfo(
                    dedent(
                        """\
                          libghdl processing time: {: 5.3f} us
                          DOM translation time:    {:5.3f} us
                        """
                    ).format(
                        document.LibGHDLProcessingTime * 10**6,
                        document.DOMTranslationTime * 10**6,
                    )
                )
        elif args.Directory is not None:
            d: Path = args.Directory.resolve()
            if not d.exists():
                self.WriteError(f"Directory '{d!s}' does not exist.")
            elif not d.is_dir():
                self.WriteError(f"Path '{d!s}' is not a directory.")

            if args.DefaultLibrary is None:
                self.WriteWarning(f"Default library is not set.")

            files = []
            for directoryItem in d.iterdir():
                if directoryItem.is_dir():
                    libraryName = directoryItem.name
                    self.WriteNormal(f"Scanning library '{libraryName}' ...")
                    for file in directoryItem.glob("**/*.vhd*"):
                        self.WriteNormal(f"  Reading file '{file!s}'")
                        document = self.addFile(file, libraryName)
                        self.WriteInfo(
                            f"    libghdl processing time: {document.LibGHDLProcessingTime * 10**6: 5.3f} us"
                        )
                        self.WriteInfo(f"    DOM translation time:    {document.DOMTranslationTime * 10**6:5.3f} us")
                elif directoryItem.is_file():
                    if directoryItem.suffix in (".vhd", ".vhdl"):
                        files.append(directoryItem)

            if len(files) > 0 and args.DefaultLibrary is None:
                self.WriteFatal(f"Files in root directory can't ne read, due to missing default library.")
            else:
                libraryName = args.DefaultLibrary
                self.WriteNormal(f"Processing files in root directory for library '{libraryName}' ...")
                for file in files:
                    self.WriteNormal(f"  Reading file '{file!s}'")
                    document = self.addFile(file, libraryName)
                    self.WriteInfo(f"    libghdl processing time: {document.LibGHDLProcessingTime * 10**6: 5.3f} us")
                    self.WriteInfo(f"    DOM translation time:    {document.DOMTranslationTime * 10**6:5.3f} us")

        if not self._design.Documents:
            self.WriteFatal("No files processed at all.")

        self._design.LoadDefaultLibraries()
        self._design.Analyze()
        self.WriteInfo(
            dedent(
                """\
                  default library load time: {:5.3f} us
                  dependency analysis time:  {:5.3f} us
                """
            ).format(
                self._design._loadDefaultLibraryTime * 10**6,
                self._design._analyzeTime * 10**6,
            )
        )

        PP = PrettyPrint()

        buffer = []
        buffer.append("Design:")
        for line in PP.formatDesign(self._design, 1):
            buffer.append(line)

        print("\n".join(buffer))

        self.Exit()

    def addFile(self, filename: Path, library: str) -> Document:
        lib = self._design.GetLibrary(library)

        document = Document(filename)
        self._design.AddDocument(document, lib)
        return document


# main program
def main():  # mccabe:disable=MC0001
    """This is the entry point for pyghdl.cli.dom written as a function.

    1. It extracts common flags from the script's arguments list, before :class:`~argparse.ArgumentParser` is fully loaded.
    2. It creates an instance of DOM test application and hands over to a class based execution.
       All is wrapped in a big ``try..except`` block to catch every unhandled exception.
    3. Shutdown the script and return its exit code.
    """
    from sys import argv as sys_argv

    # debug = "-d" in sys_argv
    # verbose = "-v" in sys_argv
    # quiet = "-q" in sys_argv

    try:
        # handover to a class instance
        app = Application()  # debug, verbose, quiet)
    except Exception:
        return

    try:
        app.CheckPythonVersion((3, 8, 0))
        app.Run()
        app.Exit()
    except PrettyPrintException as ex:
        print(f"PP: {ex!s}")
        app.Exit()

    except DOMException as ex:
        print(f"DOM: {ex!s}")
        ex2: LibGHDLException = ex.__cause__
        if ex2 is not None:
            for message in ex2.InternalErrors:
                print(f"libghdl: {message}")
            TerminalApplication.Exit(0)

        app.Exit(6)

    except LibGHDLException as ex:
        print(f"LIB: {ex!s}")
        for message in ex.InternalErrors:
            print(f"  {message}")
        app.Exit(5)

    except GHDLBaseException as ex:
        app.PrintExceptionBase(ex)

    except NotImplementedError as ex:
        app.PrintNotImplementedError(ex)

    # except ImportError as ex:
    #     printImportError(ex)
    except Exception as ex:
        app.PrintException(ex)


# entry point
if __name__ == "__main__":
    main()
