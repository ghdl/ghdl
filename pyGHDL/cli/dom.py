#!/usr/bin/env python3
from argparse import RawDescriptionHelpFormatter
from pathlib import Path
from platform import system as platform_system
from textwrap import wrap, dedent

from pyAttributes import Attribute
from pyMetaClasses import Singleton
from pydecor import export
from pyAttributes.ArgParseAttributes import ArgParseMixin, CommonSwitchArgumentAttribute, DefaultAttribute, CommandAttribute, ArgumentAttribute, \
    SwitchArgumentAttribute
from pyTerminalUI import LineTerminal, Severity

from pyGHDL import GHDLBaseException
from pyGHDL.dom import Misc
from pyGHDL.dom.formatting.prettyprint import PrettyPrint

__author__ =      "Tristan Gingold"
__copyright__ =   "Copyright (C) 2019-2021 Tristan Gingold"
__maintainer__ =  "Tristan Gingold"
__email__ =       ""
__version__ =     "0.0.0"
__status__ =      "Alpha"
__license__ =     ""
__all__ = []
__api__ = __all__

class SourceAttribute(Attribute):
    def __call__(self, func):
        self._AppendAttribute(func,  SwitchArgumentAttribute("-f", "--file", dest="Filename", type=str, help="The filename to parse."))
        self._AppendAttribute(func,  SwitchArgumentAttribute("-F", "--files", dest="Filename", type=str, help="The filename to parse."))
        self._AppendAttribute(func,  SwitchArgumentAttribute("-D", "--directory", dest="Directory", type=str, help="The directory to parse."))
        return func

@export
class Application(LineTerminal, ArgParseMixin):
    HeadLine =    "pyGHDL.dom - Test Application"

    # load platform information (Windows, Linux, Darwin, ...)
    __PLATFORM = platform_system()

    _design: Misc.Design

    def __init__(self, debug=False, verbose=False, quiet=False, sphinx=False):
        super().__init__(verbose, debug, quiet)

        # Initialize the Terminal class
        # --------------------------------------------------------------------------
        Singleton.Register(LineTerminal, self)

        # Initialize DOM with an empty design
        # --------------------------------------------------------------------------
        self._design = Misc.Design()

        # Call the constructor of the ArgParseMixin
        # --------------------------------------------------------------------------
        textWidth = min(self.Width, 160)
        description = dedent("""\
    			Application to test pyGHDL's DOM API.
    			""")
        epilog = "\n".join(wrap(dedent("""\
    		  pyGHDL is a Python binding for libghdl.
    		  """), textWidth, replace_whitespace=False))

        class HelpFormatter(RawDescriptionHelpFormatter):
            def __init__(self, *args, **kwargs):
                kwargs['max_help_position'] = 30
                kwargs['width'] = textWidth
                super().__init__(*args, **kwargs)

        ArgParseMixin.__init__(
            self,
            description=description,
            epilog=epilog,
            formatter_class=HelpFormatter,
            add_help=False
        )

        # If executed in Sphinx to auto-document CLI arguments, exit now
        # --------------------------------------------------------------------------
        if sphinx:
            return

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
        self.WriteNormal(dedent("""\
    		{HEADLINE}{line}
    		{headline: ^80s}
    		{line}""").format(line="=" * 80, headline=self.HeadLine, **LineTerminal.Foreground))

    # ============================================================================
    # Common commands
    # ============================================================================
    # common arguments valid for all commands
    # ----------------------------------------------------------------------------
    @CommonSwitchArgumentAttribute("-d", "--debug", dest="debug", help="Enable debug mode.")
    @CommonSwitchArgumentAttribute("-v", "--verbose", dest="verbose", help="Print out detailed messages.")
    @CommonSwitchArgumentAttribute("-q", "--quiet", dest="quiet", help="Reduce messages to a minimum.")
    def Run(self):
        ArgParseMixin.Run(self)

    @DefaultAttribute()
    def HandleDefault(self, _):
        self.PrintHeadline()
        self.MainParser.print_help()

        self.WriteNormal("")
        self.exit()

    # ----------------------------------------------------------------------------
    # create the sub-parser for the "help" command
    # ----------------------------------------------------------------------------
    @CommandAttribute("help", help="Display help page(s) for the given command name.")
    @ArgumentAttribute(metavar="Command", dest="Command", type=str, nargs="?", help="Print help page(s) for a command.")
    def HandleHelp(self, args):
        self.PrintHeadline()

        if (args.Command is None):
            self.MainParser.print_help()
        elif (args.Command == "help"):
            self.WriteError("This is a recursion ...")
        else:
            try:
                self.SubParsers[args.Command].print_help()
            except KeyError:
                self.WriteError("Command {0} is unknown.".format(args.Command))

        self.WriteNormal("")
        self.exit()

    # ----------------------------------------------------------------------------
    # create the sub-parser for the "version" command
    # ----------------------------------------------------------------------------
    @CommandAttribute("version", help="Display tool and version information.")
    def HandleInfo(self, args):
        self.PrintHeadline()

        copyrights = __copyright__.split("\n", 1)
        self.WriteNormal("Copyright:  {0}".format(copyrights[0]))
        for copyright in copyrights[1:]:
            self.WriteNormal("            {0}".format(copyright))
        self.WriteNormal("License:    {0}".format(__license__))
        authors = __author__.split(", ")
        self.WriteNormal("Authors:    {0}".format(authors[0]))
        for author in authors[1:]:
            self.WriteNormal("            {0}".format(author))
        self.WriteNormal("Version:    {0}".format(__version__))
        self.exit()


    # ----------------------------------------------------------------------------
    # create the sub-parser for the "token-stream" command
    # ----------------------------------------------------------------------------
    @CommandAttribute("pretty", help="Pretty-print the DOM to console.", description="Translate a source file into a DOM and pretty-print the DOM.")
    @SourceAttribute()
    def HandlePretty(self, args):
        self.PrintHeadline()

        file = Path(args.Filename)

        if (not file.exists()):
            print("File '{0!s}' does not exist.".format(file))

        with file.open('r') as fileHandle:
            content = fileHandle.read()


        self.exit()

# main program
def main():  # mccabe:disable=MC0001
    """This is the entry point for pyVHDLParser written as a function.

    1. It extracts common flags from the script's arguments list, before :py:class:`~argparse.ArgumentParser` is fully loaded.
    2. It creates an instance of VHDLParser and hands over to a class based execution.
       All is wrapped in a big ``try..except`` block to catch every unhandled exception.
    3. Shutdown the script and return its exit code.
    """
    from sys import argv as sys_argv

    debug = "-d" in sys_argv
    verbose = "-v" in sys_argv
    quiet = "-q" in sys_argv

    try:
        # handover to a class instance
        app = Application(debug, verbose, quiet)
        app.Run()
        app.exit()
    except GHDLBaseException as ex:
        LineTerminal.printExceptionBase(ex)
    except NotImplementedError as ex:
        LineTerminal.printNotImplementedError(ex)
    # except ImportError as ex:
    #     printImportError(ex)
    except Exception as ex:
        LineTerminal.printException(ex)

# entry point
if __name__ == "__main__":
    LineTerminal.versionCheck((3, 8, 0))
    main()

    # def addFile(self, filename: Path, library: str):
    #     document = Misc.Document(filename)
    #     self._design.Documents.append(document)
    #
    # def prettyPrint(self):
    #     PP = PrettyPrint()
    #
    #     buffer = []
    #
    #     buffer.append("Design:")
    #     for line in PP.formatDesign(self._design, 1):
    #         buffer.append(line)
    #
    #     print("\n".join(buffer))


# def main(items):
#     _exitcode = 0
#
#     if len(items) < 1:
#         print("Please, provide the files to be analyzed as CLI arguments.")
#         print("Using <testsuite/pyunit/SimpleEntity.vhdl> for demo purposes.\n")
#         items = ["testsuite/pyunit/Current.vhdl"]
#
#     for item in items:
#         try:
#             app = Application()
#             app.addFile(Path(item), "default_lib")
#             app.prettyPrint()
#         except GHDLBaseException as ex:
#             print(ex)
#             _exitcode = 1
#
#     return _exitcode
#
#
# if __name__ == "__main__":
#     sysexit(main(argv[1:]))
