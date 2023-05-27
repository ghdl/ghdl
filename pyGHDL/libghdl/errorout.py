# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
from enum import IntEnum, unique
from pyTooling.Decorators import export

from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
@BindToLibGHDL("errorout__enable_warning")
def Enable_Warning(Id: int, Enable: bool) -> None:
    """"""


@export
@unique
class Msgid(IntEnum):
    Msgid_Note = 0
    Warnid_Library = 1
    Warnid_Deprecated_Option = 2
    Warnid_Unexpected_Option = 3
    Warnid_Missing_Xref = 4
    Warnid_Default_Binding = 5
    Warnid_Binding = 6
    Warnid_Port = 7
    Warnid_Reserved_Word = 8
    Warnid_Pragma = 9
    Warnid_Nested_Comment = 10
    Warnid_Directive = 11
    Warnid_Parenthesis = 12
    Warnid_Vital_Generic = 13
    Warnid_Delayed_Checks = 14
    Warnid_Synthesis = 15
    Warnid_Body = 16
    Warnid_Specs = 17
    Warnid_Universal = 18
    Warnid_Port_Bounds = 19
    Warnid_Runtime_Error = 20
    Warnid_Delta_Cycle = 21
    Warnid_No_Wait = 22
    Warnid_Shared = 23
    Warnid_Hide = 24
    Warnid_Unused = 25
    Warnid_Nowrite = 26
    Warnid_Others = 27
    Warnid_Pure = 28
    Warnid_Analyze_Assert = 29
    Warnid_Attribute = 30
    Warnid_Useless = 31
    Warnid_No_Assoc = 32
    Warnid_Conformance = 33
    Warnid_Unkept_Attribute = 34
    Warnid_Unhandled_Attribute = 35
    Warnid_Static = 36
    Warnid_Elaboration = 37
    Msgid_Warning = 38
    Msgid_Error = 39
    Msgid_Fatal = 40
