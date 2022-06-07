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
    Warnid_Body = 15
    Warnid_Specs = 16
    Warnid_Universal = 17
    Warnid_Port_Bounds = 18
    Warnid_Runtime_Error = 19
    Warnid_Delta_Cycle = 20
    Warnid_No_Wait = 21
    Warnid_Shared = 22
    Warnid_Hide = 23
    Warnid_Unused = 24
    Warnid_Nowrite = 25
    Warnid_Others = 26
    Warnid_Pure = 27
    Warnid_Analyze_Assert = 28
    Warnid_Attribute = 29
    Warnid_Useless = 30
    Warnid_No_Assoc = 31
    Warnid_Static = 32
    Msgid_Warning = 33
    Msgid_Error = 34
    Msgid_Fatal = 35
