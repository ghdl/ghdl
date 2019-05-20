from ctypes import c_int8, c_int32, Structure

class Error_Record(Structure):
    _fields_ = [("origin", c_int8),
                ("id", c_int8),
                ("file", c_int32),
                ("line", c_int32),
                ("offset", c_int32),
                ("length", c_int32)]



class Msgid:
    Msgid_Note = 0
    Warnid_Library = 1
    Warnid_Missing_Xref = 2
    Warnid_Default_Binding = 3
    Warnid_Binding = 4
    Warnid_Port = 5
    Warnid_Reserved_Word = 6
    Warnid_Nested_Comment = 7
    Warnid_Directive = 8
    Warnid_Parenthesis = 9
    Warnid_Vital_Generic = 10
    Warnid_Delayed_Checks = 11
    Warnid_Body = 12
    Warnid_Specs = 13
    Warnid_Universal = 14
    Warnid_Port_Bounds = 15
    Warnid_Runtime_Error = 16
    Warnid_Delta_Cycle = 17
    Warnid_Shared = 18
    Warnid_Hide = 19
    Warnid_Unused = 20
    Warnid_Others = 21
    Warnid_Pure = 22
    Warnid_Static = 23
    Msgid_Warning = 24
    Msgid_Error = 25
    Msgid_Fatal = 26
