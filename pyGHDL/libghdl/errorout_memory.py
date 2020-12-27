from libghdl import libghdl
from ctypes import c_int8, c_int32, c_char_p, Structure


class Error_Message(Structure):
    _fields_ = [
        ("id", c_int8),
        ("group", c_int8),
        ("file", c_int32),
        ("line", c_int32),
        ("offset", c_int32),
        ("length", c_int32),
    ]


# Values for group:
Msg_Single = 0
Msg_Main = 1
Msg_Related = 2
Msg_Last = 3

Install_Handler = libghdl.errorout__memory__install_handler

Get_Nbr_Messages = libghdl.errorout__memory__get_nbr_messages

Get_Error_Record = libghdl.errorout__memory__get_error_record
Get_Error_Record.argstypes = [c_int32]
Get_Error_Record.restype = Error_Message

Get_Error_Message = libghdl.errorout__memory__get_error_message_addr
Get_Error_Message.argstype = [c_int32]
Get_Error_Message.restype = c_char_p

Clear_Errors = libghdl.errorout__memory__clear_errors
